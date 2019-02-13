### Function that fits OCR using linear regression.

fit_function = function(DT = dt, Method = "LR", Out_co = 5, Out_cop = 7,
                        group1 = "cell_culture", group2 = "Fibroblast_id", offset = NULL, y = "OCR"){
  
  DT <- copy(DT)
  
  # Check columns
  necessary_cols <- c("plate_id", group1, group2, "well", "time", y, "Interval")
  if(! all(necessary_cols %in% names(DT)))
    stop(paste0("The following columns have to be in DT: ", paste(necessary_cols, collapse = ", ")))
  
  # Check methods
  if(! Method %in% c("LR", "RR", "LR_ao") )
    stop("Method unavailable. Available methods: LR, RR, LR_ao")
  
  # Check y
  if(! y %in% c("OCR", "ECAR", "lOCR", "lECAR", "OCR_n") )
    stop("Response variable unavailable. Available y: OCR, ECAR")
  
  if(y %in% c("OCR", "lOCR"))
    if(is.null(DT$lOCR))  DT[, lOCR := log(OCR)]
  
  if(y %in% c("ECAR", "lECAR"))
    if(is.null(DT$lECAR))  DT[, lECAR := log(ECAR)]
  
  if(Method == "LR_ao") DT = DT[is.out == F]
  
  var = ifelse(y %in% c("OCR", "lOCR"),
               "lOCR", ifelse(y %in% c("ECAR", "lECAR"), "lECAR", "lOCR_n"))
  setnames(DT, var, "x")
  # if(y %in% c("OCR", "lOCR")){
  #   setnames(DT, "lOCR", "x")
  #  } else if (y %in% c("ECAR", "lECAR"))
  #    setnames(DT, "lECAR", "x")
  
  coef_res = NULL  # contains the coefficients and pvalues
  DF = NULL    # contains the fitted values
  
  for(cc in remove_na( unique(DT[[group1]])) ){
    
    # Subset for each cc
    df = DT[get(group1) == cc, c(group1, group2, "x", "Interval", "well", "time", offset), with = F]
    df = na.omit(df)
    
    if(nrow(df) > 2 ){
      if(Method %in% c("LR", "LR_ao")){   # Linear Regression
        if(is.null(offset)){
          if(length(unique(df$well)) == 1){# There may be only 1 well (replicate)
            fit = lm(x ~ -1 + Interval, data = df) } else
              fit = lm(x ~ -1 + Interval + well, data = df)
        }else{
          if(length(unique(df$well)) == 1){
            fit = lm(x - get(offset) ~ -1 + Interval, data = df) } else
              fit = lm(x - get(offset) ~ -1 + Interval + well, data = df)
        }
        
        s = summary(fit)$coefficients
        
      }else if(Method == "RR"){    # Robust (abs value) Regression
        if(length(unique(df$well)) == 1){
          fit = rq(x ~ -1 + Interval, data = df) } else
            fit = rq(x ~ -1 + Interval + well, data = df)
        
        s = summary.rq(fit, se = "iid")$coefficients
      }
      
      df$fitted = fitted(fit)   # Add fitted values to the data table
      df$residuals = residuals(fit) # Add residual values to the data table
      
      colnames(s)[4] = "pvalue"
      sdt = as.data.table(s)
      sdt$Parameter = row.names(s)
      sdt$cell_culture = cc
      sdt[[group2]] = unique(df[[group2]])
    }
    
    coef_res = rbind(sdt, coef_res, fill = T)   # contains the coefficients and pvalues
    DF = rbind(df, DF, fill = T) # contains the fitted values and residuals
  } # closes the for loop
  
  coef_res$method = Method
  DF$method = Method
  DF[, e_fitted := exp(fitted)]
  DF[, lInt_fit := mean(fitted, na.rm=T), by = .(Interval, get(group1))]
  DF[, Int_fit := exp(lInt_fit)]
  DF[, sqE := (x - lInt_fit)^2]
  DF[, mean_sqE := mean(sqE, na.rm=T), by = .(well, get(group1))]
  DF[, median_mean_sqE := median(mean_sqE, na.rm=T), by = get(group1)]
  DF[, mad_mean_sqE := mad(mean_sqE, na.rm=T), by = get(group1)]
  DF[, is.outw := median_mean_sqE + Out_co * mad_mean_sqE < mean_sqE]
  DF[, median_sqE := median(sqE, na.rm=T), by = .(get(group1), Interval)]
  DF[, mad_sqE := mad(sqE, na.rm=T), by = .(get(group1), Interval)]
  DF[, is.out := median_sqE + Out_cop * mad_sqE < sqE]
  DF[, sd_res := sd(x - lInt_fit)]
  setorderv(DF, c(group1, "well"))
  
  # Add plate_id
  DF = right_join(unique(DT[, c("plate_id", group1), with = F], by=NULL), DF, by = group1) %>% as.data.table
  
  l = list(coefficients = coef_res, fitted = DF)
  return(l)
}
 
# From the fit, we get all the coefficients (intercept, interval, well, time, etc), but we only want the 4 Intervals
get_int_levels = function(DT_fitted, group1 = "cell_culture", group2 = "Fibroblast_id"){
  ## Subset to desired columns (mind sd_res)
  xc = DT_fitted[, c("plate_id", group1, group2, "method", "Interval", "Int_fit", "sd_res"), with = F] %>% unique
  x_cast = dcast.data.table(xc, ... ~ Interval, value.var = "Int_fit")
  setorderv(x_cast, c("plate_id", group1))
  x_cast
}


get_max_resp = function(DT = dt, Method = "LR", Out_co = 5, Out_cop = 7, 
                        group1 = "cell_culture", group2 = "Fibroblast_id", offset = NULL){
  fit = fit_function(DT, Method, Out_co, Out_cop, group1, group2, offset)
  int_levels = get_int_levels(fit$fitted, group1, group2)
  int_levels[, max_resp := Int3 - Int4]
  int_levels
}


### Add the columns: theta, res and res_st to a given DT
add_theta = function(DT, Method = "LR", Out_co = 5, Out_cop = 7, 
          group1 = "cell_culture", group2 = "Fibroblast_id", offset = NULL){
  
  fit = fit_function(DT, Method, Out_co, Out_cop, group1, group2, offset)
  int_levels = get_int_levels(fit$fitted, group1, group2)
  melt_lint = melt(int_levels[method == Method, c(group1, "Int1", "Int2", "Int3", "Int4"), with = F], id.vars = group1, 
                 variable.name = "Interval", value.name = "theta")
  melt_lint[, theta := log(theta)]

  top_m = left_join(DT, melt_lint, by = c("Interval", group1)) %>% as.data.table
  top_m[, res := lOCR - theta]
  setorderv(top_m, c(group2, "well", "time"))
  top_m[, sd_res := sd(res), by = .(Interval, get(group2))]
  top_m[, res_st := res/sd_res]
  top_m
}

### Compute coefficient of variation between
add_cv_b = function(DT){
  rt = copy(DT)
  rt = remove_neg_params(rt)
  rt[, `:=` (cv_b_basal = cv(basal_resp),
             cv_b_ATP = cv(ATP_production),
             cv_b_proton = cv(proton_leak),
             cv_b_max_resp = cv(max_respiration),
             cv_b_spare = cv(spare_capacity),
             cv_b_non_mito = cv(non_mito_resp)), by = .(Fibroblast_id, method)]
  rt$neg_param = NULL
  rt = unique(rt[,.(Fibroblast_id, method, cv_b_basal, cv_b_ATP, cv_b_proton, cv_b_max_resp, cv_b_spare, cv_b_non_mito)])
  rt
}