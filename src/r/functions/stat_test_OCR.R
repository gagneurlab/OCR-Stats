### Input:
# 1. bio_dt with cell_culture, Fibroblast_id and bionergetics such as EI, MEi, etc,
# 2. comp_dt with 2 columns of cell_cultures we are interested in comparing
# 3. character vector containing the bioenergetics to be tested
### Output:
# Table with estimate and pvalue of each comparison (Fibroblast_id vs. Control) 
# for each bionergetic provided in vars


stat_test_OCR <- function(bio_dt, comp_dt, vars = c("EI", "AI", "EAi", "MI", "MEi", "lInt3"),
                          group1 = "cell_culture", group2 = "Fibroblast_id"){
  
  # comp_dt must have 2 columns only
  if(ncol(comp_dt) != 2)
    stop("comp_dt must have 2 columns only")
  
  colnames(comp_dt) <- c("s1", "s2")
  
  
  comp_samples <- union(comp_dt$s1, comp_dt$s2)
  if(!all(comp_samples %in% unique(bio_dt[[group1]]))){
    warning(
      paste0("The following samples appear in the comp_dt, but not in the results table: ", 
             paste(comp_samples[! comp_samples %in% bio_dt[[group1]]], collapse = ", "))
    )}
  
  
  comp_dt <- left_join(comp_dt, unique(bio_dt[, c(group1, group2), with = F]), by = c("s1" = group1))
  
  l = lapply(vars, function(var) {
    ct <- left_join(comp_dt, bio_dt[, c(group1, var), with = F], by = c("s1" = group1))
    setnames(ct, var, "v1")
    ct <- left_join(ct, bio_dt[, c(group1, var), with = F], by = c("s2" = group1)) %>% as.data.table
    setnames(ct, var, "v2")
    ct[, dif := v1 - v2]
  })
  names(l) = vars
  dif_dt = rbindlist(l, idcol = "id")
  dif_dt[, Estimate := exp(dif)]
  
  l2 <- lapply(vars, function(var){
    fit = lm(dif ~ 0 + get(group2), data = dif_dt[id == var])
    
    dof = fit$df.residual
    if(dof == 0){
      message("No plate replicates. It is not possible to compute p values.")
    } else if (dof < 10){
      message(paste0("Only ", dof, " plate replicates. P values computed, but more samples are recommended."))
    } else{
      message(paste0(dof, " plate replicates. Enough to get confident p values."))
    }
    # summary(fit)
    
    coef = summary(fit)$coefficients %>% as.data.table
    coef[, aux := row.names(summary(fit)$coefficients)]
    coef[, aux := gsub("get(group2)", "", aux, fixed = TRUE)]
    setnames(coef, 'aux', group2)
    colnames(coef)[4] = "pv"
    return(coef)
  })
  names(l2) = vars
  pv_dt = rbindlist(l2, idcol = "id")
  
  pv_dt[, Estimate := exp(Estimate)]
  
  return(list(dif_dt = dif_dt, pv_dt = pv_dt))
}


### Input: 
# 1. and 2. Table with all cell cultures to be compared wrt to a control (also input)
### Output:
# Table with 2 columns (s1 and s2) of cell_cultures we are interested in comparing 


create_comp_table <- function(DT, control = c("NHDF")){
  non_controls = unique(DT[! Fibroblast_id %in% control, cell_culture])
  comp_dt <- data.table(s1 = non_controls)
  comp_dt[, s2 := get_nhdf_from_cc(s1, DT), by = 1:nrow(comp_dt)]
  comp_dt[s2 == "NA", s2 := NA]
  comp_dt = remove_na(comp_dt)
  return(comp_dt)
}
