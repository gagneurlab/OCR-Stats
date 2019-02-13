# Needs: get_int_levels() and fit_function()
source('src/r/functions/fit_function.R')

compute_bioenergetics = function(DT, methods, Out_co = 5, Out_cop = 7, 
                                 group1 = "cell_culture", group2 = "Fibroblast_id", offset = NULL, y = 'OCR'){
  
  # DT = data; methods = c("LR", "RR", "LR_ao")
  l_fits = l_ints = w_fits = list()
  ints_dt = data.table()
  
  # Compute the well-wise fit and then obtain one value per sample per plate
  for(m in methods){
    l_fits[[m]] = fit_function(DT, Method = m,  Out_co = 5, Out_cop = 7, group1 = group1, group2 = group2, offset = offset, y = y)
    l_ints[[m]] = get_int_levels(l_fits[[m]]$fitted, group1 = group1, group2 = group2)
    ints_dt = rbind(ints_dt, l_ints[[m]])
  }
  
  ints_dt[, `:=` (lInt1 = log(Int1), lInt2 = log(Int2), lInt3 = log(Int3), lInt4 = log(Int4))]
  setorderv(ints_dt, c("plate_id", group1))
  
  bio_dt = ints_dt[, .(plate_id, get(group1), get(group2), method,
                       Int1, Int2, Int3, Int4,
                       lInt1, lInt2, lInt3, lInt4,
                       EI = lInt1 - lInt4,  # Corresponds to basal resp.
                       AI = lInt1 - lInt2,  # Corresponds to ATP prod.
                       EAi = lInt2 - lInt4, # Corresponds to proton leak
                       MI = lInt3 - lInt1,  # Corresponds to spare cap.
                       MEi = lInt3 - lInt4, # Corresponds to maximal resp.
                       basal_resp = Int1 - Int4,
                       ATP_production = Int1 - Int2,
                       proton_leak = Int2 - Int4,
                       max_respiration = Int3 - Int4,
                       spare_capacity = Int3 - Int1,
                       non_mito_resp = Int4)]
  setnames(bio_dt, "V2", group1)
  setnames(bio_dt, "V3", group2)
  
  return(bio_dt)
}


# out_co = 5; out_cop = 6;
# dt_ao = add_outlier_col(dt, Out_co = out_co, Out_cop = out_cop)

# bio_dt = compute_bioenergetics(dt_ao, methods = c("LR", "RR", "LR_ao"))

