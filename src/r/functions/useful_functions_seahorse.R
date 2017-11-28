get_nhdf_from_cc = function(cc, DT){
  pi = unique(DT[cell_culture %in% cc, plate_id])
  unique(DT[plate_id %in% pi & Fibroblast_id == "NHDF", cell_culture])
}

get_nhdf_from_fib = function(fib, DT){
  pi = unique(DT[Fibroblast_id %in% fib, plate_id])
  unique(DT[plate_id %in% pi & Fibroblast_id == "NHDF", cell_culture])
}

add_bhi = function(DT, a = 1, b = 1, c = 1, d = 1){
  s <- copy(DT)
  s[, bhi := log( (spare_capacity^a*ATP_production^b) / (non_mito_resp^c*proton_leak^d) )]
  return(s)
}

# In a param_table, remove all rows which contain at least one negative Bioenergetic
remove_neg_params = function(DT){
  # DT[,SD<0] gives a T/F data.table. We keep all rows with all cells = F, therefore rowSums=0
  rt = DT[rowSums(DT[, .SD < 0, .SDcols = BIOENERGETICS]) == 0]  
}

simplify_dt = function(DT){
  s = copy(DT)
  s[, c("minutes", "seconds", "minute", "cell_n_or", "exp_time","PPR","plate_serial","cart_serial","OCR_r","f_id",
        "Person", "Lab", "Transduced", "Antimycin", "Rot_ins", "OCR_p") := NULL]
  s
}
