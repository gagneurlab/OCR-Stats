## Different plots after computing bioenergetics
# author: vyepez


plot_bios <- function(DIF_DT, PT, bio = "MEi"){
  if(! bio %in% DIF_DT[, id])
    stop("Bio does not exist in table.")
  DIF_DT = DIF_DT[id == bio]
  PT = PT[id == bio]
  
  tt = merge(DIF_DT[, .(id, s1, s2, Fibroblast_id, Estimate)], PT[,  .(Fibroblast_id, id, Estimate, pv)],
             by = c("Fibroblast_id", "id"), all.x = T)
  tt[, signif := pv < .05]
  
  # If all pvalues are NAs, plot estimate only
  if(all(is.na(tt$pv))){
    g = ggplot(tt, aes(Estimate, Fibroblast_id, label = s1)) + 
      geom_point() } else{
      g = ggplot(tt, aes(Estimate.x, reorder(Fibroblast_id, pv), label = s1)) + 
        geom_point(aes(col = signif)) + scale_colour_manual(values = c("black", "firebrick"))
    }
  
  g = g + theme_bw(base_size = 14) + geom_vline(xintercept = 1) + 
    scale_x_continuous(trans = 'log2') +
    labs(x = "Estimate", y = "Sample id") + ggtitle(bio)
  
  g
}


sh_volcano = function(PT, bio = "MEi"){
  if(all( is.na(PT[id == bio, pv] )))
    stop("All pvalues are NAs, possibly due to low between plate replicates.")
  
  g = ggplot(PT[id == bio], aes(Estimate, -log10(pv), label = Fibroblast_id)) + geom_point() +
    geom_hline(yintercept = -log10(.05), linetype = "dashed", color = "firebrick") +
    scale_x_continuous(trans = 'log2') +
    theme_bw(base_size = 14) + ggtitle(bio)
  g
}


# Scatterplots different bioenergetics and color codes the significance
scatterplot_bios <- function(PT, bio1, bio2, alpha = .05){
  
  if(! all(c(bio1, bio2) %in% PT$id))
    stop("Either ", bio1, " or ", bio2, " not present in table.")
  
  # Cast the table to have estimates and pvalues in columns
  pt_cast <- dcast(PT, Fibroblast_id ~ id, 
                   value.var = c("Estimate", "pv"))
  
  # Add significant status
  pt_cast[, Signif := "None"]
  pt_cast[get(paste0("pv_", bio1)) < alpha, Signif := bio1]
  pt_cast[get(paste0("pv_", bio2)) < alpha, Signif := bio2]
  pt_cast[get(paste0("pv_", bio1)) < alpha &
            get(paste0("pv_", bio2)) < alpha, Signif := "Both"]
  
  g <- ggplot(pt_cast, aes(get(paste0("Estimate_", bio1)),
                           get(paste0("Estimate_", bio2)), 
                           label = Fibroblast_id)) +
    geom_point(aes(col = Signif)) + 
    theme_bw() + 
    scale_color_manual(values = c("firebrick", "forestgreen", "dodgerblue", "gray")) +
    labs(x = bio1, y = bio2) +  
    geom_hline(yintercept = 1) + geom_vline(xintercept = 1) 
  g
}
