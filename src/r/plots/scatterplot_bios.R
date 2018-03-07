# Scatterplots different bioenergetics and color codes the significance
# author: vyepez

scatterplot_bios <- function(PT, bio1, bio2, alpha = .05){
  
  if(! all(c(bio1, bio2) %in% pt$id))
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
  
  g <- ggplot(pt_cast, aes(exp(get(paste0("Estimate_", bio1))),
                           exp(get(paste0("Estimate_", bio2))), 
                           label = Fibroblast_id)) +
    geom_point(aes(col = Signif)) + 
    theme_bw() + scale_color_ptol() +
    labs(x = bio1, y = bio2) +  
    geom_hline(yintercept = 1) + geom_vline(xintercept = 1) 
  g
}