### After finding out that there exists a plate effect, add it to the model

get_tidy_fit = function(FIT){
  tidy_fit = tidy(FIT) %>% as.data.table   # needs package broom
  xlevels = names(FIT$xlevels)
  tidy_fit$levels = ""
  for(i in 1:length(xlevels)){
    tidy_fit$levels[which(grepl(xlevels[i], tidy_fit$term))] = xlevels[i]
  }
  tidy_fit$factor = unlist(strsplit(tidy_fit$term, tidy_fit$levels))[c(F,T)]
  tidy_fit$term = NULL
  setcolorder(tidy_fit, c("levels", "factor", "estimate", "std.error", "statistic", "p.value" ))
}

xc = bio_list$bio_dt[method == "LR_ao"]
xc[, `:=` (nInt1 = lInt1 - log(m_cn*3e4), nInt2 = lInt2 - log(m_cn*3e4), 
           nInt3 = lInt3 - log(m_cn*3e4), nInt4 = lInt4 - log(m_cn*3e4))]


tidy_fit_plate = data.table()
for(param in c("nInt1","nInt2","nInt3","nInt4")){
  fit_plate = lm(get(param) ~ -1 + Fibroblast_id + plate_id, data = xc)
  tf_plate = get_tidy_fit(fit_plate)
  tf_plate = tf_plate[levels == "plate_id", .(factor, estimate)]  
  setnames(tf_plate, "factor", "plate_id")
  tf_plate$Interval = gsub("nInt", "plate_ef", param)
  tidy_fit_plate = rbind(tidy_fit_plate, tf_plate)
}

plate_cast = dcast.data.table(tidy_fit_plate, plate_id ~ Interval, value.var = "estimate")

heatpairs(as.matrix(plate_cast[,.(plate_ef1, plate_ef2, plate_ef3, plate_ef4)]))

xc = left_join(xc, plate_cast, by = "plate_id") %>% as.data.table
xc[, `:=` (aInt1 = nInt1 - plate_ef1, aInt2 = nInt2 - plate_ef2,
           aInt3 = nInt3 - plate_ef3, aInt4 = nInt4 - plate_ef4)]

xdd = xc[Fibroblast_id %in% rh | cell_culture %in% nh ]
sapply(c("aInt1", "aInt2", "aInt3", "aInt4"), 
       function(m) plot_plate_effect_differences(xdd, m, var = "NHDF"))


png(file.path(DIR_pe, "heatscatter_plate_effect.png"), height = 1000, width = 1000, res = 120)
par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
sapply(c("lInt1", "lInt2", "lInt3", "lInt4"), 
       function(m) { plot_plate_effect_differences(xdd, m, var = "s1_s2")
         mtext("θ", outer = T, cex = 1.5)
       })
dev.off()

png(file.path(DIR_pe, "heatscatter_plate_effect_before.png"), height = 1000, width = 1000, res = 120)
par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
sapply(c("nInt1", "nInt2", "nInt3", "nInt4"), 
       function(m) { plot_plate_effect_differences(xdd, m, var = "s1_s2")
       mtext("θ/cell_n", outer = T, cex = 1.5)
})
dev.off()

png(file.path(DIR_pe, "heatscatter_plate_effect_after.png"), height = 1000, width = 1000, res = 120)
par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
sapply(c("aInt1", "aInt2", "aInt3", "aInt4"), 
       function(m){ plot_plate_effect_differences(xdd, m, var = "s1_s2")
       mtext("θ/cell_n - plate_effect", outer = T, cex = 1.5)
})
dev.off()

y = "70490"
xd = na.omit(copy(xc))
pdf(file.path(DIR_pe, "plates.pdf"), width = 10, height = 5)
for(y in remove.element("NHDF",rp)){
  pi = xd[Fibroblast_id == y, plate_id]
  if(length(pi) > 1){
    nh = xd[plate_id %in% pi & Fibroblast_id == "NHDF", cell_culture]
    sub = xd[Fibroblast_id == y | cell_culture %in% nh]
    sub_melt = melt(sub, id.vars = c("cell_culture", "Fibroblast_id", "plate_id"), 
                  measure.vars = c("nInt1","nInt2","nInt3","nInt4","aInt1","aInt2","aInt3","aInt4"))
    sub_melt$exp_date = substr(sub_melt$plate_id, 1, 8)
    for(i in 1:4){
      n = paste0("nInt",i); a = paste0("aInt",i)
      ss = sub_melt[variable %in% c(n,a)]
      q = qplot(plate_id, value, data = ss, col = Fibroblast_id, facets = ~ variable) + 
        geom_point(size = 2.5)
      print(q)
    }
  }
}
dev.off()

## Add coefficient of variation
rt = copy(xc)
setorder(rt, cell_culture)

rt[, `:=` (cv_lInt1 = cv(lInt1), cv_nInt1 = cv(nInt1), cv_dInt1 = cv(dInt1),
           cv_lInt2 = cv(lInt2), cv_nInt2 = cv(nInt2), cv_dInt2 = cv(dInt2),
           cv_lInt3 = cv(lInt3), cv_nInt3 = cv(nInt3), cv_dInt3 = cv(dInt3),
           cv_lInt4 = cv(lInt4), cv_nInt4 = cv(nInt4), cv_dInt4 = cv(dInt4)),
           by = .(Fibroblast_id)]
rt = remove_na(rt)

rt = unique(rt[,.(Fibroblast_id, cv_lInt1, cv_nInt1, cv_dInt1, cv_lInt2, cv_nInt2, cv_dInt2,
                  cv_lInt3, cv_nInt3, cv_dInt3, cv_lInt4, cv_nInt4, cv_dInt4)])
rt = rt[!grep("T|GAL", Fibroblast_id),]
rt[, `:=` (cv_nInt1 = abs(cv_nInt1), cv_dInt1 = abs(cv_dInt1), cv_nInt2 = abs(cv_nInt2), cv_dInt2 = abs(cv_dInt2),
           cv_nInt3 = abs(cv_nInt3), cv_dInt3 = abs(cv_dInt3), cv_nInt4 = abs(cv_nInt4), cv_dInt4 = abs(cv_dInt4))]

boxplot(as.matrix(rt[,.(cv_lInt1, cv_nInt1, cv_dInt1)]))
plot_matrix = as.matrix(rt[,.(cv_lInt1, cv_nInt1, cv_dInt1)])
colnames(plot_matrix) = c("cv_lInt1", "cv_lInt1 / cell_n", "cv_lInt1/cell_n - plate_ef")

png(file.path(DIR_temp, "cv_celln_plateef.png"), width = 800, height = 600)
custom_boxplot(plot_matrix, add_bee = T, main = "Coefficient of variation after cell number and plate effect corrections")
dev.off()


par(mfrow=c(2,2))
custom_boxplot(as.matrix(rt[,.(cv_lInt1, cv_nInt1, cv_dInt1)]), add_bee = T, main = "Int1")
custom_boxplot(as.matrix(rt[,.(cv_lInt2, cv_nInt2, cv_dInt2)]), add_bee = T, main = "Int2")
custom_boxplot(as.matrix(rt[,.(cv_lInt3, cv_nInt3, cv_dInt3)]), add_bee = T, main = "Int3")
custom_boxplot(as.matrix(rt[,.(cv_lInt4, cv_nInt4, cv_dInt4)]), add_bee = T, main = "Int4")
set_par_default()

par(mfrow=c(2,2))
custom_boxplot(as.matrix(rt[,.(cv_lInt1, cv_nInt1)]), add_bee = T, main = "Int1")
custom_boxplot(as.matrix(rt[,.(cv_lInt2, cv_nInt2)]), add_bee = T, main = "Int2")
custom_boxplot(as.matrix(rt[,.(cv_lInt3, cv_nInt3)]), add_bee = T, main = "Int3")
custom_boxplot(as.matrix(rt[,.(cv_lInt4, cv_nInt4)]), add_bee = T, main = "Int4")
set_par_default()


rt_tidy = melt(rt, id.vars="Fibroblast_id", variable.name = "cv", value.name = "value")
rt_plot = rt_tidy[!grep("nInt", cv)]
rt_plot[grep("lInt", cv), p_effect := "before"]
rt_plot[grep("aInt", cv), p_effect := "after"]
rt_plot[, Interval := substr(cv, 5,10)]

g = ggplot(rt_plot, aes(cv, value))
g = g + geom_boxplot(aes(fill = p_effect)) + geom_jitter(height = 0, width = 0.1)
g = add_theme_bw(g, legend_pos ="right")
g = g + scale_fill_manual(values = c("chocolate1", "cornflowerblue"))
add_pval_ggplot(g, pairs = list(c(1,2), c(3,4), c(5,6), c(7,8)), heights = c(0.22, 0.32, 0.22, 0.35), 
                size = 5, pval_text_adj = 0.01, log = F, barheight = 0.01)

m1 = "Seahorse"
m2 = "Offset"
cv_cast = dcast.data.table(cv_tot[method %in% c(m1, m2)], Fibroblast_id ~ method,
                 value.var = list("cv_b_basal","cv_b_ATP","cv_b_proton","cv_b_max_resp","cv_b_spare","cv_b_non_mito"))
cv_tidy = melt(cv_cast, id.vars="Fibroblast_id", variable.name = "cv", value.name = "value")
cv_tidy[grep(m1, cv), p_effect := F]
cv_tidy[grep(m2, cv), p_effect := T]


g = ggplot(cv_tidy, aes(cv, value))
g = g + geom_boxplot(aes(fill = p_effect)) + geom_jitter(height = 0, width = 0.1)
g = add_theme_bw(g, legend_pos ="right")
g = g + scale_fill_manual(values = c("chocolate1", "cornflowerblue"))
add_pval_ggplot(g, pairs = list(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12)), 
                heights = rep(0.8,6), 
                size = 5, pval_text_adj = 0.05, log = F, barheight = 0.01)

