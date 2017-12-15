source('src/r/config.R')

# Read example data
dt <- fread("data/table_s4_all_OCR.txt")
setnames(dt, "Cell_line", "Fibroblast_id")

# Find outliers
dt_ao <- add_outlier_col(dt)

# Compute bioenergetics
bio_dt <- compute_bioenergetics(dt_ao, methods = "LR_ao")

# Create a comparison table 
comp_dt <- create_comp_table(dt_ao)

# Perform statistical testing between samples
pt <- stat_test_OCR(bio_dt, comp_dt)

# Doesn't have enough replicates across plates to compute pvalues
pt <- stat_test_OCR(bio_dt, data.table(s1 = c("62343_20140128_1817", "Fibro_VY_083_20140128_1817"), 
                                       s2 = c("NHDF_20140128_1817", "NHDF_20140128_1817")))

#########################
#### Plots examples #####
#########################

# 1. Plot all samples inside one plate
pi = unique(dt_ao$plate_id)[10]
sh_plot(dt_ao[plate_id == pi])   # scatterplot
sh_plot(dt_ao[plate_id == pi], geom = "box")   # boxplot

# 2. Plot one sample highlighting non outliers
dt_ao = separate(dt_ao, well, into = c("row", "col"), sep = 1, remove = F)
cc = unique(dt_ao$cell_culture)[11]
outlier_plot(dt_ao, cc)

