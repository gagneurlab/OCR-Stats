# Source all necessary libraries and functions
source('src/r/config.R')

# Read example data
dt <- fread("data/table_s4_all_OCR.txt")

# Find outliers
dt_ao <- add_outlier_col(dt)

# Compute bioenergetics
bio_dt <- compute_bioenergetics(dt_ao, methods = "LR_ao")

# Create a comparison table 
comp_dt <- create_comp_table(dt_ao)

# Perform statistical testing between samples
bio_list <- stat_test_OCR(bio_dt, comp_dt)
dif_dt <- bio_list$dif_dt
pv_dt <- bio_list$pv_dt

# Doesn't have enough replicates across plates to compute pvalues
pt <- stat_test_OCR(bio_dt, data.table(s1 = c("62343_20140128_1817", "Fibro_VY_083_20140128_1817"), 
                                       s2 = c("NHDF_20140128_1817", "NHDF_20140128_1817")))

#########################
#### Plots examples #####
#########################

## Note that they are ggplot objects, so they can be stored and edited

# 1. Plot all samples inside one plate
pi = unique(dt_ao$plate_id)[10]
sh_plot(dt_ao[plate_id == pi])   # scatterplot
sh_plot(dt_ao[plate_id == pi], geom = "box")   # boxplot


# 2. Plot one sample highlighting non outliers
dt_ao = separate(dt_ao, well, into = c("row", "col"), sep = 1, remove = F)
cc = unique(dt_ao$cell_culture)[15]
outlier_plot(dt_ao, cc)

# 3. Plot the bioenergetics per sample
g1 = plot_bios(dif_dt, pt, bio = "MEi")
ggplotly(g1)

# 4. Statistical Testing volcano plot
g2 = sh_volcano(pv_dt, bio = "MEi")
# Print it using ggplotly and scroll through the dots to see the samples' names
ggplotly(g3)

# 5. Scatterplot 2 different bioenergetics
g3 <- scatterplot_bios(pv_dt, "EI", "MEi")
ggplotly(g3)
