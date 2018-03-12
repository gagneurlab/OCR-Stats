# Config file for OCR-Stats
# author: vyepez

# Libraries needed
library(data.table)
library(magrittr) # Needed for pipe operator %>%
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)     # Needed for tidy data functions like separate

# Different functions needed
source('src/r/functions/compute_bioenergetics.R')
source('src/r/functions/useful_functions_seahorse.R')
source('src/r/functions/useful_functions.R')
source('src/r/functions/add_outlier_col.R')
source('src/r/functions/stat_test_OCR.R')
source('src/r/plots/sh_plot.R')
source('src/r/plots/scatterplot_bios.R')
