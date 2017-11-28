# Config file for OCR-Stats
# author: vyepez

# Libraries needed
library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyr)

# Different functions needed
source('src/r/functions/compute_bioenergetics.R')
source('src/r/functions/useful_functions_seahorse.R')
source('src/r/functions/useful_functions.R')
source('src/r/functions/add_outlier_col.R')
source('src/r/functions/stat_test_OCR.R')
source('src/r/plots/sh_plot.R')