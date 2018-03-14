# OCR-Stats

Contains functions to estimate oxygen consumption rates using the Seahorse XF Analyzer, and perform statistical testing between samples. 
Includes plotting functions as well.
Manuscript in [bioRxiv](https://www.biorxiv.org/content/early/2018/03/08/231522).

## Installing requirements
### R

```{r}
install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("magrittr")
install.packages("plotly")
install.packages("tidyr")
```

## General repository organization notes

Sourcing the `config.R` file loads all necessary functions and variables.
Go through the `example.R` which should give a direct idea of the functions and plots available.

There are 2 folders:

- functions: contains different functions needed for the statistical OCR-stats methods
- plots: contains different plot functions using `ggplot`

R should be started from the repository root.

All data located in `data/`.

## Main functions explained

- `add_outlier_col()`: adds 2 T/F columns (is.outw and is.out) to the given dataset indicating if the OCR value is a well level or single point outlier.
- `compute_bionergetics()`: computes all four OCR interval levels in natural and log scales. Also provides bioenergetics in the natural scale (eg. maximal respiration) and in the log scale (eg. M/Ei ratio)
- `stat_test_OCR()`: compares the bioenergetics of 2 samples providing an estimate with the difference and pvalue.
- `sh_plot()`: plots a whole Seahorse experiment, differentiating samples by color. Can produce points, boxplots or violin plots. Returns a ggplot object that can be further edited.
- `outlier_plot()`: plots a single sample, highlighting outlier status. Returns a ggplot object that can be further edited.
- `sh_volcano()`: makes a volcano plot, where the x axis is the bioenergetic difference and the y-axis the -log10 of the pvalue. Samples above the horizontal dotted line are significant.
- `scatterplot_bios()`: makes a scatterplot of 2 different bioenergetic differences.


## Support

Let me know if you have any problems by creating an issue or sending me an email to yepez-at-in.tum.de.
