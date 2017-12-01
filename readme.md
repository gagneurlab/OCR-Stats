# OCR-Stats

Contains functions to estimate oxygen consumption rates using the Seahorse XF Analyzer. Includes plotting functions as well.


## Installing requirements
### R

```{r}
install.packages("data.table")
install.packages("magrittr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
```

## General repository organization notes

Run the `example.R` which should give a direct idea of the functions and plots available.

There are 2 folders:

- functions: contains different functions needed for the statistical OCR-stats methods
- plots: contains different plot functions using `ggplot`

R should be started from the repository root.

All data located in `data/`.

## Main functions explained

- `add_outlier_col()`: adds 2 T/F columns (is.outw and is.out) to the given dataset indicating if the OCR value is a well level or single point outlier.
- `compute_bionergetics()`: computes all four OCR interval levels in natural and log scales. Also provides bioenergetics in the natural scale (eg. maximal respiration) and in the log scale (eg. M/Ei ratio)
- `stat_test_OCR()`: compares the bioenergetics of 2 samples providing an estimate with the difference and pvalue.
- `sh_plot()`: plots a whole Seahorse experiment, differentiating samples by color. Can produce points, boxplots or violin plots.
- `outlier_plot()`: plots a single sample, highlighting outlier status.


## Support

Let me know if you have any problems by creating an issue or sending me an email to yepez-at-in.tum.de.
