# OCR-Stats

Contains functions to estimate oxygen consumption rates using the Seahorse XF Analyzer. Includes plotting functions as well.

Method described in: [Paper](https://i12g-gagneurweb.in.tum.de/project/genetic_diagnosis/)


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
- `compute_bionergetics()`: computes all four OCR interval levels in natural and logarithmic scales. Also provides bioenergetics in the natural scale (maximal respiration) and in the logarithmic scale (M/Ei ratio)


## Support

Let me know if you have any problems by creating an issue or sending me an email to yepez-at-in.tum.de.
