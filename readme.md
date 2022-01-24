# OCR-Stats

Contains functions to estimate oxygen consumption rates using the Seahorse XF Analyzer, and perform statistical testing between samples. 
Includes plotting functions as well.
Manuscript in [Plos ONE](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0199938).

## Installing requirements
### R
Install the following R packages using `install.packages()`:
* data.table
* dplyr
* ggplot2
* ggthemes
* magrittr
* plotly
* tidyr

## General repository organization notes

Source the `config.R` file to load all necessary functions and variables.
Go through the `example.R` which should give a direct idea of the functions and plots available.

There are 2 folders:

- functions: contains different functions needed for the statistical OCR-stats methods.
- plots: contains different plot functions. They are all created using `ggplot`, therefore they can be saved and edited.

R should be started from the repository root.

All data located in `data/`.

## Main functions explained

- `add_outlier_col()`: adds 2 T/F columns (is.outw and is.out) to the given dataset indicating if the OCR value is a well level or single point outlier.
- `compute_bionergetics()`: computes all four OCR interval levels in natural and log scales. Also provides bioenergetics in the natural scale (eg. maximal respiration) and in the log scale (eg. M/Ei ratio)
- `stat_test_OCR()`: compares the bioenergetics of 2 samples providing an estimate with the difference and pvalue. Returns a list with 2 objects: dif_dt: for each pair of samples to be compared, gives the bioenergetics of each of them, and the respective difference; pv_dt: for each sample, returns one between-plates replicates aggregated difference wrt to a control and a pvalue.
- `sh_plot()`: plots a whole Seahorse experiment, differentiating samples by color. Can produce points, boxplots or violin plots. Returns a ggplot object that can be further edited.
- `outlier_plot()`: plots a single sample, highlighting outlier status. Returns a ggplot object that can be further edited.
- `plot_bios()`: plots the specified bioenergetics difference wrt to a control of all samples. Marks as red significant samples.
- `sh_volcano()`: makes a volcano plot, where the x axis is the bioenergetic difference wrt to a control and the y-axis the -log10 of the pvalue. Samples above the horizontal dotted line are significant.
- `scatterplot_bios()`: makes a scatterplot of 2 different bioenergetic differences.


## Support

Let me know if you have any problems by creating an issue or sending an email to yepez-at-in.tum.de.
