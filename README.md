fastInteraction Version 0.1.0
=============================

Moderated mulitple regression is a common analysis in psychology.
Unfortunately, there are a large number of steps associated with
creating the desired output and code for both 2D and 3D graphs of the
moderated regression.

This package conducts the required analyses and creates both 2D and 3D
graphs of the moderated regression using ggplot and plot.ly,
respectively. Additionally, regression results of the analyses are
displayed in the console, output in list format, and embeded in APA
style tables (as per
[apaTables](https://cran.r-project.org/web/packages/apaTables/vignettes/apaTables.html))
if a filename (ending in .doc or .rtf) is specified.

The development version of fastInteraction hosted here on Github.
Eventually, version will be posted to the CRAN.

### Install Development Version

``` r
install.packages("devtools")

devtools::install_github("dstanley4/fastInteraction")

library(fastInteraction)
```

Tutorial
--------

fastInteraction uses data (i.e., cohen\_data) consistent with Table
7.4.1 from Cohen, Cohen, West, and Aiken (2003) as the demonstration
data set so that values obtained in the output can be checked against
those in the book.

The code below creates both 2D and 3D graphs and display regression
results to the console:

``` r
fast.int(data = cohen_exercise,
         criterion = endurance,
         predictor = age,
         moderator = exercise,
         center.predictors = TRUE)
```

Slightly modified code creates both 2D and 3D graphs (with custom axis
labels), display regression results to the console, and creates a Word
document with the regression tables in APA style.

``` r
new_axis_labels <- list(criterion = "Endurance",
                        predictor = "Age (centered)",
                        moderator = "Exercise (centered)")

fast.int(data = cohen_exercise,
         criterion = endurance,
         predictor = age,
         moderator = exercise,
         center.predictors = TRUE,
         axis.labels = new_axis_labels,
         filename = "tables.doc")
```
