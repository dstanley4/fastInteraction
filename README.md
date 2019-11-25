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

The code below creates both 2D and 3D graphs and displays regression
results to the console:

``` r
fast.int(data = cohen_exercise,
         criterion = endurance,
         predictor = age,
         moderator = exercise,
         center.predictors = TRUE)
```

Slightly modified code creates both 2D and 3D graphs (with custom axis
labels), displays regression results to the console, and creates a Word
document with the regression tables in APA style.

``` r
library(fastInteraction)

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

    ## 
    ## 
    ## 
    ## 
    ## Regression results using endurance as the criterion
    ##  
    ## 
    ##     Predictor       b        b_95_CI sr2   sr2_95_CI     p             Fit
    ##   (Intercept) 25.89** [24.61, 27.16]                 0.000                
    ##           age -0.26** [-0.39, -0.14] .05  [.00, .11] 0.000                
    ##      exercise  0.97**   [0.70, 1.24] .17  [.08, .25] 0.000                
    ##  age:exercise  0.05**   [0.02, 0.07] .04 [-.00, .08] 0.001                
    ##                                                                R2 = .206**
    ##                                                            95% CI[.12,.28]
    ##                                                                           
    ## 
    ## Note. A significant b-weight indicates the semi-partial correlation is also significant.
    ## b represents unstandardized regression weights. 
    ## sr2 represents the semi-partial correlation squared.
    ## Square brackets are used to enclose the lower and upper limits of a confidence interval.
    ## * indicates p < .05. ** indicates p < .01.
    ##  
    ## 
    ## Regression overall R2: R2 = 0.206, F(3, 241) = 20.86, p = 0.000
    ## 
    ## Simple slope table
    ## ------------------
    ## 
    ##       moderator moderator.value b1.slope   b1.LL   b1.UL b0.intercept
    ##  -1 SD exercise      -4.775e+00 -0.48729 -0.6688 -0.3058        21.24
    ##   Mean exercise      -1.073e-15 -0.26169 -0.3879 -0.1355        25.89
    ##  +1 SD exercise       4.775e+00 -0.03609 -0.2139  0.1417        30.53
    ##    b1.SE       t      p
    ##  0.09214 -5.2889 0.0000
    ##  0.06406 -4.0848 0.0001
    ##  0.09025 -0.3998 0.6896

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

    ## 
    ## 3D graph - see Viewer panel in RStudio
    ## 2D graph - see Plots panel in RStudio

You can also put the output into a custom object and remove key
components:

``` r
myoutput <- fast.int(data = cohen_exercise,
               criterion = endurance,
               predictor = age,
               moderator = exercise,
               center.predictors = TRUE,
               axis.labels = new_axis_labels,
               filename = "tables.doc")
```

Graphing with myoutput

``` r
# Obtain the ggplot graph presented in the Plots tab
formated.ggplot.graph <- myoutput$graph2D.formatted

# Obtain the ggplot graph presented prior to the formatting commands being applied
# If yout want to modify this graph simply add your own ggplot command as illustrated below: 
unformated.ggplot.graph <- myoutput$graph2D.unformatted

unformated.ggplot.graph <- unformated.ggplot.graph +
                              coord_cartesian(ylim = c(0, 60)) +
                              theme_grey(18)
```

Analysis information with myoutput

``` r
# Obtain the regression object presented in tables
regression.lm.object  <- myoutput$regression.lm.object

print(regression.lm.object)
```

    ## 
    ## Call:
    ## lm(formula = endurance ~ age + exercise + age * exercise, na.action = "na.exclude")
    ## 
    ## Coefficients:
    ##  (Intercept)           age      exercise  age:exercise  
    ##     25.88872      -0.26169       0.97272       0.04724

``` r
summary(regression.lm.object)
```

    ## 
    ## Call:
    ## lm(formula = endurance ~ age + exercise + age * exercise, na.action = "na.exclude")
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -21.165  -6.939   0.269   6.300  21.299 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  25.88872    0.64662  40.037  < 2e-16 ***
    ## age          -0.26169    0.06406  -4.085 6.01e-05 ***
    ## exercise      0.97272    0.13653   7.124 1.20e-11 ***
    ## age:exercise  0.04724    0.01359   3.476 0.000604 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9.7 on 241 degrees of freedom
    ## Multiple R-squared:  0.2061, Adjusted R-squared:  0.1962 
    ## F-statistic: 20.86 on 3 and 241 DF,  p-value: 4.764e-12
