![](https://github.com/dstanley4/fastInteraction/blob/master/vignettes/fastInteraction.gif)

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
[apaTables](https://cran.r-project.org/package=apaTables)) if a filename
(ending in .doc or .rtf) is specified.

The development version of fastInteraction hosted here on Github.
Eventually, a version will be posted to the CRAN.

Currently only interactions between two continous variables is
supported.

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

![Tables](https://github.com/dstanley4/fastInteraction/blob/master/vignettes/tables_apa.gif)

You can also put the output into a custom object and access key
components such different versions of the graphs, the regression object
itself, even the exact data analyzed after centering and drops due to
missing data.

``` r
library(fastInteraction)

new_axis_labels <- list(criterion = "Endurance",
                        predictor = "Age (centered)",
                        moderator = "Exercise (centered)")

myoutput <- fast.int(data = cohen_exercise,
               criterion = endurance,
               predictor = age,
               moderator = exercise,
               center.predictors = TRUE,
               axis.labels = new_axis_labels,
               filename = "tables.doc")
```

### 2D Graphing (as displayed) with myoutput object (created above)

``` r
# Obtain the ggplot graph presented in the Plots tab


formatted.ggplot.graph <- myoutput$graph2D.formatted

ggsave("graph2D.png", formatted.ggplot.graph)
```

![2D
graph](https://github.com/dstanley4/fastInteraction/blob/master/vignettes/graph2D.gif)

### 2D Graphing (custom) with myoutput object (created above)

``` r
# Obtain the ggplot graph presented prior to the formatting commands being applied
# If yout want to modify this graph simply add your own ggplot command as illustrated below: 


unformatted.ggplot.graph <- myoutput$graph2D.unformatted

custom.formatted.ggplot.graph <- unformatted.ggplot.graph +
                                   coord_cartesian(ylim = c(0, 60)) +
                                   theme_grey(18)

ggsave("graph2Dcustom.png", custom.formatted.ggplot.graph)
```

### 3D Graphing with myoutput object (created above)

Saving the 3D graph is a bit tricky. The easiest way is to adjust the
graph until it is in the orientation you like in the Viewer panel of
RStudio and then use the buttons in the Viewer to save it. Note that
with this approach, the larger the graph is on your screen the larger
the dimensions of the final graph will be. Make it very large to obtain
a version of the graph that has a resolution sufficiently high for
publication.

A beter, but more complicated, approach for obtaining a publication
quality graph is to install orca [install link
here](https://github.com/plotly/orca). I suggest using Method 4 Stand
alone binaries because it is a bit easier.

A complication with saving the 3D graph via orca is that you need to set
the orientation of the graph in the code you use to create the graph. I
suggest using the fast.int command several times, each time varing the
camera positions (cam.position), to obtain the desired graph
orientation. Once the desired orientation is obtained, the graph can be
saved with the orca command. You specify the camera position via
cam.position using theta (horizontal rotation angle in degrees -180 to
180, most likely), phi (vertical rotation angle in degrees -90 to 90,
most likely), and distance (from the origin of the graph, start with 3
or 4 and increase this value if needed).

``` r
library(fastInteraction)

new_axis_labels <- list(criterion = "Endurance",
                        predictor = "Age (centered)",
                        moderator = "Exercise (centered)")

cam_position <- list(theta = -20,
                     phi = 20,
                     distance = 2.5)

myoutput <- fast.int(data = cohen_exercise,
               criterion = endurance,
               predictor = age,
               moderator = exercise,
               center.predictors = TRUE,
               axis.labels = new_axis_labels,
               cam.position = cam_position,
               filename = "tables.doc")


graph3D <- myoutput$graph3D

library(plotly)
# This next line requires you install orca software as per link above
# This lines saves 1000 pixel by 1000 pixel version of the graph in PNG format

orca(graph3D, file = "graph3D.png", width = 1000, height = 1000)
```

![3D
graph](https://github.com/dstanley4/fastInteraction/blob/master/vignettes/graph3D.gif)

### Analysis information with myoutput object (created above)

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

### Obtain the exact data included in the regression - after centering and drops due to missing data with myoutput object (created above)

``` r
# Obtain the regression object presented in tables

regression.lm.object  <- myoutput$regression.lm.object

data_analyzed <- regression.lm.object$model

head(data_analyzed)
```

    ##   endurance        age   exercise
    ## 1        18  10.816326 -0.6734698
    ## 2        36  -9.183674 -1.6734698
    ## 3        51 -20.183674 -8.6734698
    ## 4        18  -2.183674 -0.6734698
    ## 5        23  -1.183674 -1.6734698
    ## 6        30  -7.183674 -4.6734698
