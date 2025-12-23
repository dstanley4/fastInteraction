#' Moderated mulitple regression is a common analysis in psychology. Unfortunately, there are a large number of steps associated with creating the desired output and code for both 2D and 3D graphs of the moderated regression. This package conducts the required analyses and creates both 2D and 3D graphs of the moderated regression using ggplot and plot.ly, respectively. Additionally, regression results of the analyses are displayed in the console, output in list format, and embeded in APA style tables#'
#'\tabular{ll}{
#'Package: \tab fastInteraction\cr
#'Type: \tab Package\cr
#'Version: \tab 0.1.2\cr
#'Date: \tab 2019-12-02\cr
#'License: \tab MIT\cr
#'}
#'
#'@name fastInteraction
#'@aliases fastInteraction
#'@docType package
#'@title Continuous variable interaction output and graphs (2D and 3D)
#'@author
#'\tabular{ll}{
#'Author: \tab David J. Stanley \email{dstanley@@uoguelph.ca}\cr
#'Maintainer: \tab David J. Stanley \email{dstanley@@uoguelph.ca}
#'}
#'@importFrom "stats" "sd" "predict" "as.formula" "lm" "pf" "pt" "qt" "vcov"
#'@importFrom "plotly" "add_surface" "add_trace" "layout" "plot_ly"
#'@importFrom "ggplot2" "coord_cartesian" "scale_x_continuous" "labs" "theme_classic" "ggplot" "aes" "geom_line"
#'@importFrom "dplyr" "select"
#'@importFrom "tidyr" "gather"
#'@importFrom "forcats" "fct_relevel"
utils::globalVariables(c("zv", "xv", "mv", "modvalue", "SE_B", "b1.LL", "b1.UL", "b1.slope", "block_out_rtf", "p", "pred.hi","pred.lo","b1.SE"))
NULL
