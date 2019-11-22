#'  A common problem is creating 3D graphs for regression. This packages solves that problrem.
#'
#'  Bugs and feature requests can be reported at: \url{https://github.com/dstanley4/fastInteraction/issues}
#'
#' A package overview can be obtained using the command: \code{vignette("fastInteraction")}
#'
#'\tabular{ll}{
#'Package: \tab fastInteraction\cr
#'Type: \tab Package\cr
#'Version: \tab 0.1.0\cr
#'Date: \tab 2019-11-08\cr
#'License: \tab MIT\cr
#'}
#'
#'@name fastInteraction
#'@aliases fastInteraction
#'@docType package
#'@title Create 3D graphs
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
utils::globalVariables(c("zv", "xv", "mv", "modvalue", "SE_B", "b1.LL", "b1.UL", "b1.slope", "block_out_rtf", "p", "pred.hi","pred.lo"))
NULL
