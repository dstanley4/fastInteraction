#' Creates a MMR plot
#' @param lm_object Name of independent variable 1 column in data frame
#' @param criterion Name of independent variable 2 column in data frame
#' @param predictor Name of dependent variable column in data frame
#' @param moderator Project data frame name
#' @param center.predictors test
#' @param axis.labels test
#' @return plotly object
#' @examples
#' head(grades)
#'
#' lm_object <- lm(exam ~ anxiety + preparation + anxiety*preparation, data = grades)
#'
#' # Example 1: Not mean centered
#' fast.plot(lm_object,
#'          criterion = exam,
#'          predictor = preparation,
#'          moderator = anxiety)
#'
#' # Example 2: Mean centered
#' fast.plot(lm_object,
#'          criterion = exam,
#'          predictor = preparation,
#'          moderator = anxiety,
#'          center.predictors = TRUE)
#'
#' @export
fast.plot <- function(lm_object, criterion, predictor, moderator, center.predictors = FALSE, axis.labels = NULL) {

  # add ability to
  # change color of markerss?
  # default plane
  # sim slope output
  # to special apaTables table?



  if (center.predictors == TRUE) {
    lm_object <- jtools::summ(lm_object, center = TRUE)$model
  }

  data <- lm_object$model
  data.col.names <- names(data)

  predictor.sub <- substitute(predictor)
  is.predictor <- is.valid.name(predictor.sub,data.col.names)
  moderator.sub <- substitute(moderator)
  is.moderator <- is.valid.name(moderator.sub,data.col.names)

  criterion.sub <- substitute(criterion)
  is.criterion  <- is.valid.name(criterion.sub, data.col.names)

  if (is.criterion==FALSE) {
    cat("A valid dependent variable (criterion) must be specified.\n\n")
    return(FALSE)
  }

  if (all(is.predictor==TRUE & is.moderator==TRUE)==FALSE) {
    cat("Two valid predictor/ variables must be specified (predictor and moderator).\n\n")
    return(FALSE)
  }

  predictor.name <- deparse(predictor.sub)
  moderator.name <- deparse(moderator.sub)
  criterion.name <- deparse(criterion.sub)

  predictor.axis.name <- predictor.name
  moderator.axis.name <- moderator.name
  criterion.axis.name <- criterion.name

  if (!is.null(axis.labels)){
    predictor.axis.name <- axis.labels$predictor
    moderator.axis.name <- axis.labels$moderator
    criterion.axis.name <- axis.labels$criterion
  }

  predictor <- data[,predictor.name]
  moderator <- data[,moderator.name]
  criterion  <- data[,criterion.name]

  surface.information <- calculate.surface(lm_object, criterion, predictor, moderator, criterion.name, predictor.name, moderator.name)
  surface.predicted.values <- surface.information$surface.predicted.values
  line1data <- surface.information$line1data
  line2data <- surface.information$line2data
  x.seq <- surface.information$x.seq
  m.seq <- surface.information$m.seq

  x.seq <- round(x.seq, 1)
  m.seq <- round(m.seq, 1)

  surface.graph <- plot_ly()
  surface.graph <- add_surface(surface.graph,
                               x = x.seq,
                               y = m.seq,
                               z = surface.predicted.values,
                               type = "surface",
                               opacity = .90,
                               colors = c('#d1d1d1','#555555'))

  surface.graph <-  add_trace(surface.graph,
                              x = predictor,
                              y = moderator,
                              z = criterion,
                              type = "scatter3d",
                              mode = "markers",
                              color = "black",
                              size = 5,
                              name = "Data points")

  surface.graph <- add_trace(surface.graph,
                             x = line1data$xx,
                             y = line1data$yy,
                             z = line1data$zz,
                             type = "scatter3d",
                             mode = "lines",
                             line = list(color = "black", width = 10, dash = 'dash'),
                             name = "-1 SD line")

  surface.graph <- add_trace(surface.graph,
                             x = line2data$xx,
                             y = line2data$yy,
                             z = line2data$zz,
                             type = "scatter3d",
                             mode = "lines",
                             line = list(color = "black", width = 10, dash = 'dash'),
                             name = "+1 SD line")

  surface.graph <- layout(surface.graph,
                          scene = list(xaxis = list(title = predictor.axis.name,
                                                    range = c(min(x.seq), max(x.seq)),
                                                    ticktype = "array",
                                                    tickvals = x.seq,
                                                    aspectratio=list(x=1, y=1, z=0.95),
                                                    aspectmode = "cube"),
                          yaxis = list(title = moderator.axis.name, range = c(min(m.seq), max(m.seq)), ticktype = "array", tickvals = m.seq),
                          zaxis = list(title = criterion.axis.name),
                          camera = list(eye = list(x = 2, y = -2, z = 1.25), zoom = 5),
                          showlegend = TRUE))

  return(surface.graph)

}




#' Creates a MMR plot and output
#' @param data Name of data frame for interaction
#' @param criterion Name of independent variable 2 column in data frame
#' @param predictor Name of dependent variable column in data frame
#' @param moderator Project data frame name
#' @param axis.labels test
#' @param center.predictors test
#' @examples
#'
#' fast.int(data = grades,
#'          criterion = exam,
#'          predictor = preparation,
#'          moderator = anxiety,
#'          center.predictors = TRUE)
#'
#' @return plotly object
#' @export
fast.int <- function(data, criterion, predictor, moderator, center.predictors = FALSE, axis.labels = NULL) {

  data = as.data.frame(data)

  data.col.names <- names(data)

  predictor.sub <- substitute(predictor)
  moderator.sub <- substitute(moderator)
  criterion.sub <- substitute(criterion)

  is.predictor <- is.valid.name(predictor.sub,data.col.names)
  is.moderator <- is.valid.name(moderator.sub,data.col.names)
  is.criterion  <- is.valid.name(criterion.sub, data.col.names)
  if (is.criterion==FALSE) {
    message("A valid dependent variable (criterion) must be specified.\n\n")
    return(FALSE)
  }
  if (all(is.predictor==TRUE & is.moderator==TRUE)==FALSE) {
    message("Two valid predictor variables must be specified (predictor and moderator).\n\n")
    return(FALSE)
  }

  predictor.name <- deparse(predictor.sub)
  moderator.name <- deparse(moderator.sub)
  criterion.name <- deparse(criterion.sub)

  predictor <- data[,predictor.name]
  moderator <- data[,moderator.name]
  criterion <- data[,criterion.name]


  formula.orig <- get.formula.orig.vars(criterion.name, predictor.name, moderator.name)
  lm_object.orig <- lm(formula = formula.orig, data = data, na.action="na.exclude")

  xmz.data <- data.frame(zv = criterion, xv = predictor, mv = moderator)
  lm_object <- lm(formula = as.formula("zv ~ xv + mv + xv*mv"), data = xmz.data, na.action="na.exclude")

  if (center.predictors == TRUE) {
    lm_object <- jtools::summ(lm_object, center = TRUE)$model
    lm_object.orig <- jtools::summ(lm_object.orig, center = TRUE)$model
  }

  b0.intercept <- as.numeric(lm_object.orig$coefficients["(Intercept)"])
  b.predictor <- as.numeric(lm_object.orig$coefficients[predictor.name])
  b.moderator <- as.numeric(lm_object.orig$coefficients[moderator.name])
  b.interaction <- as.numeric(lm_object.orig$coefficients[paste(predictor.name, moderator.name, sep = ":")])

  predictor.processed <- lm_object.orig$model[predictor.name][,]
  moderator.processed <- lm_object.orig$model[moderator.name][,]


  moderator.value.mean    <- mean(moderator.processed, na.rm = TRUE)
  moderator.value.plusSD  <- moderator.value.mean + sd(moderator.processed, na.rm = TRUE)
  moderator.value.minusSD <- moderator.value.mean - sd(moderator.processed, na.rm = TRUE)
  moderator.values <- c(moderator.value.minusSD, moderator.value.mean, moderator.value.plusSD)
  moderator.labels<-  paste(c("-1 SD", "Mean", "+1 SD"),moderator.name, sep = " ")

  simple.slopes     <- b.predictor + b.interaction * moderator.values
  simple.intercepts <- b0.intercept + b.moderator * moderator.values

  simple.slope.table = data.frame(moderator = moderator.labels,
                                  moderator.values = moderator.values,
                                  b1.slope = simple.slopes,
                                  b0.intercept = simple.intercepts)


  # Calculate simple slope significance
  covmatrix <- vcov(lm_object.orig)[2:4,2:4] # omit intercept from covariance matrix
  SE2B11 <- covmatrix[1,1]
  SE2B22 <- covmatrix[2,2]
  SE2B33 <- covmatrix[3,3]
  COV13 <- covmatrix[1,3]
  COV23 <- covmatrix[2,3]
  Z <- simple.slope.table$moderator.values
  simple.slope.table$SE_B <- sqrt(SE2B11 + 2*Z*COV13 + (Z^2) * SE2B33)
  simple.slope.table$t <- (b.predictor + b.interaction*Z) / simple.slope.table$SE_B
  N <- dim(lm_object.orig$model)[1]
  df <- N - 3 -1
  simple.slope.table$p <- 1-pt(q = abs(simple.slope.table$t), df = df)
  simple.slope.table$p <- round(simple.slope.table$p *2, 4)

  # Calculate CI for simple slopes
  me <- qt(.975, df = df) * simple.slope.table$SE_B
  simple.slope.table$b1.LL <- simple.slope.table$b1.slope - me
  simple.slope.table$b1.UL <- simple.slope.table$b1.slope + me

  # calculate 2D plot values
  predictor.value.mean    <- mean(predictor.processed, na.rm = TRUE)
  predictor.value.minusSD <- predictor.value.mean - sd(predictor.processed, na.rm = TRUE)
  predictor.value.plusSD  <- predictor.value.mean + sd(predictor.processed, na.rm = TRUE)

  simple.slope.table$pred.lo <- simple.slope.table$b0.intercept + simple.slope.table$b1.slope * predictor.value.minusSD
  simple.slope.table$pred.hi <- simple.slope.table$b0.intercept + simple.slope.table$b1.slope * predictor.value.plusSD

  plot.table <- select(simple.slope.table, moderator, pred.lo, pred.hi)
  plot.table <- gather(data = plot.table,
                       key = predictor,
                       value = criterion,
                       pred.lo:pred.hi,
                       factor_key = FALSE)

  plot.table$predictor[plot.table$predictor == "pred.lo"] <-as.character(predictor.value.minusSD)
  plot.table$predictor[plot.table$predictor == "pred.hi"] <-as.character(predictor.value.plusSD)
  plot.table$predictor <- as.numeric(plot.table$predictor)

  crit.min <- mean(lm_object.orig$model[,1], na.rm = TRUE) - sd(lm_object.orig$model[,1], na.rm = TRUE)
  crit.max <- mean(lm_object.orig$model[,1], na.rm = TRUE) + sd(lm_object.orig$model[,1], na.rm = TRUE)

  plot.table$moderator <- fct_relevel(plot.table$moderator,
                                      levels(plot.table$moderator)[plot.table$moderator[3]],
                                      levels(plot.table$moderator)[plot.table$moderator[2]],
                                      levels(plot.table$moderator)[plot.table$moderator[1]])

  graph2D <- ggplot(plot.table, aes(x=predictor,
                                    y = criterion,
                                    group = as.factor(moderator),
                                    linetype = as.factor(moderator))) +
    geom_line(size = 1) +
    coord_cartesian(ylim = c(crit.min, crit.max)) +
    scale_x_continuous(breaks = round(seq(predictor.value.minusSD, predictor.value.plusSD, by = predictor.value.plusSD),2)) +
    theme_classic(14) +
    labs(x = predictor.name, linetype = moderator.name, y = criterion.name)


  #axis.labels <- list(criterion = criterion.name,
  #                    predictor = predictor.name,
  #                    moderator = moderator.name)

  graph3D <- fast.plot(lm_object = lm_object,
                            criterion = zv,
                            predictor = xv,
                            moderator = mv,
                            center.predictors = FALSE,
                            axis.labels = axis.labels)

  reg.sum.table <- summary(lm_object.orig)
  summary.p.values <- sprintf("%1.3f",reg.sum.table$coefficients[,"Pr(>|t|)"])
  summary.p.values <- c(summary.p.values, "","","")


  apa.out <- apaTables::apa.reg.table(lm_object.orig)
  apa.out.tablebody <- apa.out$table_body
  apa.out.tablebody$p <- summary.p.values
  apa.temp <- apa.out.tablebody[,1:5]
  apa.p <- apa.out.tablebody[,7]
  apa.fit <- apa.out.tablebody[,6]
  apa.out.tablebody <- cbind(apa.temp, p = apa.p, Fit = apa.fit)
  apa.out$table_body <- apa.out.tablebody
  #print(apa.out)


  # report overall F here with a sprint statement

  fvalue <- reg.sum.table$fstatistic[1]
  df1 = reg.sum.table$fstatistic[2]
  df2 = reg.sum.table$fstatistic[3]
  pfvalue <- pf(fvalue, df1, df2, lower.tail = FALSE)

  Overall_R2_F <- sprintf("F(%g, %g) = %1.2f, p = %1.3f",
                    df1,
                    df2,
                    fvalue,
                    pfvalue)



  output <- list(apa.table = apa.out,
                 Overall_R2_F = Overall_R2_F,
                 simple.slope.table = simple.slope.table,
                 graph2D = graph2D,
                 graph3D = graph3D)



 return(output)
}

get.formula.orig.vars <- function(criterion.name, predictor.name, moderator.name) {
  measurevar <-  criterion.name
  product.term <- paste(predictor.name, "*", moderator.name)
  groupvars  <- c(predictor.name, moderator.name, product.term)
  paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ ")
  lm.formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  return(lm.formula)
}

