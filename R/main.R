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
                          scene = list(xaxis = list(title = predictor.axis.name, range = c(min(x.seq), max(x.seq)), ticktype = "array", tickvals = x.seq),
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
  lm_object.orig <- lm(formula = formula.orig, data = data)

  xmz.data <- data.frame(zv = criterion, xv = predictor, mv = moderator)
  lm_object <- lm(formula = as.formula("zv ~ xv + mv + xv*mv"), data = xmz.data)

  if (center.predictors == TRUE) {
    lm_object <- jtools::summ(lm_object, center = TRUE)$model
    lm_object.orig <- jtools::summ(lm_object.orig, center = TRUE)$model
  }

  axis.labels <- list(criterion = criterion.name,
                      predictor = predictor.name,
                      moderator = moderator.name)

  graph.object <- fast.plot(lm_object = lm_object,
                            criterion = zv,
                            predictor = xv,
                            moderator = mv,
                            center.predictors = FALSE,
                            axis.labels = axis.labels)

  simple.slope.data <- interactions::sim_slopes(lm_object, pred = xv, modx = mv)$slope

  simple.slope.data.rounded <- round(simple.slope.data,2)
  simple.slope.data.rounded$p <- round(simple.slope.data$p,3)

  line.for <- paste(moderator.name, c("(-1 SD)", "(Mean)","(+1 SD)"), sep = " ")
  simple.slope.table <- cbind(line.for, simple.slope.data.rounded)

  linename <- "Moderator Value"
  linevalue  <- "modvalue"
  slopevalue <- "b"
  sevalue <- "SE"
  LLvalue <- "LL"
  ULvalue <- "UL"
  tvalue <- "t"
  pvalue <- "p"
  new.col.names <- c(linename, linevalue, slopevalue, sevalue, LLvalue, ULvalue, tvalue, pvalue)
  names(simple.slope.table) <- new.col.names

  simple.slope.table <- dplyr::select(simple.slope.table, -modvalue)

  print(summary(lm_object.orig))
  print(apaTables::apa.reg.table(lm_object.orig))

  output <- list(simple.slope.table = simple.slope.table,
                 graph = graph.object)



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

