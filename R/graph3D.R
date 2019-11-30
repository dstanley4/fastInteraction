#' Creates a MMR plot
#' @param lm_object Name of independent variable 1 column in data frame
#' @param criterion Name of independent variable 2 column in data frame
#' @param predictor Name of dependent variable column in data frame
#' @param moderator Project data frame name
#' @param center.predictors test
#' @param axis.labels test
#' @param cam.position A list with theta (degrees), phi (degrees), and distance values. Suggest default distance of 3.
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
fast.plot <- function(lm_object, criterion, predictor, moderator, center.predictors = FALSE, axis.labels = NULL, cam.position = NULL) {

  # add ability to
  # change color of markerss?
  # default plane
  # sim slope output
  # to special apaTables table?

  if (!is.null(cam.position)) {
    cam.theta    <- cam.position$theta
    cam.phi      <- cam.position$phi
    cam.distance <- cam.position$distance

    cam.theta.rad <- degrees.to.radians(cam.theta)
    cam.phi.rad <- degrees.to.radians(cam.phi)
    cam.positon.xyz <- pracma::sph2cart(c(cam.theta.rad, cam.phi.rad, cam.distance))

    x.cam <- cam.positon.xyz[1]
    y.cam <- cam.positon.xyz[2]
    z.cam <- cam.positon.xyz[3]
  }

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
    message("A valid dependent variable (criterion) must be specified.\n\n")
    return(FALSE)
  }

  if (all(is.predictor==TRUE & is.moderator==TRUE)==FALSE) {
    mesasge("Two valid predictor/ variables must be specified (predictor and moderator).\n\n")
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

  if (!is.null(cam.position)) {

  surface.graph <- layout(surface.graph,
                          scene = list(xaxis = list(title = predictor.axis.name,
                                                    range = c(min(x.seq), max(x.seq)),
                                                    ticktype = "array",
                                                    tickvals = x.seq),
                                       yaxis = list(title = moderator.axis.name,
                                                    range = c(min(m.seq), max(m.seq)),
                                                    ticktype = "array",
                                                    tickvals = m.seq),
                                       zaxis = list(title = criterion.axis.name),
                                       camera = list(eye = list(x = x.cam, y = y.cam, z = z.cam),
                                                     center = list(x = 0,
                                                                   y = 0,
                                                                   z = 0)),
                                       aspectmode = "cube",
                                       aspectratio = list(x = 1, y = 1, z = 0.95),
                                       showlegend = TRUE))
  } else {

    surface.graph <- layout(surface.graph,
                            scene = list(xaxis = list(title = predictor.axis.name,
                                                      range = c(min(x.seq), max(x.seq)),
                                                      ticktype = "array",
                                                      tickvals = x.seq),
                                         yaxis = list(title = moderator.axis.name,
                                                      range = c(min(m.seq), max(m.seq)),
                                                      ticktype = "array",
                                                      tickvals = m.seq),
                                         zaxis = list(title = criterion.axis.name),
                                         aspectmode = "cube",
                                         aspectratio = list(x = 1, y = 1, z = 0.95),
                                         showlegend = TRUE))

  }

  return(surface.graph)

}


degrees.to.radians<-function(degrees)
{
  radians<-degrees*pi/180
  return(radians)
}
