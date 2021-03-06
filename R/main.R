#' Creates moderated regression output and graphs (2D/3D)
#' @param data Name of data frame
#' @param criterion Name of criterion variable
#' @param predictor Name of predictor variable
#' @param moderator Name of predictor variable
#' @param axis.labels Optional. Override column names as axis labels by sending in a list of new names.
#' @param center.predictors Boolean. Indicate if predictors hsould be centered. Default is FALSE.
#' @param path Optional. If specified creates a file at the path. Use a filename as "myfile.doc" or "myfile.rtf" for regression tables
#' @param cam.position A list with theta (degrees), phi (degrees), and distance values. Suggest default distance of 3.
#' @references
#' Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). Applied multiple regression. Correlation Analysis for the Behavioral Sciences (3rd Edition). Lawerence Earlbaum Associates: London. ISBN-13: 978-0805822236
#' @return A list with seven objects. Specifically the objects: regression.lm.object (regression for overall analysis), apa.table (a data frame with the results table of the overall analysis), Overall.R2.F (a string with results for overall percxentage of variance), simple.slope.table (a table of the simple slopes), graph2D (a formatted ggplot graph), graph2Dunformatted (an unformatted ggplot graph), and graph3D (a plot.ly surface graph).
#' @examples
#'
#' # Compare results to Table 7.4.1 from Cohen, Cohen, West, and Aiken (2003)
#'
#' results <- fast.int(data = cohen_exercise,
#'                 criterion = endurance,
#'                 predictor = age,
#'                 moderator = exercise,
#'                 center.predictors = TRUE)
#'
#' print(results)
#' @export
fast.int <- function(data, criterion, predictor, moderator, center.predictors = FALSE, axis.labels = NULL, path = NULL, cam.position = NULL) {

  make_file_flag = FALSE
  if (!is.null(path)) {
    make_file_flag = TRUE
  }


  data = as.data.frame(data)

  data.col.names <- names(data)

  predictor.sub <- substitute(predictor)
  moderator.sub <- substitute(moderator)
  criterion.sub <- substitute(criterion)

  is.predictor  <- is.valid.name(predictor.sub, data.col.names)
  is.moderator  <- is.valid.name(moderator.sub, data.col.names)
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

  if (!is.null(axis.labels)) {
    predictor.axis.label <- axis.labels$predictor
    moderator.axis.label <- axis.labels$moderator
    criterion.axis.label <- axis.labels$criterion
  } else {
    predictor.axis.label <- predictor.name
    moderator.axis.label <- moderator.name
    criterion.axis.label <- criterion.name
  }

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
  moderator.value <- c(moderator.value.minusSD, moderator.value.mean, moderator.value.plusSD)
  moderator.labels<-  paste(c("-1 SD", "Mean", "+1 SD"),moderator.name, sep = " ")

  simple.slopes     <- b.predictor + b.interaction * moderator.value
  simple.intercepts <- b0.intercept + b.moderator * moderator.value

  simple.slope.table = data.frame(moderator = moderator.labels,
                                  moderator.value = moderator.value,
                                  b1.slope = simple.slopes,
                                  b0.intercept = simple.intercepts)


  # Calculate simple slope significance
  covmatrix <- vcov(lm_object.orig)[2:4,2:4] # omit intercept from covariance matrix
  SE2B11 <- covmatrix[1,1]
  SE2B22 <- covmatrix[2,2]
  SE2B33 <- covmatrix[3,3]
  COV13 <- covmatrix[1,3]
  COV23 <- covmatrix[2,3]
  Z <- simple.slope.table$moderator.value
  simple.slope.table$b1.SE <- sqrt(SE2B11 + 2*Z*COV13 + (Z^2) * SE2B33)
  simple.slope.table$t <- (b.predictor + b.interaction*Z) / simple.slope.table$b1.SE
  N <- dim(lm_object.orig$model)[1]
  df <- N - 3 -1
  simple.slope.table$p <- 1-pt(q = abs(simple.slope.table$t), df = df)
  simple.slope.table$p <- round(simple.slope.table$p *2, 4)

  # Calculate CI for simple slopes
  me <- qt(.975, df = df) * simple.slope.table$b1.SE
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

  crit.min <- min(lm_object.orig$model[,1], na.rm = TRUE)
  crit.max <- max(lm_object.orig$model[,1], na.rm = TRUE)

  plot.table$moderator <- fct_relevel(plot.table$moderator,
                                      levels(plot.table$moderator)[plot.table$moderator[3]],
                                      levels(plot.table$moderator)[plot.table$moderator[2]],
                                      levels(plot.table$moderator)[plot.table$moderator[1]])

  graph2D.unformatted <- ggplot(plot.table, aes(x=predictor,
                                    y = criterion,
                                    group = as.factor(moderator),
                                    linetype = as.factor(moderator))) +
    geom_line(size = 1)

  graph2D <- graph2D.unformatted +
    coord_cartesian(ylim = c(crit.min, crit.max)) +
    scale_x_continuous(breaks = round(seq(predictor.value.minusSD, predictor.value.plusSD, by = predictor.value.plusSD),2)) +
    theme_classic(14) +
    labs(x = predictor.axis.label, linetype = moderator.axis.label, y = criterion.axis.label)


  axis.labels <- list(criterion = criterion.axis.label,
                      predictor = predictor.axis.label,
                      moderator = moderator.axis.label)

  graph3D <- fast.plot(lm_object = lm_object,
                            criterion = zv,
                            predictor = xv,
                            moderator = mv,
                            center.predictors = FALSE,
                            axis.labels = axis.labels,
                            cam.position = cam.position)

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
  apa.col.names <- names(apa.out.tablebody)
  apa.col.names[3] <- "b_95_CI"
  apa.col.names[5] <- "sr2_95_CI"
  names(apa.out.tablebody) <- apa.col.names
  apa.out$table_body <- apa.out.tablebody
  #print(apa.out)


  # report overall F here with a sprint statement
  R2 <- reg.sum.table$r.squared
  fvalue <- reg.sum.table$fstatistic[1]
  df1 = reg.sum.table$fstatistic[2]
  df2 = reg.sum.table$fstatistic[3]
  pfvalue <- pf(fvalue, df1, df2, lower.tail = FALSE)

  Overall_R2_F <- sprintf("R2 = %1.3f, F(%g, %g) = %1.2f, p = %1.3f",
                    R2,
                    df1,
                    df2,
                    fvalue,
                    pfvalue)


  simple.slope.table.out <- simple.slope.table[,1:9]

  simple.slope.table.out <- dplyr::select(simple.slope.table.out,
                                          moderator,
                                          moderator.value,
                                          b1.slope,
                                          b1.LL,
                                          b1.UL,
                                          b0.intercept,
                                          b1.SE,
                                          t,
                                          p)
  output <- list(regression.lm.object  =lm_object.orig,
                 apa.table = apa.out,
                 Overall.R2.F = Overall_R2_F,
                 simple.slope.table = simple.slope.table.out,
                 graph2D = graph2D,
                 graph2D.unformatted = graph2D.unformatted,
                 graph3D = graph3D)


  #Create RTF code
  if (make_file_flag==TRUE) {

    # First table
    table_title <- sprintf("Moderated regression results using %s as the criterion, %s as the predictor, and %s as the moderator\n",criterion.name, predictor.name, moderator.name)
    colwidths <- get_rtf_column_widths_overall_lm(apa.out$table_body)
    regression_table <- apa.out.tablebody
    names(regression_table) <- get_rtf_column_names_overall_lm(regression_table)
    rtfTable <- apaTables:::RtfTable$new(isHeaderRow=TRUE, defaultDecimalTableProportionInternal=.15)
    rtfTable$setTableContent(as.matrix(regression_table))
    rtfTable$setCellWidthsInches(colwidths)
    rtfTable$setRowSecondColumnDecimalTab(.4)
    txt_body <- rtfTable$getTableAsRTF(FALSE,FALSE)
    table_note <- "A significant {\\i b}-weight indicates the semi-partial correlation is also significant. {\\i b} represents unstandardized regression weights. {\\i sr\\super 2\\nosupersub} represents the semi-partial correlation squared. LL and UL indicate the lower and upper limits of a confidence interval, respectively. {\\i p} indicates the {\\i p}-value. \\par * {\\i p} indicates {\\i p} < .05. ** indicates {\\i p} < .01."



    # Second table
    simple.slope.table.rounded <- simple.slope.table.out
    simple.slope.table.rounded[,2] <- round(simple.slope.table.rounded[,2], 3)
    simple.slope.table.rounded[,3] <- round(simple.slope.table.rounded[,3], 3)
    simple.slope.table.rounded[,4] <- round(simple.slope.table.rounded[,4], 3)
    simple.slope.table.rounded[,5] <- round(simple.slope.table.rounded[,5], 3)
    simple.slope.table.rounded[,6] <- round(simple.slope.table.rounded[,6], 3)
    simple.slope.table.rounded[,7] <- round(simple.slope.table.rounded[,7], 3)
    simple.slope.table.rounded[,8] <- round(simple.slope.table.rounded[,8], 3)


    table_title2 <- sprintf("Simple slope regression results using %s as the criterion, %s as the predictor, and %s as the moderator\n",criterion.name, predictor.name, moderator.name)
    colwidths <- get_rtf_column_widths_overall_lm(simple.slope.table.rounded)
    regression_table <- simple.slope.table.rounded
    names(regression_table) <- get_rtf_column_names_overall_lm(regression_table)
    rtfTable <- apaTables:::RtfTable$new(isHeaderRow=TRUE, defaultDecimalTableProportionInternal=.15)
    rtfTable$setTableContent(as.matrix(regression_table))
    rtfTable$setCellWidthsInches(colwidths)
    rtfTable$setRowSecondColumnDecimalTab(.4)
    txt_body2 <- rtfTable$getTableAsRTF(FALSE,FALSE)
    table_note2 <- "{\\i b\\sub 1\\nosupersub} represents the slope. {\\i b\\sub 0\\nosupersub} represents the intercept. SE represents standard error. LL and UL indicate the lower and upper limits of a confidence interval, respectively. {\\i t} represents the {\\i t}-obtained value. {\\i p} represents the {\\i p}-value. \\par * {\\i p} indicates {\\i p} < .05. ** indicates {\\i p} < .01."


    # Is 3D export function installed...
    # is.xxx.installed <- sum(installed.packages()[,1] == "apaTables")

    write.rtf.table.fastint(filename = path,
                   txt.body = txt_body,
                   table.title = table_title,
                   table.note = table_note,
                   table.number = 1,
                   txt.body2 = txt_body2,
                   table.title2 = table_title2,
                   table.note2 = table_note2,
                   landscape=TRUE)

  }

  class(output) <- "fastintoutput"

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

