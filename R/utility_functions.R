calculate.surface <- function(lm_object, criterion, predictor, moderator, criterion.name, predictor.name, moderator.name) {

  mx <- mean(predictor, na.rm = TRUE)
  mm <- mean(moderator, na.rm = TRUE)
  sdx <- sd(predictor, na.rm = TRUE)
  sdm <- sd(moderator, na.rm = TRUE)

  x.range <- c( (mx - 2*sdx), (mx + 2*sdx))
  m.range <- c( (mm - 2*sdm), (mm + 2*sdm))
  x.seq <- seq((mx - 2*sdx), (mx + 2*sdx), by = sdx)
  m.seq <- seq((mm - 2*sdm), (mm + 2*sdm), by = sdm)

  length.x.seq <- length(x.seq)
  length.m.seq <- length(x.seq)


  new.data <- as.data.frame(expand.grid(x.seq, m.seq))
  num_cases <- length(new.data[,1])
  criterion.temp <- data.frame(criterion.temp = rep(NA, num_cases))
  new.data <- cbind(criterion.temp,new.data)
  names(new.data) <- c(criterion.name, predictor.name, moderator.name)
  new.data[,1] = predict(object = lm_object, newdata = new.data)

  surface.predicted.values <- matrix(rep(NA, length.x.seq*length.m.seq), length.x.seq, length.m.seq)
  rownames(surface.predicted.values) <- round(x.seq,2)
  colnames(surface.predicted.values) <- round(m.seq,2)

  cur_row <- 0
  for (x in 1:length(x.seq)) {
    for (m in 1:length(m.seq)) {
      cur_row <- cur_row + 1
      surface.predicted.values[m, x] <- new.data[cur_row,1]
    }
  }

  line_data_sdym1 <- as.numeric(surface.predicted.values[2,])
  line_data_sdyp1 <- as.numeric(surface.predicted.values[4,])

  line1data <- data.frame(xx = x.seq, yy = rep((mm-sdm), 5), zz = line_data_sdym1)
  line2data <- data.frame(xx = x.seq, yy = rep((mm+sdm), 5), zz = line_data_sdyp1)

  output <- list(surface.predicted.values = surface.predicted.values,
                 line1data = line1data,
                 line2data = line2data,
                 x.seq = x.seq,
                 m.seq = m.seq)

  return(output)
}



is.valid.name <- function(sub.name, data.col.names) {
  is.name.valid <- FALSE
  if (!is.null(sub.name)) {
    is.name.valid <- any(sub.name == data.col.names)
    if (is.name.valid==FALSE){
      cat(sprintf("%s is not a valid column name.\n\n",as.character(sub.name)))
    }
  }
  return(is.name.valid)
}
