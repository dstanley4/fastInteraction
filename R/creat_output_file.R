output_rtf_name_overall_lm <- function(column_name) {
  switch(column_name,
         Predictor="Predictor",
         b = "{\\i b}",
         b_95_CI = "{\\i b \\par 95% CI\\par[LL, UL]}",
         sr2 = "{\\i sr\\super 2 \\nosupersub}",
         sr2_95_CI= "{\\i sr\\super 2 \\nosupersub \\par 95% CI\\par[LL, UL]}",
         Fit = "Fit",
         moderator = "Moderator",
         moderator.value = "Moderator Value",
         b1.slope = "{\\i b\\sub 1 \\nosupersub slope}",
         b1.LL = "{\\i b\\sub 1 \\nosupersub \\par LL}",
         b1.UL = "{\\i b\\sub 1 \\nosupersub \\par UL}",
         p = "{\\i p}",
         b0.intercept = "{\\i b\\sub 0 \\nosupersub \\par intercept}",
         b1.SE ="{\\i b\\sub 1 \\nosupersub SE}",
         t ="{\\i t}")

}



output_column_width_overall_lm  <- function(column_name) {
  narrow <- .60
  wide   <- .95
  switch(column_name,
         Predictor = wide*1.5,
         b         = narrow*1.5,
         b_95_CI   = wide*1.5,
         sr2       = narrow*.8,
         sr2_95_CI = wide*1.2,
         p         = narrow,
         Fit       = wide*1.5,
         moderator = wide*1.5,
         moderator.value = wide*1.2,
         b1.slope  = narrow,
         b1.LL    = narrow,
         b1.UL    = narrow,
         b0.intercept = narrow *1.3,
         b1.SE = narrow,
         t    = narrow)
}

get_rtf_column_widths_overall_lm  <- function(df) {
  n <- names(df)
  width_out <- c()
  for (i in 1:length(n)) {
    width_out[i] <-output_column_width_overall_lm(n[i])
  }
  return(width_out)
}


get_rtf_column_names_overall_lm  <- function(df) {
  n <- names(df)
  names_out <- c()
  for (i in 1:length(n)) {
    names_out[i] <-output_rtf_name_overall_lm(n[i])
  }
  return(names_out)
}







write.rtf.table.fastint <- function(filename, table.number=NA, table.title=NA, txt.body, table.note=NA, table.title2=NA, txt.body2, table.note2=NA, landscape=TRUE, paper="us") {
  #generate document format code if needed
  doc.type <- list()
  doc.type$uslandscape <- "\\paperw15840 \\paperh12240 \\margl1440 \\margr1440 \\margt1440 \\margb1440 \\landscape "
  doc.type$usportrait <- ""
  doc.type$a4landscape <- "\\paperw16834 \\paperh11909 \\margl1440 \\margr1440 \\margt1440 \\margb1440 \\landscape "
  doc.type$a4portrait <- ""

  if (!any(paper == c("us","a4"))) {
    paper <- "us"
  }
  if (landscape == TRUE) {
    orientation <- "landscape"
  } else {
    orientation <- "portrait"
  }

  table.number.str <- ""
  if (is.na(table.number)) {
    table.number.str <- "XX"
    table.number.str2 <- "XX"
  } else {
    table.number <- round(table.number)
    table.number.str <- sprintf("%1.0f", table.number)
    table.number2 <- table.number + 1
    table.number.str2 <- sprintf("%1.0f", table.number2)
  }



  #document format
  doc.spec <- paste(paper,orientation,sep="")
  txt.format <- doc.type[[doc.spec]]
  txt.start <- "{\\rtf1\\ansi\\deff0 {\\fonttbl {\\f0 Times New Roman;}}"
  txt.end <- "}"

  #Table X, title, and note
  blank.line <- c("{\\pard  \\par}")
  number.line <-sprintf("{\\pard Table %s \\par}",table.number.str)
  number.line2 <-sprintf("{\\pard Table %s \\par}",table.number.str2)

  if (is.na(table.title)) {
    title.line <- sprintf("{\\pard\\i Table title goes here \\par}")
  } else {
    title.line <- sprintf("{\\pard\\i %s\\par}",table.title)
    title.line2 <- sprintf("{\\pard\\i %s\\par}",table.title2)
  }
  if (is.na(table.note)) {
    note.line <- sprintf("{\\i Table note goes here}")
  } else {
    note.line <- sprintf("{\\pard \\par}{\\pard{\\i Note.} %s\\par}",table.note)
    note.line2 <- sprintf("{\\pard \\par}{\\pard{\\i Note.} %s\\par}",table.note2)
  }

  page.break.code <- "\\page"

  txt.body <- c(number.line , blank.line, title.line , blank.line, txt.body , note.line, page.break.code,
                number.line2, blank.line, title.line2, blank.line, txt.body2, note.line2)


  file.id <- file(filename,"wt")
  writeLines(txt.start,file.id)
  if (landscape==TRUE) {
    writeLines(txt.format,file.id)
  }
  length.body <- length(txt.body)
  for (i in 1:length.body) {
    writeLines(txt.body[i],file.id)
  }
  writeLines(txt.end,file.id)
  close(file.id)
}

