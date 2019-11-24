output_rtf_name_overall_lm <- function(column_name) {
  switch(column_name,
         Predictor="Predictor",
         b = "{\\i b\\sub 1 \\nosupersub}",
         b_95_CI = "{\\i b\\sub 1 \\nosupersub \\par 95% CI\\par[LL, UL]}",
         sr2 = "{\\i sr\\super 2 \\nosupersub}",
         sr2_95_CI= "{\\i sr\\super 2 \\nosupersub \\par 95% CI\\par[LL, UL]}",
         Fit = "Fit",
         moderator = "Moderator",
         moderator.value = "Moderator Value",
         b1.slope = "{\\i b\\sub 1 \\nosupersub slope}",
         b1.LL = "{\\i b\\sub 1 \\nosupersub LL}",
         b1.UL = "{\\i b\\sub 1 \\nosupersub LL}",
         p = "{\\i p}",
         b0.intercept = "{\\i b\\sub 0 \\nosupersub intercept}",
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
         moderator = wide*1.2,
         moderator.value = wide*1.5,
         b1.slope  = narrow*1.8,
         b1.LL    = narrow*1.5,
         b1.UL    = narrow*1.5,
         b0.interxept = narrow,
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
