condset <- function(object) {
  if(any(c("condition","sex_condtion") %in% names(object)))
    c("strain", "sex", "condition")
  else
    c("strain", "sex")
}
