condset <- function(object) {
  if(any(c("condition") %in% names(object)) &&
     !all(is.na(object$condition)))
    c("strain", "sex", "condition")
  else
    c("strain", "sex")
}
