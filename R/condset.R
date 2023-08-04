condset <- function(object) {
  # The `condition` column is optional; if present and all `NA`, ignore.
  # If the `key_trait` column is present (from `bestcor()`) then also
  # ignore `condition`; this is special case for that routine.
  if(!("key_trait" %in% names(object)) &&
     ("condition" %in% names(object) &&
      !all(is.na(object$condition))))
    c("strain", "sex", "condition")
  else
    c("strain", "sex")
}
