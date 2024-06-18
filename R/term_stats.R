term_stats <- function(object, signal = TRUE, condition_name = NULL,
                      drop_noise = TRUE, cellmean = signal, ...) {
  terms <- unique(object$term)
  # Drop noise and other terms not of interest to user.
  terms <- terms[!(terms %in% c("rest","rawSD"))]
  if(drop_noise) { 
    terms <- terms[terms != "noise"]
  }
  
  if(is.null(condition_name))
    condition_name <- "condition"
  if(signal) {
    # Return the strain terms with condition if present
    if(any(grepl(condition_name, terms)))
      terms <- c("signal", terms[grepl(paste0(".*strain.*", condition_name), terms)])
    else
      terms <- c("signal", terms[grepl(".*strain", terms)])
  } else {
    terms <- terms[terms != "signal"]
  }
  if(!cellmean) {
    terms <- terms[terms != "cellmean"]
  }
  terms
}
