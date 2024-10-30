#' @importFrom dplyr filter mutate
#' @importFrom tidyr separate_wider_delim unite
#' @importFrom rlang .data
#' 
mutate_datasets <- function(object, datasets = NULL, undo = FALSE) {
  if(is.null(object))
    return(NULL)

  if(undo) {
    for(i in seq_along(datasets)) {
      object <- dplyr::mutate(
        object,
        dataset = ifelse(
          .data$dataset == datasets[[i]],
          names(datasets)[i], .data$dataset))
    }
  } else {
    if(is.null(datasets))
      return(object)
    
    object$dataset <- as.character(object$dataset)
    m <- match(object$dataset, names(datasets), nomatch = 0)
    object$dataset[m>0] <- datasets[m]
    if("probandset" %in% names(object)) {
      object$probandset <- as.character(object$probandset)
      m <- match(object$probandset, names(datasets), nomatch = 0)
      object$probandset[m>0] <- datasets[m]
    }
  }
  object
}
