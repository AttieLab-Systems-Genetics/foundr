#' @importFrom dplyr filter mutate
#' @importFrom tidyr separate_wider_delim unite
#' @importFrom rlang .data
#' 
mutate_datasets <- function(object, datasets, undo = FALSE) {
  if(is.null(object) | is.null(datasets))
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
rename_datasets <- function(object, datasets, undo = FALSE) {
  if(is.null(object) | is.null(datasets))
    return(NULL)
  
  if(undo) {
    # object = long names of selected datasets
    
    # Translate long name from menu selection (object)
    # back to short name
    
    dtypes <- object
    
    # Short name of datasets = measurement sets
    m <- match(dtypes,
               paste(names(datasets), datasets, sep = ": "),
               nomatch = 0)
    dtypes[m>0] <- names(datasets)[m]
  } else {
    #unique(object$dataset)
    # object = data frame with dataset column of short names
    
    # Get datasets in object (data frame has short names)
    # Return long name for use in menu or displays
    
    dtypes <- unique(object$dataset)
    # Match up input datasets with custom settings. Should all match.
    m <- match(dtypes, names(datasets), nomatch = 0)
    # Replace datasets with long name for dataset
    dtypes[m>0] <- paste(names(datasets)[m], datasets[m], sep = ": ")
  }
  # Return un-named vector
  as.vector(dtypes)
}
unite_datatraits <- function(object, traitnames, undo = FALSE, sep = ": ") {
  if(undo) {
    # object = data frame to subset based on datatraits = `dataset: trait` names
    # result = data frame with only entries matching datatraits
    tidyr::separate_wider_delim(
      dplyr::filter(
        tidyr::unite(
          object,
          datatraits,
          .data$dataset, .data$trait,
          sep = sep),
        .data$datatraits %in% traitnames),
      .data$datatraits,
      delim = sep,
      names = c("dataset", "trait"))    
  } else {
    # object = data frame with dataset and trait columns
    # result = vector of `dataset: trait` names
    tidyr::unite(
      object,
      datatraits,
      .data$dataset, .data$trait,
      sep = sep)$datatraits
  }
}


