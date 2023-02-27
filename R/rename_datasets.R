mutate_datasets <- function(object, datasets, undo = FALSE) {
  if(is.null(object) | is.null(datasets))
    return(NULL)

  if(undo) {
    for(i in seq_along(datasets)) {
      object <- dplyr::mutate(
        object,
        dataset = ifelse(dataset == datasets[[i]],
                          names(datasets)[i], dataset))
    }
  } else {
    for(i in seq_along(datasets)) {
      object <- dplyr::mutate(
        object,
        dataset = ifelse(dataset == names(datasets)[i],
                          datasets[[i]], dataset))
      if("probandtype" %in% names(object)) {
        object <- dplyr::mutate(
          object,
          probandtype = ifelse(probandtype == names(datasets)[i],
                            datasets[[i]], probandtype))
      }
    }
  }
  object
}
rename_datasets <- function(object, datasets, undo = FALSE) {
  if(is.null(object) | is.null(datasets))
    return(NULL)
  
  if(undo) {
    #shiny::req(input$dataset)
    # object = long names of selected datasets
    
    # Translate long name from menu selection (object)
    # back to short name
    
    dtypes <- object
    
    # Short name of datasets = measurement sets
    m <- match(dtypes, datasets, nomatch = 0)
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
    dtypes[m>0] <- datasets[m]
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
          datatraits, dataset, trait, sep = sep),
        datatraits %in% traitnames),
      datatraits, sep,
      names = c("dataset", "trait"))    
  } else {
    # object = data frame with dataset and trait columns
    # result = vector of `dataset: trait` names
    tidyr::unite(
      object,
      datatraits,
      dataset, trait,
      sep = sep)$datatraits
  }
}


