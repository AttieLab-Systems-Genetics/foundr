mutate_datatypes <- function(object, datatypes, undo = FALSE) {
  if(is.null(object) | is.null(datatypes))
    return(NULL)

  if(undo) {
    for(i in seq_along(datatypes)) {
      object <- dplyr::mutate(
        object,
        datatype = ifelse(datatype == datatypes[[i]],
                          names(datatypes)[i], datatype))
    }
  } else {
    for(i in seq_along(datatypes)) {
      object <- dplyr::mutate(
        object,
        datatype = ifelse(datatype == names(datatypes)[i],
                          datatypes[[i]], datatype))
      if("probandtype" %in% names(object)) {
        object <- dplyr::mutate(
          object,
          probandtype = ifelse(probandtype == names(datatypes)[i],
                            datatypes[[i]], probandtype))
      }
    }
  }
  object
}
rename_datatypes <- function(object, datatypes, undo = FALSE) {
  if(is.null(object) | is.null(datatypes))
    return(NULL)
  
  if(undo) {
    #shiny::req(input$datatype)
    # object = long names of selected datatypes
    
    # Translate long name from menu selection (object)
    # back to short name
    
    dtypes <- object
    
    # Short name of datatypes = measurement sets
    m <- match(dtypes, datatypes, nomatch = 0)
    dtypes[m>0] <- names(datatypes)[m]
  } else {
    #unique(object$datatype)
    # object = data frame with datatype column of short names
    
    # Get datatypes in object (data frame has short names)
    # Return long name for use in menu or displays
    
    dtypes <- unique(object$datatype)
    # Match up input datatypes with custom settings. Should all match.
    m <- match(dtypes, names(datatypes), nomatch = 0)
    # Replace datatypes with long name for datatype
    dtypes[m>0] <- datatypes[m]
  }
  # Return un-named vector
  as.vector(dtypes)
}


