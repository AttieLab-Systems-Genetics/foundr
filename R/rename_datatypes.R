rename_datatypes <- function(object, datatypes) {
  for(i in names(datatypes)) {
    object <- dplyr::mutate(
      object,
      datatype = ifelse(datatype == i, datatypes[[i]], datatype))
  }
  object
}