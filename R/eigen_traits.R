#' Compare Eigen Traits with Original Traits
#'
#' @param object module list object
#' @param sexname name of sex combination
#' @param modulename name of module to examine
#' @param contr_object contrast object from `conditionContrasts()`
#'
#' @return data frame
#' @export
#' @importFrom dplyr bind_rows filter left_join mutate rename select
#' @importFrom stats reorder
#' @importFrom stringr str_remove
#'
eigen_traits <- function(object,
                          sexname = sexnames,
                          modulename,
                          contr_object,
                          eigen_object = eigen_contrast(object, contr_object)) {
  
  sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
  sexes <- names(sexes)[match(sexname, sexes)]
  
  # Get `modules` from `object` and filter to `modulename`. 
  module_object <- 
    dplyr::select(
      dplyr::filter(
        object[[sexname]]$modules,
        paste(sexes, .data$module, sep = "_") == modulename),
      -module)
  
  # No rows if sexname changed.
  if(!nrow(module_object))
    return(NULL)
  
  # Drop unneeded columns from `eigen_object`
  if("size" %in% names(eigen_object))
    eigen_object <- dplyr::select(eigen_object, -size)

  # Join contrast object with module object.
  object <- dplyr::left_join(
    # Filter contrast object to include module traits.
    dplyr::filter(
      # De-select `p.value` from contrast objects.
      contr_object,
      .data$sex == sexname,
      .data$trait %in% module_object$trait),
    module_object,
    by = c("trait"))
  
  # Bind eigen object information.
  object <- 
    # Reorder traits to be in increasing `kME` order.
    dplyr::mutate(
      dplyr::bind_rows(
      object,
      # Filter eigen object to get only `sex` and `module`.
      # Set `kME` to 1 for eigentrait.
      dplyr::select(
        dplyr::mutate(
          # Filter `eigen` object by `sex` and `module`.
          dplyr::filter(
            eigen_object,
            .data$sex == sexname,
            .data$trait == modulename),
          kME = 1),
        -module)),
      p.value = signif(.data$p.value, 4),
      trait = stats::reorder(as.character(.data$trait), -.data$kME))
  
  class(object) <- c("conditionContrasts", class(object))
  attr(object, "conditions") <- attr(contr_object, "conditions")
  attr(object, "termname") <- attr(contr_object, "termname")
  attr(object, "modulename") <- modulename
  attr(object, "ordername") <- "kME"
  object
}
eigen_traits_dataset <- function(object = NULL,
                                 sexname = NULL,
                                 modulename = NULL,
                                 contr_object = NULL,
                                 eigen_object = eigen_contrast(object, contr_object)) {
  if(is.null(object) || is.null(contr_object) || is.null(eigen_object) ||
     is.null(modulename))
    return(NULL)
  
  datasetname <- stringr::str_remove(modulename, ": .*")
  modulename <- stringr::str_remove(modulename, ".*: ")
  if(!(datasetname %in% names(object)))
    return(NULL)
  
  eigen_traits(object[[datasetname]],
               sexname,
               modulename,
               dplyr::filter(contr_object, .data$dataset == datasetname),
               dplyr::filter(eigen_object, .data$dataset == datasetname))
}
