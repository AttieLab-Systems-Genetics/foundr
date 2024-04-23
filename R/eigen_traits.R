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
  if(is.null(object) | is.null(contr_object))
    return(NULL)
  
  # Module name `dataset` must match one of the elements of `object`
  if(!all(stringr::str_remove(modulename, ": .*$") %in% names(object)))
    return(NULL)
  
  if(!is_sex_module(object))
    return(eigen_traits_dataset_value(object, sexname, modulename, contr_object, eigen_object))
  
  eigen_traits_dataset_sex(object, sexname, modulename, contr_object, eigen_object)
}
keptDatatraits <- function(traitModule, dataset) {
  # If MixMod, then get all the kept `dataset: trait` values.
  ds <- dataset
  if(ds == "MixMod") {
    tidyr::unite(
      dplyr::filter(traitModule[[ds]]$value$module, !dropped),
      datatraits, dataset, trait, sep = ": ")$datatraits
  } else {
    NULL
  }
}

eigen_traits_contr_object <- function(object, traitStat, traitSignal,
                                       term_id = "strain:diet") {
  # This generates `contr_object` for `eigen_traits_dataset_value`
  objectTraits <- unite(filter(object$value$modules, !dropped),
                        datatrait, dataset, trait, sep = ": ",
                        remove = FALSE)
  
  # The default contr_object refers only to the datasets.
  # Need to go back to how shinyContrastTable sets up
  # Get traits as below filter from traitSignal
  # Probably need to set up another module.
  
  modStats <- 
    traitStats |>
    unite(datatrait, dataset, trait, sep = ": ", remove = FALSE) |>
    filter(datatrait %in% 
           filter(objectTraits, module %in% modulename)$datatrait) |>
    select(-datatrait)
  modSignal <- 
    traitSignal |>
    unite(datatrait, dataset, trait, sep = ": ", remove = FALSE) |>
    filter(datatrait %in% 
           filter(objectTraits, module %in% modulename)$datatrait) |>
    select(-datatrait)
  
  conditionContrasts(modSignal, modStats, 
                     termname = term_id, rawStats = modStats)
}
eigen_traits_dataset_value <- function(object = NULL,
                                     sexname = NULL,
                                     modulename = NULL,
                                     contr_object = NULL,
                                     eigen_object = eigen_contrast(object, contr_object)) {
  if(is.null(object) | is.null(contr_object))
    return(NULL)
  
  # Can only handle one trait module right now.
  if(length(object) > 1)
    return(NULL)
  
  # The `object` is one traitModule with element `value`.
  object <- object[[1]]
  datasets <- names(object)
  if(!all(datasets %in% "value"))
    return(NULL)

  modulename <- stringr::str_remove(modulename, "^.*: ")
  contr_object <- 
    dplyr::filter(
      dplyr::left_join(
        dplyr::filter(contr_object, sex %in% sexname),
        dplyr::select(object$value$modules, -dropped),
        by = c("dataset", "trait")),
      .data$module %in% modulename)
  # Return contr_object after filtering
  # Could add columns from `object$value$modules`
  dplyr::select(
    dplyr::bind_rows(
      (dplyr::filter(eigen_object, trait == modulename, sex %in% sexname) |>
         dplyr::mutate(trait = "Eigen", module = modulename))[names(contr_object)],
      contr_object),
    -module)
}
eigen_traits_dataset_sex <- function(object = NULL,
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
