#' Eigen Contrasts
#'
#' @param object list object with trait module information
#' @param contr_object data frame of class `conditionContrasts`
#'
#' @return data frame
#' @export
#' @importFrom dplyr arrange bind_rows left_join mutate rename
#' @importFrom purrr map transpose
#' @importFrom tidyr pivot_longer separate_wider_delim
#' @importFrom tibble rownames_to_column
#'
eigen_contrast <- function(object, contr_object) {
  if(is.null(object))
    return(NULL)

  object <- 
    dplyr::mutate(
      dplyr::left_join(
        dplyr::bind_rows(
          # Make data frame of `eigen` matrix.
          purrr::map(purrr::transpose(object)$eigen, eigen_df)),
        # Get information by module.
        module_info(object, contr_object),
        by = c("sex", "module")),
      trait = factor(.data$module, unique(.data$module)),
      module = match(.data$trait, levels(.data$trait)),
      sex = factor(.data$sex, levels(contr_object$sex)))
  
  class(object) <- c("conditionContrasts", class(object))
  attr(object, "conditions") <- attr(contr_object, "conditions")
  attr(object, "termname") <- attr(contr_object, "termname")
  attr(object, "ordername") <- "module"
  object
}
# Apply eigen_contrast over list of `traitModule`s and `contr_object`.
is_sex_module <- function(object) {
  !("value" %in% names(object[[1]]))
}
eigen_contrast_dataset <- function(object, contr_object) {
  if(is.null(object) | is.null(contr_object))
    return(NULL)
  
  if(!is_sex_module(object))
    return(eigen_contrast_dataset_value(object, contr_object))
  
  eigen_contrast_dataset_sex(object, contr_object)
}

eigen_contrast_dataset_value <- function(object, contr_object) {
  # Can only handle one trait module right now.
  if(length(object) > 1)
    return(NULL)
  
  # Should be one module with element `value`.
  object <- object[[1]]
  datasets <- names(object)
  if(!all(datasets %in% "value"))
    return(NULL)
  
  # Get information for each module.
  # Could add information from `contr_object` later.
  objectInfo <-
    dplyr::ungroup(
      dplyr::summarize(
        dplyr::group_by(
          object$value$modules,
          .data$module),
        kME = signif(max(abs(kME), na.rm = TRUE), 4),
        #       p.value = signif(min(p.value, na.rm = TRUE), 4),
        size = dplyr::n(),
        drop = signif(sum(dropped) / size, 4),
        .groups = "drop"))
  
  # Join contrast object of eigenvalues with module information
  dplyr::mutate(
    dplyr::left_join(
      dplyr::rename(contr_object, module = "trait"),
      objectInfo,
      by = "module"),
    trait = factor(.data$module, unique(.data$module)),
    module = match(.data$trait, levels(.data$trait)))
}

eigen_contrast_dataset_sex <- function(object, contr_object) {
  datasets <- names(object)
  if(!all(datasets %in% contr_object$dataset))
    return(NULL)
  # Split contrast object by dataset
  contr_object <- split(contr_object, contr_object$dataset)[datasets]
  # For each `dataset`, construct contrast of eigens.
  # The `contr_object` is only used for module information.
  out <- purrr::map(datasets, function(x) {
    eigen_contrast(object[[x]], contr_object[[x]])
    })
  class(out) <- c("conditionContrasts", class(out))
  c(out)
}
eigen_df <- function(object) {
  # Pivot `eigen` matrix into long data frame.
  # Separate `dataset`, `strain` and `sex` into their own columns.
  
  sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
  
  dplyr::mutate(
    tidyr::separate_wider_delim(
      tidyr::pivot_longer(
        tibble::rownames_to_column(object, "dataset_strain"),
        -dataset_strain,
        names_to = "module", values_to = "value"),
      dataset_strain, delim = "_",
      names = c("dataset", "strain","sex")),
    module = paste(names(sexes)[match(.data$sex, sexes)], module, sep = "_"))
}
module_info <- function(traitModule, traitContrast) {
  sexes <- c("B","F","M","C")
  names(sexes) <- c("Both Sexes", "Female", "Male", "Sex Contrast")
  
  dplyr::mutate(
    dplyr::ungroup(
      dplyr::summarize(
        dplyr::group_by(
          dplyr::left_join(
            dplyr::bind_rows(
              purrr::transpose(traitModule)$modules, .id = "sex"),
            dplyr::distinct(traitContrast, .data$trait, p.value),
            by = "trait"),
          .data$sex, .data$module),
        kME = signif(max(abs(kME), na.rm = TRUE), 4),
        p.value = signif(min(p.value, na.rm = TRUE), 4),
        size = dplyr::n(),
        .groups = "drop")),
    module = paste(sexes[.data$sex], .data$module, sep = "_"))
}
contrast_module <- function(traitContrast, contrastModule) {
  # No need to run twice
  if("module" %in% names(traitContrast))
    return(traitContrast)
  
  sexes <- c("B","F","M","S")
  names(sexes) <- c("Both Sexes", "Female", "Male", "Sex Contrast")
  
  dplyr::arrange(
    dplyr::filter(
      dplyr::left_join(
        traitContrast,
        dplyr::mutate(
          dplyr::bind_rows(
            purrr::transpose(contrastModule)$modules, .id = "sex"),
          sex = factor(.data$sex, levels(traitContrast$sex)),
          module = paste(sexes[.data$sex], .data$module, sep = "_"),
          trait = factor(.data$trait, levels(traitContrast$trait))),
        by = c("sex", "trait")),
      !is.na(.data$module)),
    .data$trait, .data$sex)
}
