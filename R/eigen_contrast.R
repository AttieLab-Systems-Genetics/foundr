#' Eigen Contrasts
#'
#' @param object list object with trait module information
#' @param contr_object data frame of class `conditionContrasts`
#'
#' @return data frame
#' @export
#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr map transpose
#' @importFrom tidyr pivot_longer separate_wider_delim
#' @importFrom tibble rownames_to_column
#'
eigen_contrast <- function(object, contr_object) {
  object <- dplyr::mutate(
    dplyr::bind_rows(
      # Make data frame of `eigen` matrix.
      purrr::map(purrr::transpose(object)$eigen, eigen_df)),
    trait = factor(.data$trait, unique(.data$trait)),
    module = match(.data$trait, levels(.data$trait)),
    sex = factor(sex, levels(contr_object$sex)))
  
  class(object) <- c("conditionContrasts", class(object))
  attr(object, "conditions") <- attr(contr_object, "conditions")
  attr(object, "termname") <- attr(contr_object, "termname")
  attr(object, "ordername") <- "module"
  object
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
        names_to = "trait", values_to = "value"),
      dataset_strain, delim = "_",
      names = c("dataset", "strain","sex")),
    trait = paste(names(sexes)[match(.data$sex, sexes)], trait, sep = "_"))
}
