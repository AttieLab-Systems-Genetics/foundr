#' Prepare biplot data
#'
#' @param dat 
#' @param traits 
#' @param factors 
#'
#' @return data frame
#' 
#' @importFrom dplyr across filter mutate
#' @importFrom tidyr pivot_wider
#' @export
#'
#' @examples
biplot_data <- function(dat, traits, factors = c("strain", "animal", "sex", "condition")) {
  dplyr::mutate(
    tidyr::pivot_wider(
      dplyr::filter(
        dat,
        trait %in% traits),
      factors,
      names_from = "trait", values_from = "value"),
    dplyr::across(where(is.numeric), function(x) {
      m <- mean(x, na.rm = TRUE)
      x[is.na(x)] <- m
      x
    }))
}

#' Get PCA components from biplot data
#'
#' @param bip 
#' @param factors 
#' @param strain 
#'
#' @return data frame of PCA components
#' 
#' @importFrom ordr as_tbl_ord confer_inertia mutate_cols mutate_rows
#' @importFrom stats princomp
#' @importFrom dplyr select
#' @export
#'
biplot_pca <- function(bip,
                       factors = c("strain", "animal", "sex", "condition"),
                       strain = "strain") {
  ordr::mutate_cols(
    ordr::mutate_rows(
      ordr::confer_inertia(
        ordr::as_tbl_ord(
          stats::princomp(
            dplyr::select(bip, -factors),
            cor = TRUE)),
        1),
      strain = bip[[strain]]),
    trait = names(dplyr::select(bip, -factors)))
}

#' Biplot using ggplot2 via ordr package
#'
#' @param bip_pca 
#' @param scale.factor 
#'
#' @return object of class ggplot2
#' 
#' @importFrom ggplot2 aes
#' @importFrom ordr geom_cols_vector geom_cols_text_radiate geom_rows_point ggbiplot
#' @export
#'
#' @examples
biggplot <- function(bip_pca, scale.factor = 2) {
  ordr::ggbiplot(bip_pca) +
    ggplot2::aes(color = strain, sec.axes = "cols", scale.factor = scale.factor) +
    ordr::geom_rows_point() +
    ordr::geom_cols_vector() +
    ordr::geom_cols_text_radiate(aes(label = trait))
}
