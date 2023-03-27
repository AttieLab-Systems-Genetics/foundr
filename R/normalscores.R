normalscores <- function(object) {
  # Normal scores by trait
  dplyr::filter(
    dplyr::ungroup(
      dplyr::mutate(
        dplyr::group_by(
          object,
          .data$trait),
        value = nqrank(.data$value, jitter = TRUE))),
    !is.na(.data$value),
    !is.nan(.data$value)) 
}
