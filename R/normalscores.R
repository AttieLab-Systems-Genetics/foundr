normalscores <- function(object) {
  # Normal scores by trait
  dplyr::filter(
    dplyr::ungroup(
      dplyr::mutate(
        dplyr::group_by(
          object,
          trait),
        value = nqrank(value, jitter = TRUE))),
    !is.na(value),
    !is.nan(value)) 
}
