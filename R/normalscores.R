normalscores <- function(object) {
  # Normal scores by trait
  dplyr::ungroup(
    dplyr::mutate(
      dplyr::group_by(
        object,
        trait),
      value = nqrank(value, jitter = TRUE)))  
}
