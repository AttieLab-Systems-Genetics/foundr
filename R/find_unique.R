find_unique <- function(data) {
  if(!("datatraits" %in% names(data))) {
    if("dataset" %in% names(data))
      data <- tidyr::unite(data, datatraits, dataset, trait)
    else
      data <- dplyr::rename(data, datatraits = "trait")
  }
  dplyr::filter(
    dplyr::summarise(
      dplyr::group_by(
        data,
        strain, sex, condition, datatraits),
      n = dplyr::n(), .groups = "drop"),
    n > 1L)
}