#' Bind trait data together into unified objects.
#'
#' @param datasets vector of dataset names.
#' @param dirname name of directory
#'
#' @return side effects: save data in local RDS and CSV files
#' @export
#' @importFrom dplyr bind_rows filter
#' @importFrom rlang .data
#' @importFrom purrr map set_names
#' @importFrom readr write_csv
#'
bind_traits <- function(datasets, dirname = ".") {
  traitStats <- bind_traits_object(datasets, "Stats", dirname)

  # This should have been done already.
  # Reduce to traits that can produce valid stats.
  # Drop traits with any missing p.values
  dropTraits <- unique(
    dplyr::filter(
      # Ignore "noise" term as it has no p.value.
      dplyr::filter(
        traitStats,
        .data$term != "noise"),
      is.na(.data$p.value)))$trait
  
  traitStats <- dplyr::filter(traitStats, !(.data$trait %in% dropTraits))
  
  # Additional traits were dropped due to failed fit. Keep what is left.
  keepTraits <- unique(traitStats$trait)

  saveRDS(traitStats, "traitStats.rds")
  readr::write_csv(traitStats, "traitStats.csv")
  
  rm(traitStats)
  
  traitData <- 
    dplyr::filter(
      bind_traits_object(datasets, "Data", dirname),
      .data$trait %in% keepTraits)
  
  saveRDS(traitData, "traitData.rds")
  readr::write_csv(traitData, "traitData.csv")
  
  rm(traitData)
  
  traitSignal <-
    dplyr::filter(
      bind_traits_object(datasets, "Signal", dirname),
      .data$trait %in% keepTraits)

  saveRDS(traitSignal, "traitSignal.rds")
  readr::write_csv(traitSignal, "traitSignal.csv")
  
  rm(traitSignal)
  
  invisible()
}
bind_traits_object <- function(datasets, filetype, dirname = ".") {
  # This assumes columns are in right order.
  dplyr::bind_rows(
    purrr::set_names(
      purrr::map(
        datasets,
        function(x) {
          readRDS(
            paste0(
              file.path(dirname, x),
              filetype,
              ".rds"))
        }),
      datasets),
    .id = "dataset")
}

