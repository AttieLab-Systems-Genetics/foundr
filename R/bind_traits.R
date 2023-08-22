#' Bind trait data together into unified objects.
#'
#' @param datasets vector of dataset names.
#' @param dirname name of directory
#' @param traitRoot root for trait dataset
#' @param CSV save as CSV if `TRUE`
#'
#' @return side effects: save data in local RDS and CSV files
#' @export
#' @importFrom dplyr bind_rows filter
#' @importFrom rlang .data
#' @importFrom purrr map set_names
#' @importFrom readr write_csv
#'
bind_traits <- function(datasets, dirname = ".", traitRoot = "trait",
                        CSV = FALSE) {
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

  saveRDS(traitStats, paste0(traitRoot, "Stats.rds"))
  if(CSV)
    readr::write_csv(traitStats, paste0(traitRoot, "Stats.csv"))
  
  traitData <- 
    dplyr::filter(
      bind_traits_object(datasets, "Data", dirname),
      .data$trait %in% keepTraits)
  
  saveRDS(traitData, paste0(traitRoot, "Data.rds"))
  if(CSV)
    readr::write_csv(traitData, paste0(traitRoot, "Data.csv"))
  
  traitSignal <-
    dplyr::filter(
      bind_traits_object(datasets, "Signal", dirname),
      .data$trait %in% keepTraits)

  saveRDS(traitSignal, paste0(traitRoot, "Signal.rds"))
  if(CSV)
    readr::write_csv(traitSignal, paste0(traitRoot, "Signal.csv"))
  
  traitObject <- list(
    Data = traitData,
    Signal = traitSignal,
    Stats = traitStats)
  class(traitObject) <- c("traitObject", class(traitObject))
  saveRDS(traitObject, paste0(traitRoot, "Object.rds"))
  
  invisible(traitObject)
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
              file.path(dirname, x, x),
              filetype,
              ".rds"))
        }),
      datasets),
    .id = "dataset")
}

