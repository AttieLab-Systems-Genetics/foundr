#' Harmonize Dataset
#'
#' @param dataset name of dataset
#' @param links data frame with `shortname`, `address`, `longname`
#' @param userHarmony function provided by user
#' @param ... additional optional parameters to `userHarmony()`
#'
#' @return side action to save RDS files locally
#' @export
#' @importFrom dplyr filter
#'
#' @examples
harmonize <- function(dataset, links, userHarmony, ...) {
  # Harmonize data with user-supplied harmony function.
  # Function must have `dataset` as first argument and include `...` argument.
  cat("Harmonizing raw data ...\n", stderr())
  traitData <- userHarmony(dataset, links, ...)
  
  cat("Running statistics on traits ...\n", stderr())
  traitStats <- strainstats(traitData)
  
  # Reduce to traits that can produce valid stats.
  # Drop traits with any missing p.values
  dropTraits <- unique(
    dplyr::filter(
      # Ignore "noise" term as it has no p.value.
      dplyr::filter(
        traitStats,
        term != "noise"),
      is.na(p.value)))$trait
  traitStats <- dplyr::filter(traitStats, !(trait %in% dropTraits))
  # Additional traits were dropped due to failed fit. Keep what is left.
  keepTraits <- unique(traitStats$trait)
  
  saveRDS(traitStats, paste0(dataset, "Stats.rds"))
  
  traitData <- traitData %>%
    dplyr::filter(trait %in% keepTraits)
  saveRDS(traitData, paste0(dataset, "Data.rds"))
  
  cat("Running partition of traits ...\n", stderr())
  traitSignal <- partition(traitData) %>%
    dplyr::filter(trait %in% keepTraits)
  saveRDS(traitSignal, paste0(dataset, "Signal.rds"))
  
  invisible()
}
