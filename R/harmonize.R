#' Harmonize Dataset
#'
#' @param dataset name of dataset
#' @param links data frame with `shortname`, `address`, `longname`
#' @param userHarmony function provided by user
#' @param ... additional optional parameters to `userHarmony()`
#' @param normalize apply `normalscores` if `TRUE`
#' @param condition_name name of `condition` column if present.
#'
#' @return side action to save RDS files locally
#' @export
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
harmonize <- function(dataset, links, userHarmony, ...,
                      normalize = TRUE,
                      condition_name = "condition") {
  # Harmonize data with user-supplied harmony function.
  # Function must have `dataset` as first argument and include `...` argument.
  cat("Harmonizing raw data ...\n", stderr())
  traitData <- userHarmony(dataset, links, ...)

  cat("Running statistics on traits ...\n", stderr())
  # Always run stats on normalized data.
  if(normalize) {
    traitData <- normalscores(traitData)
    traitStats <- strainstats(traitData,
                              condition_name = condition_name)
  } else {
    traitStats <- strainstats(normalscores(traitData),
                              condition_name = condition_name)
  }
  
  # Reduce to traits that can produce valid stats.
  # Drop traits with any missing p.values
  dropTraits <- unique(
    dplyr::filter(
      # Ignore "noise" term as it has no p.value.
      dplyr::filter(
        traitStats,
        .data$term != "noise"),
      is.na(.data$p.value))$trait)
  traitStats <-
    dplyr::filter(
      traitStats,
      !(.data$trait %in% dropTraits))
  # Additional traits were dropped due to failed fit. Keep what is left.
  keepTraits <- unique(traitStats$trait)
  
  if(!dir.exists(dataset))
    dir.create(dataset)
  
  saveRDS(traitStats, file.path(dataset, paste0(dataset, "Stats.rds")))
  
  traitData <-
    dplyr::filter(
      traitData,
      .data$trait %in% keepTraits)
  saveRDS(traitData, file.path(dataset, paste0(dataset, "Data.rds")))
  
  cat("Running partition of traits ...\n", stderr())
  traitSignal <- 
    dplyr::filter(
      partition(traitData),
      .data$trait %in% keepTraits)
  saveRDS(traitSignal, file.path(dataset, paste0(dataset, "Signal.rds")))
  
  invisible()
}
