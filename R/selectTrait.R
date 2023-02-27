#' Select Traits and Strains for Response
#'
#' @param traitData data frame
#' @param traitSignal data frame
#' @param traitnames names of `dataset: trait` combinations to subset
#' @param strains names of strains to subset
#' @param response name of response to return
#' @param abbrev abbreviate names if `TRUE`
#'
#' @return data frame
#' @export
#' @importFrom dplyr all_of filter left_join mutate select
#' @importFrom tidyr unite
#' @importFrom rlang .data
#'
#' @examples
selectTrait <- function(traitData, traitSignal,
                        traitnames = unite_datatraits(traitSignal),
                        strains = names(CCcolors),
                        response = c("individual", "mean", "signal", "ind_signal"),
                        abbrev = TRUE,
                        sep = ": ") {
  if("condition" %in% names(traitData)) {
    bys <- c("dataset","strain","sex","condition","trait")
  } else {
    bys <- c("dataset","strain","sex","trait")
  }
  
  response <- match.arg(response)
  if(response %in% c("individual", "ind_signal")) {
    if(response == "ind_signal") {
      # This does not appear to be working
      # And scale is off.
      traitData <-
        dplyr::select(
          dplyr::mutate(
            dplyr::left_join(
              traitData,
              traitSignal,
              by = bys),
            value = value - mean + signal),
          -mean, -signal)
    }
    
    traitData <- 
      dplyr::filter(
        unite_datatraits(traitData, traitnames, TRUE),
        strain %in% strains)

  } else {
    traitData <- selectSignal(traitSignal, traitnames, strains, response)
  }
  
  # This breaks when traitnames = `dataset: trait`
  # Want to preserve order of traitnames into traitData
  if(abbrev) {
    ltrait <- length(traitnames)
    # Temporary. Need to address dataset as well.
    tmp <- tidyr::unite(traitData, datatraits, dataset, trait, sep = sep, remove = FALSE)
    m <- which(!duplicated(tmp$datatraits))
    names(m) <- tmp$datatraits[m]
    m <- m[traitnames]
    
    traitData <- dplyr::mutate(
      traitData,
      trait = abbreviate(trait, ceiling(60 / ltrait)),
      trait = factor(trait, unique(trait[m])),
      dataset = factor(dataset, unique(dataset[m])))
  }
  
  if("condition" %in% names(traitData)) {
    traitData <- tidyr::unite(
      traitData,
      sex_condition, sex, condition,
      remove = FALSE,
      na.rm = TRUE)
  }
  
  class(traitData) <- c("traitData", class(traitData))
  traitData
}
