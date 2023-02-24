#' Select Traits and Strains for Response
#'
#' @param traitData data frame
#' @param traitSignal data frame
#' @param traitnames names of traits to subset
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
                        traitnames = unique(traitSignal$trait),
                        strains = names(CCcolors),
                        response = c("individual", "mean", "signal", "ind_signal"),
                        abbrev = TRUE) {
  if("condition" %in% names(traitData)) {
    bys <- c("datatype","strain","sex","condition","trait")
  } else {
    bys <- c("datatype","strain","sex","trait")
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
    
    traitData <- dplyr::filter(
      traitData,
      trait %in% traitnames,
      strain %in% strains)
    
  } else {
    traitData <- selectSignal(traitSignal, traitnames, strains, response)
  }
  
  if(abbrev) {
    ltrait <- length(traitnames)
    # Temporary. Need to address datatype as well.
    m <- which(!duplicated(traitData$trait))
    names(m) <- traitData$trait[m]
    m <- m[traitnames]
    
    traitData <- dplyr::mutate(
      traitData,
      trait = abbreviate(trait, ceiling(60 / ltrait)),
      trait = factor(trait, trait[m]))
  }
  
  if("condition" %in% names(traitData)) {
    traitData <- tidyr::unite(
      traitData,
      sex_condition, sex, condition,
      remove = FALSE,
      na.rm = TRUE)
  }
  traitData
}
