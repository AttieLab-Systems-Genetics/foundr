#' Select Signal
#'
#' @param traitSignal data frame
#' @param traitnames names of traits to subset
#' @param strains names of strains to subset
#' @param response name of response
#'
#' @return data frame
#' @export
#' @importFrom dplyr all_of arrange filter mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#'
#' @examples
selectSignal <- function(traitSignal,
                         traitnames,
                         strains = names(CCcolors),
                         response = c("mean", "signal")) {
  if(is.null(traitSignal))
    return(NULL)
  if(!all(traitnames %in% traitSignal$trait))
    return(NULL)
  
  response <- match.arg(response)
  
  if("condition" %in% names(traitSignal)) {
    bys <- c("dataset","strain","sex","condition","trait")
  } else {
    bys <- c("dataset","strain","sex","trait")
  }

  dplyr::mutate(
    dplyr::select(
      dplyr::mutate(
        dplyr::filter(
          unite_datatraits(traitData, traitnames, TRUE),
          strain %in% strains),
        value = .data[[response]]),
      dplyr::all_of(c(bys, "value"))),
    strain = factor(strain, names(CCcolors)),
    value = signif(value, 4))
}
selectSignalWide <- function(traitSignal,
                             traitnames,
                             strains = names(CCcolors),
                             response = c("mean", "signal"),
                             condition_name = "condition") {
  
  out <- selectSignal(traitSignal, traitnames, strains, response)
  if(is.null(out))
    return(NULL)
  
  nout <- c(names(out), levels(out$strain))
  nout <- nout[!(nout %in% c("strain", "value"))]
  out <- dplyr::arrange(
    dplyr::select(
      tidyr::pivot_wider(
        out,
        names_from = "strain", values_from = "value"),
      dplyr::all_of(nout)),
    trait, sex)
  
  m <- match("condition", names(out))
  if(!is.na(m)) {
    names(out)[m] <- condition_name
  }
  out
}
