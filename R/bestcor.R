#' Traits with Best Correlation
#'
#' Find other traits with best correlation with selected traits.
#' 
#' @param object data frame from `partition`
#' @param traits names of traits in `object`
#' @param term either `signal` or `mean`
#'
#' @return sorted vector of absolute correlations with names
#' @export
#' @importFrom dplyr across arrange distinct filter select
#' @importFrom tidyr matches pivot_wider unite
#'
#' @examples
bestcor <- function(object, traits, term = c("signal", "mean")) {
  term <- match.arg(term)
  
  # Need to check if condition is present.
  # Need to check if traits are missing some combos
  
  if(!all(traits %in% unique(object$trait)))
    return(NULL)
  
  proband <- dplyr::filter(object, trait %in% traits)
  
  if("condition" %in% names(object)) {
    if(all(is.na(proband$condition))) {
      # proband does not use condition, so subset to traits that agree
      proband$condition <- NULL
      object <- dplyr::filter(object, is.na(condition))
      groupsex <- "sex"
    } else {
      groupsex <- "sex_condition"
    }
  } else {
    groupsex <- "sex"
  }
  
  # Need to first check subset of strain, sex, condition included.
  if(groupsex == "sex")
    conds <- c("strain", "sex")
  else
    conds <- c("strain", "sex", "condition")
  
  factors <- unique(
    tidyr::unite(
      dplyr::distinct(
        proband,
        dplyr::across(conds)),
      levels,
      tidyr::matches(conds))$levels)
  ofactors <- tidyr::unite(
    object,
    levels,
    tidyr::matches(conds))$levels %in% factors
  
  object <- object[ofactors,]
  
  # Pivot wider to put each trait in its own column
  myfun <- function(object, term, groupsex) {
    if(term == "mean")
      object$signal <- NULL
    else
      object$mean <- NULL
    if("datatype" %in% names(object))
      object$datatype <- NULL

    if(groupsex == "sex")
      conds <- c("strain", "sex")
    else
      conds <- c("strain", "sex", "condition")
    
    dplyr::select(
      tidyr::pivot_wider(
        dplyr::arrange(
          object,
          trait, dplyr::across(conds)),
        names_from = "trait", values_from = term),
      -tidyr::matches(conds))
  }

  proband <- myfun(proband, term, groupsex)
  object <- myfun(
    dplyr::filter(object, !(trait %in% traits)),
    term, groupsex)

  sort(
    apply(
      cor(object, proband, use = "pair"),
      1, function(x) max(abs(x))),
    decreasing = TRUE)
}
