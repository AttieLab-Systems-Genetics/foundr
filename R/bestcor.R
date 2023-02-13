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
#' @importFrom dplyr across arrange desc distinct filter select
#' @importFrom tidyr matches pivot_wider unite
#' @importFrom tibble as_tibble
#'
#' @examples
bestcor <- function(object, traits, term = c("signal", "mean")) {
  term <- match.arg(term)
  
  # Need to check if condition is present.
  # Need to check if traits are missing some combos
  
  if(is.null(object) | is.null(traits))
    return(NULL)
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
  
  # Create data frame with absmax and columns of correlations.
  out <- as.data.frame(cor(object, proband, use = "pair"))
  out$absmax <- apply(out, 1, function(x) max(abs(x)))
  out$trait <- row.names(out)
  out <- dplyr::arrange(
    dplyr::select(
      tibble::as_tibble(out),
      trait, absmax, dplyr::everything()),
    dplyr::desc(absmax))
  class(out) <- c("bestcor", class(out))
  out
}
#' GGplot of bestcor object
#'
#' @param object object of class `bestcor`
#' @param mincor minimum absolute correlation to plot
#' @param abscor plot absolute value of correlation if `TRUE`
#' @param ... additional parameters not used
#'
#' @return
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#' @importFrom ggplot2 aes autoplot element_text facet_grid geom_point ggplot theme
#' @rdname bestcor
#'
#' @examples
ggplot_bestcor <- function(object, mincor = 0.7, abscor = TRUE, ...) {
  object <- tidyr::pivot_longer(
    dplyr::select(
      dplyr::filter(
        dplyr::mutate(
          object,
          trait = stats::reorder(trait, dplyr::desc(absmax))),
        absmax >= mincor),
      -absmax),
    -trait,
    names_to = "proband",
    values_to = "cors")
  
  if(abscor) {
    object <- dplyr::mutate(object, cors = abs(cors))
  }

  p <- ggplot2::ggplot(object) +
    ggplot2::aes(trait, cors, col = proband) +
    ggplot2::geom_point(size = 2) + 
    ggplot2::facet_grid(proband ~ .) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))
  if(!abscor) {
    p <- p + ggplot2::geom_hline(yintercept = 0, color = "darkgray")
  }
  p
}
#' @rdname bestcor
#' @method autoplot bestcor
#'
autoplot.bestcor <- function(object, ...) {
  ggplot_bestcor(object, ...)
}
