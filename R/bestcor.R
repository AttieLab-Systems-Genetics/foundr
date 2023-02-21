#' Traits with Best Correlation
#'
#' Find other traits with best correlation with selected traits.
#' 
#' @param traitSignal data frame from `partition`
#' @param traits names of traits in `traitSignal`
#' @param term either `signal` or `mean`
#'
#' @return sorted vector of absolute correlations with names
#' @export
#' @importFrom dplyr across arrange desc distinct filter mutate select
#' @importFrom tidyr matches pivot_wider unite
#' @importFrom tibble as_tibble
#'
#' @examples
#' sampleSignal <- partition(sampleData)
#' out <- bestcor(sampleSignal, "A")
#' ggplot_bestcor(out, 0)
#' ggplot_bestcor(out, 0, abscor = FALSE)
bestcor <- function(traitSignal, traits, term = c("signal", "mean")) {
  term <- match.arg(term)
  
  # Need to check if condition is present.
  # Need to check if traits are missing some combos
  
  if(is.null(traitSignal) | is.null(traits))
    return(NULL)
  if(!all(traits %in% unique(traitSignal$trait)))
    return(NULL)
  
  proband <- dplyr::filter(traitSignal, trait %in% traits)
  
  if("condition" %in% names(traitSignal)) {
    if(all(is.na(proband$condition))) {
      # proband does not use condition, so subset to traits that agree
      proband$condition <- NULL
      traitSignal <- dplyr::filter(traitSignal, is.na(condition))
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
    traitSignal,
    levels,
    tidyr::matches(conds))$levels %in% factors
  
  traitSignal <- traitSignal[ofactors,]
  
  # Pivot wider to put each trait in its own column
  myfun <- function(traitSignal, term, groupsex) {
    if(term == "mean")
      traitSignal$signal <- NULL
    else
      traitSignal$mean <- NULL
    
    if(!("datatype" %in% names(traitSignal)))
      traitSignal$datatype <- "unknown"
    traitSignal <- tidyr::unite(
      traitSignal,
      datatype_trait,
      datatype, trait,
      sep = ";")

    if(groupsex == "sex")
      conds <- c("strain", "sex")
    else
      conds <- c("strain", "sex", "condition")
    
    dplyr::select(
      tidyr::pivot_wider(
        dplyr::arrange(
          traitSignal,
          datatype_trait, dplyr::across(conds)),
        names_from = "datatype_trait", values_from = term),
      -tidyr::matches(conds))
  }

  proband <- myfun(proband, term, groupsex)
  traitSignal <- myfun(
    dplyr::filter(traitSignal, !(trait %in% traits)),
    term, groupsex)
  
  # Create data frame with absmax and columns of correlations.
  out <- as.data.frame(cor(traitSignal, proband, use = "pair"))
  out$absmax <- apply(out, 1, function(x) max(abs(x)))
  out$trait <- row.names(out)
  out <- dplyr::arrange(
    dplyr::select(
      tibble::as_tibble(out),
      trait, absmax, dplyr::everything()),
    dplyr::desc(absmax))
  
  # Rearrange as dataframe with datatype, trait, absmax, probandtype, proband, cors
  out <- 
    tidyr::separate(
      tidyr::separate(
        tidyr::pivot_longer(
          out,
          tidyr::all_of(names(proband)),
          names_to = "proband", values_to = "cors"),
        trait, c("datatype", "trait"), sep = ";"),
      proband, c("probandtype", "proband"), sep = ";")
  class(out) <- c("bestcor", class(out))
  out
}

bestcorStats <- function(traitStats, traitnames = "") {
  if(any(traitnames == "") | is.null(traitnames) | is.null(traitStats))
    return(traitStats)
  
  dplyr::mutate(
    dplyr::arrange(
      dplyr::mutate(
        traitStats,
        trait = factor(trait, unique(traitnames))),
      trait),
    trait = as.character(trait))
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
#' @importFrom tidyr pivot_longer unite
#' @importFrom dplyr mutate select
#' @importFrom ggplot2 aes autoplot element_text facet_grid geom_point ggplot theme
#' @rdname bestcor
#'
ggplot_bestcor <- function(object, mincor = 0.7, abscor = TRUE, ...) {
  if(is.null(object))
    return(NULL)
  
  if(!nrow(object))
    return(NULL)
  
  if(abscor) {
    object <- dplyr::mutate(object, cors = abs(cors))
  }
  
  object <- 
    dplyr::select(
      dplyr::filter(
        dplyr::mutate(
          tidyr::unite(
            object,
            trait, datatype, trait, sep = ": "),
          trait = stats::reorder(trait, dplyr::desc(absmax))),
        absmax >= mincor),
    -absmax)

  p <- ggplot2::ggplot(object) +
    ggplot2::aes(trait, cors, col = proband) +
    ggplot2::geom_point(size = 2) + 
    ggplot2::facet_grid(probandtype + proband ~ .) +
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
