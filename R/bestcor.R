#' Traits with Best Correlation
#'
#' Find other traits with best Spearman correlation with selected traitnames.
#' 
#' @param traitSignal data frame from `partition`
#' @param traitnames names of traits in `traitSignal`
#' @param term either `signal` or `mean`
#'
#' @return sorted vector of absolute correlations with names
#' @export
#' @importFrom dplyr across arrange as_tibble bind_rows desc distinct
#'             everything filter mutate select
#' @importFrom tidyr matches pivot_wider separate_wider_delim unite
#' @importFrom stats cor
#' @importFrom rlang .data
#'
#' @examples
#' sampleSignal <- partition(sampleData)
#' out <- bestcor(sampleSignal, "sample: A")
#' ggplot_bestcor(out, 0)
#' ggplot_bestcor(out, 0, abscor = FALSE)
bestcor <- function(traitSignal,
                    traitnames = NULL,
                    term = c("cellmean", "signal")) {
  term <- match.arg(term)

  # Check if traitSignal is missing.
  if(is.null(traitSignal) | !nrow(traitSignal))
    return(NULL)
  
  traitSignal <- tidyr::unite(
    traitSignal,
    datatraits,
    .data$dataset, .data$trait,
    sep = ": ", remove = FALSE)
  
  if(is.null(traitnames))
    traitnames <- traitSignal$datatraits[1]
  if(!all(traitnames %in% unique(traitSignal$datatraits)))
    return(NULL)
  
  # If condition is present, is it the same for all traits?
  # If not, then need to combine condition and trait throughout
  groupsex <- "sex"
  if("condition" %in% names(traitSignal)) {
    if(all(is_condition <- is.na(traitSignal$condition))) {
      # traitSignal does not use condition, so drop condition column
      traitSignal$condition <- NULL
    } else {
      # Adjust traitSignal 
      is_condition <- !is_condition
      if(all(is_condition)) {
        # All traits have condition.
        groupsex <- "sex_condition"
      } else {
        # Unite trait and condition for those with condition.
        tmp1 <- tidyr::unite(
          dplyr::filter(
            traitSignal,
            is_condition),
          trait,
          .data$condition, .data$trait,
          sep = ":")
        # Keep trait as is for those without condtion
        tmp2 <- dplyr::select(
          dplyr::filter(
            traitSignal,
            !is_condition),
          -.data$condition)
        # Drop any condition:trait entries in tmp2 already in tmp1
        is_dup <- (tmp2$trait %in% tmp1$trait)
        if(any(is_dup)) {
          tmp2 <- tmp2[!is_dup,]
        }
        traitSignal <- dplyr::bind_rows(tmp1, tmp2)
      }
    }
  }
  
  proband <- dplyr::filter(
      traitSignal,
      .data$datatraits %in% traitnames)
  uproband <- dplyr::arrange(
    dplyr::mutate(
      dplyr::distinct(
        proband,
        .data$datatraits, .data$dataset, .data$trait),
      datatraits = factor(.data$datatraits, traitnames)),
    .data$datatraits)
  
  # Identify subset of strain, sex, condition included.
  conds <- condset(proband)
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
    if(term == "signal")
      traitSignal$cellmean <- NULL
    else
      traitSignal$signal <- NULL
    
    if(!("dataset" %in% names(traitSignal)))
      traitSignal$dataset <- "unknown"
    
    if(groupsex == "sex")
      conds <- c("strain", "sex")
    else
      conds <- c("strain", "sex", "condition")
    
    # Remake datatraits as trait may have changed.
    
    dplyr::select(
      tidyr::pivot_wider(
        dplyr::arrange(
          tidyr::unite(
            traitSignal,
            datatraits,
            .data$dataset, .data$trait,
            sep = ": "),
          .data$datatraits, dplyr::across(conds)),
        names_from = "datatraits", values_from = term),
      -tidyr::matches(conds))
  }

  proband <- myfun(proband, term, groupsex)
  traitSignal <- myfun(
    dplyr::filter(
      traitSignal,
      !(.data$datatraits %in% traitnames)),
    term, groupsex)
  
  # Create data frame with absmax and columns of correlations.
  out <- as.data.frame(stats::cor(traitSignal, proband, use = "pair",
                                  method = "spearman"))
  out$absmax <- apply(out, 1, function(x) max(abs(x)))
  out$trait <- row.names(out)
  out <- dplyr::arrange(
    dplyr::select(
      dplyr::as_tibble(out),
      .data$trait, .data$absmax, dplyr::everything()),
    dplyr::desc(.data$absmax))
  
  # Rearrange as dataframe with dataset, trait, absmax, probandset, proband, cors
  out <- 
    dplyr::mutate(
      tidyr::separate_wider_delim(
        tidyr::separate_wider_delim(
          tidyr::pivot_longer(
            out,
            tidyr::all_of(names(proband)),
            names_to = "proband", values_to = "cors"),
          trait,
          delim = ": ", names = c("dataset", "trait")),
        proband,
        delim = ": ",
        names = c("probandset", "proband")),
      proband = factor(.data$proband, unique(uproband$trait)),
      probandset = factor(.data$probandset, unique(uproband$dataset)))
  
  class(out) <- c("bestcor", class(out))
  out
}

bestcorStats <- function(traitStats, traitnames = NULL,
                         bestcorObject,
                         term = c("signal", "cellmean")) {
  corterm <- match.arg(term)

  if(is.null(traitStats))
    return(NULL)
  
  bestcorObject <- 
    tidyr::unite(
      bestcorObject,
      datatraits,
      .data$dataset, .data$trait,
      sep = ": ")
  
  if(is.null(traitnames))
    traitnames <- traitStats$datatraits[1]
  
  traitnames <- unique(c(
    traitnames,
    bestcorObject$datatraits
  ))
  
  if(is.null(traitnames))
    return(NULL)
  
  traitStats <- tidyr::unite(
    traitStats,
    datatraits,
    .data$dataset, .data$trait,
    sep = ": ", remove = FALSE)
  
  if(!all(m <- (traitnames %in% unique(traitStats$datatraits)))) {
    traitnames <- traitnames[m]
  }
  if(!length(traitnames))
    return(NULL)
  
  dplyr::filter(
    dplyr::select(
      dplyr::arrange(
        dplyr::mutate(
          traitStats,
          datatraits = factor(.data$datatraits, traitnames)),
        .data$datatraits),
      -.data$datatraits),
    .data$term == corterm)
}

#' GGplot of bestcor object
#'
#' @param object object of class `bestcor`
#' @param mincor minimum absolute correlation to plot
#' @param abscor plot absolute value of correlation if `TRUE`
#' @param ... additional parameters not used
#'
#' @return ggplot object
#' @export
#' @importFrom tidyr pivot_longer unite
#' @importFrom dplyr mutate select
#' @importFrom ggplot2 aes autoplot element_text facet_grid
#'             geom_hline geom_point ggplot theme
#' @importFrom stats reorder
#' @importFrom rlang .data
#' 
#' @rdname bestcor
#'
ggplot_bestcor <- function(object, mincor = 0.7, abscor = TRUE, ...) {
  if(is.null(object) || !nrow(object))
    return(plot_null("No Correlations to Plot."))

  if(abscor) {
    object <- dplyr::mutate(object, cors = abs(.data$cors))
  }
  
  object <- 
    dplyr::select(
      dplyr::filter(
        dplyr::mutate(
          tidyr::unite(
            object,
            trait,
            .data$dataset, .data$trait,
            sep = ": "),
          trait = stats::reorder(.data$trait, dplyr::desc(absmax))),
        .data$absmax >= mincor),
    -.data$absmax)

  if(!nrow(object))
    return(plot_null(paste("No Correlations above", mincor)))
  
  p <- ggplot2::ggplot(object) +
    ggplot2::aes(.data$trait, .data$cors, col = .data$proband) +
    ggplot2::geom_point(size = 2) + 
    ggplot2::facet_grid(.data$probandset + .data$proband ~ .) +
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

#' Summary of bestcor objects
#'
#' @param object object of class `bestcor`
#' @param mincor minimum correlation to show
#' @param ... not used
#'
#' @return data frame
#' @export
#' @rdname bestcor
#' @importFrom dplyr filter mutate select
#' @importFrom rlang .data
#'
summary_bestcor <- function(object, mincor = 0.5, ...) {
  dplyr::mutate(
    dplyr::select(
      dplyr::filter(
        object,
        .data$absmax >= mincor),
      -.data$absmax),
    cors = signif(.data$cors, 4))
}
#' @export
#' @rdname bestcor
#' @method summary bestcor
summary.bestcor <- function(object, ...) 
  summary_bestcor(object, ...)
  
