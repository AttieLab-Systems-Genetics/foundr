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
  
  # Need traitnames to all be in datatraits for traitSignal.
  if(!all(traitnames %in% unique(traitSignal$datatraits)))
    return(NULL)
  
  # Need datatraits for traitSignal to be more than traitnames.
  if(all(unique(traitSignal$datatraits) %in% traitnames))
    return(NULL)
  
  # If condition is present, is it the same for all traits?
  # If not, then need to combine condition and trait throughout
  groupsex <- "sex"
  cond_trait <- FALSE
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
        cond_trait <- TRUE
        tmp1 <- 
          dplyr::rename(
            tidyr::unite(
              dplyr::filter(
                traitSignal,
                is_condition),
              condtraits,
              .data$condition, .data$trait,
              sep = ":", remove = FALSE),
            key_trait = "trait",
            trait = "condtraits")
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
  
  key_trait <- dplyr::filter(
      traitSignal,
      .data$datatraits %in% traitnames)
  ukey_trait <- dplyr::arrange(
    dplyr::mutate(
      dplyr::distinct(
        key_trait,
        .data$datatraits, .data$dataset, .data$trait),
      datatraits = factor(.data$datatraits, traitnames)),
    .data$datatraits)
  
  # Identify subset of strain, sex, condition included.
  conds <- condset(traitSignal)
  factors <- unique(
    tidyr::unite(
      dplyr::distinct(
        key_trait,
        dplyr::across(conds)),
      levels,
      tidyr::matches(conds))$levels)
  ofactors <- tidyr::unite(
    traitSignal,
    levels,
    tidyr::matches(conds))$levels %in% factors
  
  traitSignal <- traitSignal[ofactors,]
  
  # Careful handling if condtion and trait combined.
  # also handled in `trait_pivot()` by checking for `key_trait` column.
  if(cond_trait) {
    newcol <- 
      dplyr::mutate(
        dplyr::rename(
          dplyr::distinct(traitSignal, trait, condition, key_trait),
          key_condition = "condition",
          condtrait = "trait"),
        key_trait = ifelse(is.na(key_trait),
                           condtrait, as.character(key_trait)))
  }
  
  key_trait <- trait_pivot(key_trait, term, groupsex)
  traitSignal <- trait_pivot(
    dplyr::filter(
      traitSignal,
      !(.data$datatraits %in% traitnames)),
    term, groupsex)
  
  # Match up two tables if needed by first doing `inner_join` based on
  # conditions and then reassigning.
  out <- dplyr::inner_join(
    key_trait,
    traitSignal,
    by = "cond")
  key_trait <- out[names(key_trait)[-1]]
  traitSignal <- out[names(traitSignal)[-1]]

  # Create data frame with absmax and columns of correlations.
  out <- as.data.frame(stats::cor(traitSignal, key_trait, use = "pair",
                                  method = "spearman"))
  out$absmax <- apply(out, 1, function(x) max(abs(x)))
  out$trait <- row.names(out)
  out <- dplyr::arrange(
    dplyr::select(
      dplyr::as_tibble(out),
      .data$trait, .data$absmax, dplyr::everything()),
    dplyr::desc(.data$absmax))
  
  # Rearrange as dataframe with dataset, trait, absmax, key_dataset, key_trait, cors
  out <- 
    dplyr::mutate(
      tidyr::separate_wider_delim(
        tidyr::separate_wider_delim(
          tidyr::pivot_longer(
            out,
            tidyr::all_of(names(key_trait)),
            names_to = "key_trait", values_to = "cors"),
          trait,
          delim = ": ", names = c("dataset", "trait")),
        key_trait,
        delim = ": ",
        names = c("key_dataset", "key_trait")),
      key_trait = factor(.data$key_trait, unique(ukey_trait$trait)),
      key_dataset = factor(.data$key_dataset, unique(ukey_trait$dataset)))
  
  # If condition and trait may be combined; separate now.
  if(cond_trait) {
    # Expand out trait if needed.
    out <- 
      dplyr::select(
        dplyr::left_join(
          dplyr::rename(out, condtrait = "trait"),
          dplyr::rename(newcol,
                        condition = "key_condition",
                        trait = "key_trait"),
          by = "condtrait"),
        -condtrait)
    
    # Expand out key_trait if needed.
    out <- 
      dplyr::select(
        dplyr::select(
          dplyr::left_join(
            dplyr::rename(out, condtrait = "key_trait"),
            newcol,
            by = "condtrait"),
          -condtrait),
        dataset, trait, condition, key_dataset, key_trait, key_condition,
        dplyr::everything())
    
    if(all(is.na(out$condition))) out$condition <- NULL
    if(all(is.na(out$key_condition))) out$key_condition <- NULL
  }
  
  class(out) <- c("bestcor", class(out))
  out
}
# Pivot wider to put each trait in its own column
trait_pivot <- function(traitSignal, term, groupsex) {
  if(term == "signal")
    traitSignal$cellmean <- NULL
  else
    traitSignal$signal <- NULL
  
  if(!("dataset" %in% names(traitSignal)))
    traitSignal$dataset <- "unknown"
  
  # Handling `condition`.
  if(groupsex == "sex")
    conds <- c("strain", "sex")
  else
    conds <- c("strain", "sex", "condition")
  
  # Handling `key_trait`
  if("key_trait" %in% names(traitSignal)) {
    traitSignal <- dplyr::select(traitSignal, -condition, -key_trait)
  }
  
  # Pivot datatraits wider for correlations later.
  tidyr::pivot_wider(
    # Arrange by datatraits and conditions.
    dplyr::arrange(
      # Combine conditions and put as first column.
      tidyr::unite(
        # Remake datatraits as trait may have changed.
        tidyr::unite(
          traitSignal,
          datatraits,
          .data$dataset, .data$trait,
          sep = ": "),
        cond, tidyr::matches(conds)),
      .data$datatraits, .data$cond),
    names_from = "datatraits", values_from = term)
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

#' Is object a BestCor object?
#'
#' @param object object of class `bestcor`
#'
#' @return logical
#' @export
#' @rdname bestcor
is_bestcor <- function(object) {
  !is.null(object) && nrow(object) && ("cors" %in% names(object))
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
#' @importFrom ggplot2 aes autoplot facet_grid
#'             geom_vline geom_point ggplot scale_y_discrete theme
#' @importFrom stats reorder
#' @importFrom rlang .data
#' 
#' @rdname bestcor
#'
ggplot_bestcor <- function(object, mincor = 0.7, abscor = TRUE, ...) {
  if(!is_bestcor(object))
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
    ggplot2::aes(.data$cors, .data$trait, col = .data$key_trait) +
    ggplot2::geom_point(size = 2) + 
    ggplot2::facet_grid(.data$key_dataset + .data$key_trait ~ .) +
    ggplot2::theme(
      legend.position = "none") +
    ggplot2::scale_y_discrete(limits = rev)
  if(!abscor) {
    p <- p + ggplot2::geom_vline(xintercept = 0, color = "darkgray")
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
  if(is.null(object) || !("cors" %in% names(object)))
    return(NULL)
  
  dplyr::mutate(
    dplyr::select(
      dplyr::filter(
        dplyr::rename(
          object,
          correlation = "cors"),
        .data$absmax >= mincor),
      -.data$absmax),
    correlation = signif(.data$correlation, 4))
}
#' @export
#' @rdname bestcor
#' @method summary bestcor
summary.bestcor <- function(object, ...) 
  summary_bestcor(object, ...)
  
