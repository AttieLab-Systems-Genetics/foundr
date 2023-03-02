#' Traits with Best Correlation
#'
#' Find other traits with best correlation with selected traitnames.
#' 
#' @param traitSignal data frame from `partition`
#' @param traitnames names of traits in `traitSignal`
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
bestcor <- function(traitSignal, traitnames, term = c("signal", "mean")) {
  term <- match.arg(term)
  
  # Need to check if condition is present.
  # Need to check if traitnames are missing some combos
  
  if(is.null(traitSignal) | is.null(traitnames))
    return(NULL)

  proband <- tidyr::unite(
    traitSignal,
    datatraits,
    dataset, trait,
    sep = ": ", remove = FALSE)
  
  if(!all(traitnames %in% unique(proband$datatraits)))
    return(NULL)
  
  proband <- dplyr::filter(
      proband,
      datatraits %in% traitnames)
  uproband <- dplyr::arrange(
    dplyr::mutate(
      dplyr::distinct(
        proband,
        datatraits, dataset, trait),
      datatraits = factor(datatraits, traitnames)),
    datatraits)
  
  # Figure out better way to do correlation by cond:trait if appropriate
  # Think calcium8G
  # Following seems to work if proband has all condition = NA, but not for mix.
  # But maybe computations are wrong?
  
  if("condition" %in% names(traitSignal)) {
    if(all(is.na(proband$condition))) {
      # proband does not use condition, so drop condition column
      proband$condition <- NULL
      
      # Adjust traitSignal
      is_condition <- !is.na(traitSignal$condition)
      if(any(is_condition)) {
        tmp1 <- tidyr::unite(
          dplyr::filter(
            traitSignal,
            is_condition),
          trait, condition, trait,
          sep = ":")
        tmp2 <- dplyr::select(
          dplyr::filter(
            traitSignal,
            !is_condition),
          -condition)
        
        # Drop any condition:trait entries in tmp2 already in tmp1
        is_dup <- (tmp2$trait %in% tmp1$trait)
        if(any(is_dup)) {
          tmp2 <- tmp2[!is_dup,]
        }
        traitSignal <- dplyr::bind_rows(tmp1, tmp2)
      } else {
        # subset traitSignal to traitnames that agree and drop condition
        traitSignal <- dplyr::filter(traitSignal, is.na(condition))
        traitSignal$condition <- NULL
      }
      if(!nrow(traitSignal))
        return(NULL)

      groupsex <- "sex"
    } else {
      groupsex <- "sex_condition"
    }
  } else {
    groupsex <- "sex"
  }
  
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
    if(term == "mean")
      traitSignal$signal <- NULL
    else
      traitSignal$mean <- NULL
    
    if(!("dataset" %in% names(traitSignal)))
      traitSignal$dataset <- "unknown"
    
    if(groupsex == "sex")
      conds <- c("strain", "sex")
    else
      conds <- c("strain", "sex", "condition")
    
    dplyr::select(
      tidyr::pivot_wider(
        dplyr::arrange(
          dplyr::select(
            traitSignal,
            -dataset, -trait),
          datatraits, dplyr::across(conds)),
        names_from = "datatraits", values_from = term),
      -tidyr::matches(conds))
  }

  proband <- myfun(proband, term, groupsex)
  traitSignal <- myfun(
    dplyr::filter(
      tidyr::unite(
        traitSignal,
        datatraits,
        dataset, trait,
        sep = ": ", remove = FALSE),
      !(datatraits %in% traitnames)),
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
  
  # Rearrange as dataframe with dataset, trait, absmax, probandset, proband, cors
  out <- 
    dplyr::mutate(
      tidyr::separate(
        tidyr::separate(
          tidyr::pivot_longer(
            out,
            tidyr::all_of(names(proband)),
            names_to = "proband", values_to = "cors"),
          trait, c("dataset", "trait"), sep = ": "),
        proband, c("probandset", "proband"), sep = ": "),
      proband = factor(proband, unique(uproband$trait)),
      probandset = factor(probandset, unique(uproband$dataset)))
  
  class(out) <- c("bestcor", class(out))
  out
}

bestcorStats <- function(traitStats, traitnames = NULL,
                         bestcorObject) {

  if(is.null(traitStats))
    return(NULL)
  
  traitnames <- unique(c(
    traitnames,
    tidyr::unite(
      bestcorObject,
      datatraits,
      dataset, trait,
      sep = ": ")$datatraits
  ))
  
  if(is.null(traitnames))
    return(NULL)
  
  traitStats <- tidyr::unite(
    traitStats,
    datatraits,
    dataset, trait,
    sep = ": ", remove = FALSE)
  
  if(!all(m <- (traitnames %in% unique(traitStats$datatraits)))) {
    traitnames <- traitnames[m]
  }
  if(!length(traitnames))
    return(NULL)
  
  dplyr::select(
    dplyr::arrange(
      dplyr::mutate(
        traitStats,
        datatraits = factor(datatraits, traitnames)),
      datatraits),
    -datatraits)
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
  if(is.null(object) || !nrow(object))
    return(plot_null("No Correlations to Plot."))

  if(abscor) {
    object <- dplyr::mutate(object, cors = abs(cors))
  }
  
  object <- 
    dplyr::select(
      dplyr::filter(
        dplyr::mutate(
          tidyr::unite(
            object,
            trait, dataset, trait, sep = ": "),
          trait = stats::reorder(trait, dplyr::desc(absmax))),
        absmax >= mincor),
    -absmax)

  if(!nrow(object))
    return(plot_null(paste("No Correlations above", mincor)))
  
  p <- ggplot2::ggplot(object) +
    ggplot2::aes(trait, cors, col = proband) +
    ggplot2::geom_point(size = 2) + 
    ggplot2::facet_grid(probandset + proband ~ .) +
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
