#' Contrasts of Conditions
#'
#' @param traitSignal data frame of signals 
#' @param traitStats data frame of stats
#' @param termname name of term
#' @param rawStats data frame with `rawSD`
#'
#' @return object of class `conditionContrasts`
#' @export
#' @importFrom dplyr filter matches mutate right_join select
#' @importFrom tidyr pivot_wider
#' @importFrom stats reorder
#'
conditionContrasts <- function(traitSignal, traitStats, termname = "signal",
                               rawStats = traitStats) {
  if(is.null(traitSignal) || is.null(traitStats))
    return(NULL)
  
  conditions <- unique(traitSignal$condition)
  conditions <- conditions[!is.na(conditions)]
  
  if("condition" %in% names(traitSignal) && length(conditions) == 2) {
    # Replace `SD` with `rawSD`.
    traitStats <- replace_rawSD(
      # Filter `traitStats` to `termname` and arrange by `p.value`.
      dplyr::select(
        dplyr::arrange(
          dplyr::filter(
            traitStats,
            .data$term == termname),
          .data$p.value),
        -term),
      rawStats)

    # Create contrast of conditions.
    traitSignal <-
      dplyr::select(
        dplyr::mutate(
          # Pivot to put conditions on same row.
          tidyr::pivot_wider(
            # Focus on `cellmean`, drop `signal`.
            dplyr::select(traitSignal, -signal),
            names_from = "condition", values_from = "cellmean"),
          value = .data[[conditions[1]]] - .data[[conditions[2]]]),
        -dplyr::matches(conditions))
  } else {
    return(NULL)
  }
  
  out <- 
    dplyr::select(
      dplyr::mutate(
        # Join Stats to add `SD` and `p.value` columns.
        dplyr::right_join(
          traitSignal,
          traitStats,
          by = c("dataset", "trait")),
        # Standardize by SD to have comparable trait ranges.
        value = .data$value / .data$SD,
        # Reorder levels of trait by `p.value`.
        trait = stats::reorder(.data$trait, -.data$p.value)),
      # Remove `SD` from dataset. 
      -SD)
  
  # Join with `F-M` and `F+M` sex combinations.
  out <- dplyr::bind_rows(
    # Female and Male.
    out,
    # Sex Contrast: (Female - Male) / 2.
    dplyr::ungroup(
      dplyr::summarize(
        dplyr::group_by(out, .data$dataset, .data$trait, .data$strain),
        value = ifelse(dplyr::n() == 2,
                       diff(.data$value, na.rm = TRUE)[1] / 2,
                       NA),
        sex = "F-M",
        p.value = mean(p.value),
        .groups = "drop")),
    # Both Sexes: (Female + Male) / 2.
    dplyr::ungroup(
      dplyr::summarize(
        dplyr::group_by(out, .data$dataset, .data$trait, .data$strain),
        value = ifelse(dplyr::n() == 2,
                       mean(.data$value, na.rm = TRUE),
                       NA),
        sex = "F+M",
        p.value = mean(p.value),
        .groups = "drop")))
  
  sexes <- c("Both Sexes", "Female", "Male", "Sex Contrast")
  names(sexes) <- c("F+M", "F", "M", "F-M")
  
  out <- dplyr::mutate(out, sex = factor(sexes[.data$sex], sexes))
  
  class(out) <- c("conditionContrasts", class(out))
  attr(out, "conditions") <- conditions
  attr(out, "termname") <- termname
  attr(out, "ordername") <- "p.value"
  out
}

# Turn `conditionContrasts` object into a `traitSignal` object.
contrast2signal <- function(contrasts) {
  if(is.null(contrasts))
    return(NULL)
  
  dplyr::mutate(
    dplyr::select(
      dplyr::rename(
        contrasts,
        cellmean = "value"),
      -p.value),
    signal = .data$cellmean)
}

#' GGplot of Contrasts of Conditions
#'
#' @param object object of class `conditionContrasts`
#' @param bysex type of sex from c("F","M","F-M","F+M")
#' @param ... additional parameters for `ggplot_spread_evidence()`
#'
#' @return ggplot object
#' @export
#' @rdname conditionContrasts
#' @importFrom dplyr dense_rank filter group_by mutate summarize ungroup
#' @importFrom ggplot2 aes element_text facet_wrap geom_jitter geom_point
#'             geom_vline ggplot scale_fill_manual theme xlab ylab
#'
ggplot_conditionContrasts <- function(object,
                                      bysex = c("Both Sexes", "Female", "Male", "Sex Contrast"),
                                      ...) {
  conditions <- attr(object, "conditions")
  
  if(is.null(object) || is.null(conditions))
    return(plot_null("no contrast data"))
  
  # Filter by sex, sex contrast, or sex mean.
  bysex <- match.arg(bysex)
  object <- dplyr::select(dplyr::filter(object, .data$sex == bysex), -sex)
  
  # Modify X label to be sex and conditions
  xlab <- bysex
  if(!is.null(conditions)) {
    xlab <- paste(conditions[2], "-",
                  xlab,
                  "+", conditions[1])
  }
  
  attr(object, "axes") <- "strain"
  
  ggplot_evidence_spread(object, xlab, ...)
}
#' Plot method for Contrasts of Conditions
#'
#' @param x object of class `conditionContrasts`
#' @param ... parameters passed to `ggplot_conditionContrasts`
#'
#' @return ggplot object
#' @export
#' @rdname conditionContrasts
#' @method plot conditionContrasts
#'
plot.conditionContrasts <- function(x, ...) {
  ggplot_conditionContrasts(x, ...)  
}

#' Summary method for Contrasts of Condtions
#'
#' @param object object of class `conditionContrasts`
#' @param ntrait number of traits to plot
#' @param sortby sort summary by this variable
#' @param ordername column name to order entries
#' @param ... parameters passed to `ggplot_conditionContrasts`
#'
#' @return data frame
#' @export
#' @importFrom dplyr arrange dense_rank everything filter mutate select
#' @importFrom tidyr pivot_wider
#' @rdname conditionContrasts
#'
summary_conditionContrasts <- function(object, ntrait = 20,
                                       sortby = ordername,
                                       ordername = attr(object, "ordername"),
                                       ...) {
  
  if(ordername == "kME") {
    ford <- function(x) -abs(x)
  } else {
    ford <- function(x) x
  }
  out <- tidyr::pivot_wider(
      dplyr::arrange(
        dplyr::mutate(
          dplyr::filter(
            dplyr::arrange(
              object,
              .data[[ordername]], .data$sex),
            dplyr::dense_rank(ford(.data[[ordername]])) <= ntrait),
          value = signif(.data$value, 4),
          strain = factor(strain, names(foundr::CCcolors))),
        .data$strain),
      names_from = "strain", values_from = "value")
  
  if(ordername %in% c("p.value", "kME"))
    out[[ordername]] <- signif(out[[ordername]], 4)
  
  dplyr::select(
    dplyr::arrange(out, ford(.data[[sortby]])),
    dataset, sex, trait, dplyr::matches(sortby), dplyr::everything())
}

#' Summary method for Contrasts of Conditions
#'
#' @param object object of class `conditionContrasts`
#' @param ntraits number of traits to plot
#' @param ... parameters passed to `ggplot_conditionContrasts`
#'
#' @return data frame
#' @export
#' @rdname conditionContrasts
#' @method summary conditionContrasts
#'
summary.conditionContrasts <- function(object, ...) 
  summary_conditionContrasts(object, ...)

#' Combine method for Contrasts of Conditions
#'
#' @param ... objects of class `conditionContrasts`
#'
#' @return object of class `conditionContrasts`
#' @export
#' @rdname conditionContrasts
#' @method c conditionContrasts
#'
c.conditionContrasts <- function(...) {
  
  out <- as.list(...)
  if(!inherits(out[[1]], "conditionContrasts"))
    return(NULL)
  if(length(out) == 1) {
    return(out[[1]])
  } else {
    for(i in seq(2, length(out)))
      if(!inherits(out[[i]], "conditionContrasts"))
        return(NULL)
  }
  
  classout <- class(out[[1]])
  attrout <- list(
    conditions = attr(out[[1]], "conditions"),
    termname = attr(out[[1]], "termname"),
    ordername = attr(out[[1]], "ordername"))
  
  out <- dplyr::bind_rows(out)
  class(out) <- classout

  attr(out, "conditions") <- attrout$conditions
  attr(out, "termname") <- attrout$termname
  attr(out, "ordername") <- attrout$ordername
  out
}

#' Split method for Contrasts of Conditions
#'
#' @param x objects of class `conditionContrasts`
#' @param ... additional parameters for generic `split`
#'
#' @return object of class `conditionContrasts`
#' @export
#' @rdname conditionContrasts
#' @method split conditionContrasts
#'
split.conditionContrasts <- function(x, ...) {
  
  classout <- class(x)
  attrout <- list(
    conditions = attr(x, "conditions"),
    termname = attr(x, "termname"),
    ordername = attr(x, "ordername"))
  
  out <- NextMethod(x, ...)
  for(i in names(out)) {
    class(out[[i]]) <- classout
    
    attr(out[[i]], "conditions") <- attrout$conditions
    attr(out[[i]], "termname") <- attrout$termname
    attr(out[[i]], "ordername") <- attrout$ordername
  }
  out
}