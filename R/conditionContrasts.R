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
    out,
    # Contrast: female - male.
    dplyr::ungroup(
      dplyr::summarize(
        dplyr::group_by(out, .data$dataset, .data$trait, .data$strain),
        value = diff(.data$value, na.rm = TRUE)[1],
        sex = "F-M",
        p.value = mean(p.value),
        .groups = "drop")),
    # Mean: (female + male) / 2.
    dplyr::ungroup(
      dplyr::summarize(
        dplyr::group_by(out, .data$dataset, .data$trait, .data$strain),
        value = mean(.data$value, na.rm = TRUE),
        sex = "F+M",
        p.value = mean(p.value),
        .groups = "drop")))
  
  out <- dplyr::mutate(out, sex = factor(.data$sex, c("F","M","F+M","F-M")))
  
  class(out) <- c("conditionContrasts", class(out))
  attr(out, "conditions") <- conditions
  attr(out, "termname") <- termname
  out
}
#' GGplot of Contrasts of Conditions
#'
#' @param object object of class `conditionContrasts`
#' @param bysex type of sex from c("F","M","F-M","F+M")
#' @param ntraits number of traits (if not volcano)
#' @param volcano volcano plot if `TRUE`
#' @param ... additional parameters for `volcano()`
#'
#' @return ggplot object
#' @export
#' @rdname conditionContrasts
#' @importFrom dplyr dense_rank filter group_by mutate summarize ungroup
#' @importFrom ggplot2 aes element_text facet_wrap geom_jitter geom_point
#'             geom_vline ggplot scale_fill_manual theme xlab ylab
#'
ggplot_conditionContrasts <- function(object, bysex = names(sexes),
                                      ntraits = 20, volcano = FALSE,
                                      ...) {
  conditions <- attr(object, "conditions")
  termname <- attr(object, "termname")
  
  if(is.null(object) || is.null(conditions))
    return(plot_null("no contrast data"))
  
  sexes <- c("Female", "Male", "Sex Contrast", "Both Sexes")
  names(sexes) <- c("F","M","F-M","F+M")
  bysex <- match.arg(bysex)
  
  # Filter by sex, sex contrast, or sex mean.
  object <- dplyr::filter(object, .data$sex == bysex)

  if(volcano) { # Volcano Plot
    p <- volcano(
      dplyr::mutate(
        dplyr::rename(object, SD = "value"),
        term = termname),
      "signal", facet = TRUE, traitnames = FALSE, ...)
  } else { # Plot contrasts of strains by trait.
    # Pick top traits to plot
    object <- 
      dplyr::mutate(
        dplyr::filter(
          object,
          .data$trait %in% rev(levels(object$trait))[seq_len(ntraits)]),
        trait = abbreviate(paste(.data$dataset, .data$trait, sep = ": "), 40))
    
    textsize <- 12
    p <- ggplot2::ggplot(object) +
      ggplot2::aes(.data$value, .data$trait, fill = .data$strain) +
      ggplot2::geom_vline(xintercept = 0, col = "darkgrey") +
      ggplot2::geom_jitter(height = 0.2, width = 0, color = "black",
                           size = 3, shape = 21, alpha = 0.65) +
      ggplot2::scale_fill_manual(values = foundr::CCcolors) +
      ggplot2::ylab("")
    
    p <- theme_template(p)
  }

  # Modify X label to be sex and conditions
  xlab <- sexes[bysex]
  if(!is.null(conditions)) {
    xlab <- paste(conditions[2], "-",
                  xlab,
                  "+", conditions[1])
  }
  p + ggplot2::xlab(xlab)
}
#' Plot method for Contrasts of Condtions
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
#' @param ... parameters passed to `ggplot_conditionContrasts`
#'
#' @return data frame
#' @export
#' @rdname conditionContrasts
#'
summary_conditionContrasts <- function(object, ntrait = 20, ...) {
  sexes <- c("Female", "Male", "Sex Contrast", "Both Sexes")
  names(sexes) <- c("F","M","F-M","F+M")
  
  tidyr::pivot_wider(
      dplyr::mutate(
        dplyr::filter(
          dplyr::arrange(
            object,
            .data$p.value, .data$sex),
          dplyr::dense_rank(.data$p.value) <= ntrait),
        sex = factor(sexes[.data$sex], sexes),
        p.value = signif(.data$p.value, 4),
        value = signif(.data$value, 4),
        strain = factor(strain, names(foundr::CCcolors))),
      names_from = "strain", values_from = "value")
}

#' Summary method for Contrasts of Condtions
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