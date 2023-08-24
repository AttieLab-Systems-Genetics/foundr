strain_diff <- function(traitSignal, traitStats, termname = "signal") {
  # Arrange stats by `p.value` for `signal`.
  traitStats <- 
    dplyr::select(
      dplyr::filter(
        traitStats,
        .data$term == termname),
      -term)
  if("SD" %in% names(traitStats)) traitStats$SD <- NULL
  
  conditions <- unique(traitSignal$condition)
  conditions <- conditions[!is.na(conditions)]
  
  if("condition" %in% names(traitSignal) && length(conditions) == 2) {
    # Create difference of conditions.
    traitSignal <-
      dplyr::select(
        dplyr::mutate(
          tidyr::pivot_wider(
            dplyr::select(traitSignal, -signal),
            names_from = "condition", values_from = "cellmean"),
          dif = .data[[conditions[1]]] - .data[[conditions[2]]]),
        -dplyr::matches(conditions))
  } else {
    return(NULL)
  }
  
  out <- dplyr::mutate(
    dplyr::right_join(
      traitSignal,
      traitStats,
      by = c("dataset", "trait")),
    trait = stats::reorder(trait, -p.value))
  
  attr(out, "conditions") <- conditions
  out
}
ggplot_strain_diff <- function(object, bysex = TRUE, ntrait = 20) {
  conditions <- attr(object, "conditions")
  
  if(is.null(object) || is.null(conditions))
    return(plot_null("no difference data"))
  
  # Pick top traits to plot
  object <- dplyr::filter(
    object,
    trait %in% rev(levels(object$trait))[seq_len(ntrait)])
  
  # Standardize by dataset, trait to have range [0,1].
  if(bysex) {
    object <- dplyr::ungroup(
      dplyr::mutate(
        dplyr::group_by(object, .data$dataset, .data$trait),
        strain = .data$strain,
        sex = .data$sex,
        dif = .data$dif / sd(.data$dif, na.rm = TRUE)))
  } else {
    object <- dplyr::ungroup(
      dplyr::mutate(
        dplyr::group_by(
          dplyr::ungroup(
            dplyr::summarize(
              dplyr::group_by(object, .data$dataset, .data$trait, .data$strain),
              dif = mean(.data$dif, na.rm = TRUE),
              .groups = "drop")),
          .data$dataset, .data$trait),
        strain = .data$strain,
        dif = .data$dif / sd(.data$dif, na.rm = TRUE)))
  }
  p <- ggplot2::ggplot(object) +
    ggplot2::aes(.data$dif, .data$trait, fill = .data$strain) +
    ggplot2::geom_vline(xintercept = 0, col = "darkgrey") +
    ggplot2::geom_jitter(height = 0.2, width = 0, color = "black",
                         size = 3, shape = 21, alpha = 0.65) +
    ggplot2::scale_fill_manual(values = foundr::CCcolors) +
    ggplot2::theme(legend.position = "right",
                   legend.text = ggplot2::element_text(size = 12),
                   axis.text = ggplot2::element_text(size = 12))
  if(bysex) {
    p <- p + ggplot2::facet_wrap(~ .data$sex)
  }
  if(!is.null(conditions)) {
    p <- p + ggplot2::xlab(paste(conditions, collapse = " - "))
  }
  p
}