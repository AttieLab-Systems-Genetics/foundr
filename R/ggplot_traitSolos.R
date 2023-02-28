#' GGplot by strain and condition
#'
#' @param object data frame to be plotted
#' @param facet_strain facet by strain if `TRUE` 
#' @param shape_sex use different shape by sex if `TRUE`
#' @param boxplot overlay boxplot if `TRUE`
#' @param horizontal flip vertical and horizontal axis if `TRUE`
#' @param ... additional parameters (ignored)
#'
#' @return object of class ggplot
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats formula
#' @importFrom dplyr distinct
#' @importFrom ggplot2 aes element_text facet_grid geom_jitter ggplot ggtitle
#'             scale_fill_manual scale_shape_manual theme ylab
#' @importFrom cowplot plot_grid
#' @export
#'
#' @examples
#' ggplot_traitSolos(sampleData)
#' 
ggplot_traitSolos <- function(object,
                              ...) {
  
  if(is.null(object) || !nrow(object))
    return(plot_null("No Traits to Plot."))
  
  if("condition" %in% names(object)) {
    if(all(is.na(object$condition)))
      object$condition <- NULL
  }
  if(!("condition" %in% names(object)))
    return(ggplot_onetrait(object, ...))
  
  if(is.null(object$dataset))
    object$dataset <- "unknown"
  
  # Find all condition groupings (including NA) for plotting.
  object <- left_join(
    object,
    dplyr::ungroup(
      dplyr::summarize(
        dplyr::group_by(
          object,
          dataset, trait),
        condgroup = paste(unique(condition), collapse = ";"))),
    by = c("dataset", "trait"))

  # Split object by condition grouping
  object <- split(
    dplyr::select(
      object,
      -condgroup),
    object$condgroup)

  plots <- purrr::map(object, ggplot_onetrait, ...)
  lplots <- length(plots)
  
  for(i in seq_len(lplots - 1))
    plots[[i]] <- plots[[i]] + ggplot2::xlab("")
  
  # Patch plots together by rows
  cowplot::plot_grid(plotlist = plots, nrow = lplots)
}
ggplot_onetrait <- function(object,
                            facet_strain = FALSE,
                            shape_sex = FALSE,
                            boxplot = FALSE,
                            horizontal = FALSE,
                            ...) {
  # Allow for dataset grouping for traits
  if("dataset" %in% names(object)) {
    tmp <- dplyr::distinct(object, dataset, trait)
    dataset <- tmp$dataset
    trait <- tmp$trait
    ltrait <- length(trait)
    form <- "dataset + trait ~"
  } else {
    trait <- unique(object$trait)
    ltrait <- length(trait)
    form <- "trait ~"
  }
  
  # Check if there is a condition column that is not all NA
  condition <- "sex"
  if("condition" %in% names(object)) {
    if(all(is.na(object$condition)))
      object$condition <- NULL
    else {
      condition <- "sex_condition"
      if(!("sex_condition" %in% names(object)))
        object <- tidyr::unite(object, sex_condition, sex, condition, remove = FALSE)
    }
  }
  
  # Make sure strain is in proper order
  object <- dplyr::mutate(object, strain = factor(strain, names(CCcolors)))
  
  p <- ggplot2::ggplot(object)
  
  if(boxplot) {
    p <- p + ggplot2::geom_boxplot(col = "gray", fill = NA)
  }
  
  if(facet_strain) {
    # The condition could be multiple columns; unite as one.
    if(length(condition) > 1) {
      tmp <- paste(condition, sep = "_")
      object <- tidyr::unite(
        object,
        tmp,
        tidyr::all_of(condition),
        na.rm = TRUE)
      condition <- tmp
    }
    ncond <- sort(unique(object[[condition]]))
    cond_colors <- RColorBrewer::brewer.pal(n = length(ncond), name = "Dark2")
    names(cond_colors) <- ncond
    form <- stats::formula(paste(form, "strain"))
    
    if(horizontal) {
      p <- p +
        ggplot2::aes(value, .data[[condition]], fill = .data[[condition]]) +
        ggplot2::xlab("")
    } else {
      p <- p +
        ggplot2::aes(.data[[condition]], value, fill = .data[[condition]]) +
        ggplot2::ylab("")
    }
    
    p <- p +
      ggplot2::facet_grid(form, scales = "free_y") +
      ggplot2::scale_fill_manual(values = cond_colors)
  } else {
    
    if(horizontal) {
      p <- p +
        ggplot2::aes(value, strain, fill = strain) +
        ggplot2::xlab("")
    } else {
      p <- p +
        ggplot2::aes(strain, value, fill = strain) +
        ggplot2::ylab("")
    }
    
    p <- p +
      ggplot2::scale_fill_manual(values = CCcolors)
    
    form <- stats::formula(paste(form, condition))
    p <- p + ggplot2::facet_grid(form, scales = "free_y")
  }
  
  if(shape_sex) {
    p <- p +
      ggplot2::geom_jitter(aes(shape = sex), size = 3, color = "black", alpha = 0.65) +
      ggplot2::scale_shape_manual(values = c(23, 22))
  } else {
    p <- p +
      ggplot2::geom_jitter(size = 3, shape = 21, color = "black", alpha = 0.65)
  }
  
  p +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))
}
#' @export
#' @rdname ggplot_traitSolos
strainplot <- function(object, ...) {
  strainplot(object, ...)
}
#' @export
#' @rdname ggplot_traitSolos
#' @method autoplot traitSolos
autoplot.traitSolos <- function(object, ...) {
  ggplot_traitSolos(object, ...)
}
#' @export
#' @rdname ggplot_traitSolos
#' @method plot traitSolos
plot.traitSolos <- function(object, ...) {
  ggplot_traitSolos(object, ...)
}
