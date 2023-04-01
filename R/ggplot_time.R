#' GGplot of object over time
#'
#' @param object 
#' @param facet_strain facet by strain if `TRUE`
#' @param xlab label for X axis
#' @param facet_time name of column to facet on if not `NULL`
#' @param ... additional parameters 
#'
#' @return object of class ggplot2
#' 
#' @importFrom ggplot2 aes element_text facet_grid geom_jitter geom_smooth ggplot
#'                     scale_color_manual scale_fill_manual theme xlab ylab
#' @importFrom rlang .data
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats formula
#' @export
#'
ggplot_time <- function(object,
                        facet_strain = FALSE,
                        xlab = "time",
                        facet_time = NULL,
                        ...) {
  
  if(is.null(object))
    return(plot_null("no time object to plot"))
  
  object <-
    dplyr::mutate(
      object,
      strain = factor(strain, names(foundr::CCcolors)))
  
  ylab <- unique(object$trait)[1]
  if("dataset" %in% names(object))
    ylab <- paste(unique(object$dataset)[1], ylab, sep = ": ")
  
  p <- ggplot2::ggplot(object) +
    ggplot2::aes(.data$time, .data$value) +
    ggplot2::geom_jitter(shape = 21, size = 2, color = "black", width = .3, height = 0) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))
  
  # Only one time point?
  if(length(unique(object$time)) < 3) {
    p <- p +
      ggplot2::geom_smooth(se = FALSE, method = 'lm', formula = 'y ~ x')
  } else {
    p <- p +
      ggplot2::geom_smooth(se = FALSE, method = 'loess', formula = 'y ~ x')
  }
    
  if(is.null(facet_time) || !is.character(facet_time))
    form <- "sex ~"
  else
    form <- paste(facet_time, "+ sex ~")
  
  if(facet_strain) {
    ncond <- sort(unique(object$condition))
    cond_colors <- RColorBrewer::brewer.pal(
      n = max(3, length(ncond)), name = "Dark2")
    names(cond_colors) <- ncond[seq_len(length(ncond))]
    form <- stats::formula(paste(form, "strain"))
    
    p <- p +
      ggplot2::aes(fill = .data$condition, color = .data$condition) +
      ggplot2::facet_grid(form, scales = "free_y") +
      ggplot2::scale_color_manual(values = cond_colors) +
      ggplot2::scale_fill_manual(values = cond_colors)
  } else {
    form <- stats::formula(paste(form, "condition"))

    p <- p +
      ggplot2::aes(fill = .data$strain, color = .data$strain) +
      ggplot2::facet_grid(form, scales = "free_y") +
      ggplot2::scale_color_manual(values = foundr::CCcolors) +
      ggplot2::scale_fill_manual(values = foundr::CCcolors)
  }
    
  p
}
