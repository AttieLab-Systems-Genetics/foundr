#' GGplot template for other routines
#'
#' @param object,x data frame to be plotted
#' @param facet_strain facet by strain if `TRUE` 
#' @param shape_sex use different shape by sex if `TRUE`
#' @param boxplot overlay boxplot if `TRUE`
#' @param horizontal flip vertical and horizontal axis if `TRUE`
#' @param ... additional parameters
#' @param drop_xlab drop `xlab` except last if `TRUE`
#' @param legend_position show legend if `TRUE`
#'
#' @return object of class ggplot
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats formula
#' @importFrom dplyr distinct group_by mutate select summarize ungroup
#' @importFrom tidyr all_of unite
#' @importFrom rlang .data
#' @importFrom ggplot2 aes element_text facet_grid geom_jitter ggplot ggtitle
#'             scale_fill_manual scale_shape_manual theme xlab ylab
#' @importFrom cowplot plot_grid
#' @export
#'
ggplot_template <- function(object,
                            ...,
                            drop_xlab = FALSE,
                            legend_position = "none") {
  
  if(is.null(object))
    return(plot_null("No Traits to Plot."))
  
  response <- attr(object, "response")

  plots <- purrr::map(object, ggplot_onerow,
                      response = response,
                      legend_position = legend_position, ...)
  
  lplots <- length(plots)
  
  # Remove x label and legend for multiple plots
  if(drop_xlab) {
    for(i in seq_len(lplots - 1)) {
      plots[[i]] <- plots[[i]] +
        ggplot2::xlab("")
      if(legend_position != "none")
        plots[[i]] <- plots[[i]] +
          ggplot2::theme(legend.position = "none")
    }
  }
  
  # Patch plots together by rows
  cowplot::plot_grid(plotlist = plots, nrow = lplots)
}
ggplot_onerow <- function(object,
                            facet_strain = FALSE,
                            shape_sex = FALSE,
                            boxplot = FALSE,
                            horizontal = FALSE,
                            pairplot = attr(object, "pair"),
                            title = "",
                            xname = "value",
                            legend_position = "none",
                            ...) {
  
  # Used for optional lines.
  smooth_method <- attr(object, "smooth_method")
  # Response
  response <- attr(object, "response")
  
  # Allow for dataset grouping for traits
  if(!is.null(pairplot)) {
    form <- ". ~"
  } else {
    if("dataset" %in% names(object)) {
      tmp <- dplyr::distinct(object, .data$dataset, .data$trait)
      dataset <- tmp$dataset
      trait <- tmp$trait
      ltrait <- length(trait)
      form <- "dataset + trait ~"
    } else {
      trait <- unique(object$trait)
      ltrait <- length(trait)
      form <- "trait ~"
    }
  }
  
  # Check if there is a condition column that is not all NA
  condition <- "sex"
  if("condition" %in% names(object)) {
    if(all(is.na(object$condition)))
      object$condition <- NULL
    else {
      condition <- "sex_condition"
      if(!("sex_condition" %in% names(object)))
        object <- tidyr::unite(
          object, 
          sex_condition,
          .data$sex, .data$condition,
          remove = FALSE)
    }
  }
  # Used for plotting Stats.
  if("term" %in% names(object))
    condition <- "term"
  
  # Code for trait pair plot.
  if(!is.null(pairplot))
    object <- parallels(object, ..., pair = pairplot)
  
  # Make sure strain is in proper order
#  object <- dplyr::mutate(
#    object,
#    strain = factor(.data$strain, names(foundr::CCcolors)))
  
  p <- ggplot2::ggplot(object)
  
  if(boxplot) {
    p <- p + ggplot2::geom_boxplot(col = "gray", fill = NA,
                                   outlier.shape = NA)
  }
  
  if(facet_strain) {
    fillname <- condition
    ncond <- sort(unique(object[[condition]]))
    plotcolors <- RColorBrewer::brewer.pal(
      n = max(3, length(ncond)), name = "Dark2")
    names(plotcolors) <- ncond[seq_len(length(ncond))]
    
    form <- stats::formula(paste(form, "strain"))
  } else {
    fillname <- "strain"
    plotcolors <- foundr::CCcolors
    
    form <- stats::formula(paste(form, condition))
  }
  
  if(!is.null(pairplot)) {
    # Code for trait pair plots and time plots.
    p <- strain_lines(object, p, plotcolors, fillname,
                      pair = pairplot, smooth_method = smooth_method,
                      ...)
    # Make -log10(p.value) scale further rescaled.
    if(response == "p.value")
      p <- p + ggplot2::scale_y_log10()
  } else {
    if(horizontal) {
      p <- p +
        ggplot2::aes(.data[[xname]], .data[[fillname]]) +
        ggplot2::xlab("")
    } else {
      p <- p +
        ggplot2::aes(.data[[fillname]], .data[[xname]]) +
        ggplot2::ylab("")
    }
  }
  
  p <- p +
    ggplot2::scale_fill_manual(values = plotcolors) +
    ggplot2::facet_grid(form, scales = "free_y")
  
  if(shape_sex) {
    p <- p +
      ggplot2::geom_jitter(
        ggplot2::aes(
          shape = .data$sex,
          fill = .data[[fillname]]),
        width = 0.25, height = 0,
        size = 3, color = "black", alpha = 0.65) +
      ggplot2::scale_shape_manual(values = c(23, 22))
  } else {
    p <- p +
      ggplot2::geom_jitter(
        ggplot2::aes(
          fill = .data[[fillname]]),
        shape = 21, 
        width = 0.25, height = 0,
        size = 3, color = "black", alpha = 0.65)
  }
  
  if(title != "")
    p <- p + ggplot2::ggtitle(title)
  
  p +
    ggplot2::theme(
      legend.position = legend_position,
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))
}

parallels <- function(
    object,
    line_strain = (response == "value"),
    parallel_lines = TRUE,
    pair = NULL,
    ...) {
  if(parallel_lines & !is.null(pair)) {
    response <- attr(object, "response")
    
    if("sex_condition" %in% names(object)) {
      groupsex <- "sex_condition"
    } else {
      groupsex <- "sex"
    }
    if(line_strain) {
      form <- formula(paste0("`", pair[2], "` ~ `", pair[1],
                             "` + strain * `", groupsex, "`"))
      bys <- c(pair, "strain", groupsex)
    } else {
      form <- formula(paste0("`", pair[2], "` ~ `", pair[1],
                             "` + `", groupsex, "`"))
      bys <- c(pair, groupsex)
    }
    dplyr::left_join(
      object,
      broom::augment(lm(form, object)),
      by = bys)
  } else {
    object
  }
}
strain_lines <- function(
    object,
    p,
    plotcolors = foundr::CCcolors,
    fillname = "strain",
    line_strain = (response == "value"),
    parallel_lines = TRUE,
    smooth_method = attr(object, "smooth_method"),
    pair = NULL,
    span = 0.4,
    ...) {
  
  if(is.null(pair))
    return(p)
  
  if(is.null(smooth_method))
    smooth_method <- "lm"
  
  response <- attr(object, "response")
  
  # Set x and y to the pair of traits.
  p <- p +
    ggplot2::aes(.data[[pair[1]]], .data[[pair[2]]])
  
  if(line_strain) {
    if(parallel_lines) {
      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(
            group = .data[[fillname]], col = .data[[fillname]],
            y = .data$.fitted),
          linewidth = 1) +
        ggplot2::scale_color_manual(values = plotcolors)
    } else {
      p <- p +
        ggplot2::geom_smooth(
          ggplot2::aes(
            group = .data[[fillname]], col = .data[[fillname]]),
          method = smooth_method, se = FALSE, formula = "y ~ x",
          span = span, linewidth = 1) +
        ggplot2::scale_color_manual(values = plotcolors)
    }
  } else {
    if(parallel_lines) {
      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(y = .data$.fitted),
          span = span, linewidth = 1, col = "darkgrey")
      
    } else {
      p <- p +
        ggplot2::geom_smooth(
          method = smooth_method, se = FALSE, formula = "y ~ x",
          span = span, linewidth = 1, col = "darkgrey")
    }
  }
  p
}

