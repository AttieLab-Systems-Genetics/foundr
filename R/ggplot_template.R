#' GGplot template for other routines
#'
#' This routine is used by multiple others (`traitSolos`, `traitPairs`, `traitTimes`),
#' providing one place for specialized plotting code to avoid duplication and improve
#' visualization. However, it has some quirks to meet all needs. See details.
#' @param object list of data frames to be plotted
#' @param facet_strain facet by strain if `TRUE` 
#' @param shape_sex use different shape by sex if `TRUE`
#' @param boxplot overlay boxplot if `TRUE`
#' @param ... additional parameters
#' @param drop_xlab drop `xlab` except last if `TRUE`
#' @param legend_position show legend if `TRUE`
#' 
#' @details The template consists of a wrapper (`ggplot_template`) to make multiple
#' calls to create individual ggplot objects (`ggplot_onerow`). The `object` is a
#' list constructed by other routines
#' (`ggpot_traitSolos`, `ggplot_traitPairs`, `ggplot_traitTimes`)
#' in distinct ways. Signals are passed in subtle ways about how the routines are
#' adapted for distinct uses:
#' \itemize{
#' \item{shape_sex}{different shapes by `sex` if `TRUE` (all)}
#' \item{facet_strain}{facet by `strain` if `TRUE` (Solos, Pairs); used in tricky ways for Times}
#' \item{legend_position}{"none" (Solos, Pairs) or "bottom" (Times)}
#' \item{drop_xlab}{drop xlab from all but last ggplot object (Solos, Times)}
#' \item{parallel_lines}{}
#' }
#' Other peculiarities are more deeply embedded in attributes either to the `object`
#' or to the elements of the list in `object.`
#' \itemize{
#' \item{traitnames}{trait names as `dataset: trait` (`object` for all)}
#' \item{response}{"value","cellmean","signal" (Solos,Pairs)}
#' \item{smooth_method}{"lm" (Solos,Pairs), "loess" (timetype == "strain"; Times), "line" (timetype == "stats"; Times)}
#' \item{pair}{pair of traits (Pairs), "time" and trait name (Times)}
#' \item{sep}{separator " ON " (Pairs)}
#' \item{timetype}{"strain","stats" (Times)}
#' \item{time}{"minute","week" (timetype == "strain") or "minute_summary","week_summary" (timetype == "stats") (Times)}
#' }
#' In addition the presence or absence of a column for "condition" affects whether
#' faceting or coloring is based on "sex" or "sex_condition".
#' Typically, parameter `facet_strain` controls facet vs color:
#' if `TRUE`, facet by `strain` and color by `sex` and `condition`;
#' if `FALSE` facet by `sex` and `condition` and color by `strain`;
#' however, if a Times plot with `response` == "value", then color by `strain`.
#'
#' @return object of class ggplot
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats formula
#' @importFrom dplyr distinct group_by mutate select summarize ungroup
#' @importFrom tidyr all_of unite
#' @importFrom rlang .data
#' @importFrom ggplot2 aes element_text facet_grid geom_jitter ggplot ggtitle
#'             guides guide_legend scale_fill_manual scale_shape_manual theme xlab ylab
#' @importFrom cowplot plot_grid
#' @importFrom grid unit
#' @export
#'
ggplot_template <- function(object,
                            ...,
                            drop_xlab = FALSE,
                            legend_position = "none") {
  
  if(is.null(object))
    return(plot_null("No Traits to Plot."))
  
  plots <- purrr::map(object, ggplot_onerow,
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
                            pairplot = attr(object, "pair"),
                            title = "",
                            xname = "value",
                            legend_position = "none",
                          legend_nrow = 3,
                            ...) {
  
  # Used for optional lines.
  smooth_method <- attr(object, "smooth_method")
  # Response
  response <- attr(object, "response")
  timetype <- attr(object, "timetype")

  # Allow for dataset grouping for traits
  gpname <- FALSE
  if(!is.null(pairplot)) {
    # If called from ggplot_traitTimes and timetype == "strain"
    if(gpname <- (pairplot[1] == "time" &&
       timetype == "strain" &&
       response == "value")) {
      if(facet_strain) {
        if("condition" %in% names(object))
          form <- "sex_condition ~"
        else
          form <- "sex ~"
      } else {
        form <- "strain ~"
      }
    } else {
      # If called from ggplot_traitPairs or timetype == "stats
      form <- ". ~"
    }
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
  if(!is.null(pairplot)) {
    object <- parallels(object, ..., pair = pairplot)
  }
  
  # Facet formula.
  if(facet_strain) {
    form <- stats::formula(paste(form, "strain"))
  } else {
    form <- stats::formula(paste(form, condition))
  }
  
  # Plot colors.
  ncond <- unique(object[[condition]])
  if(facet_strain & !gpname) {
    fillname <- condition
    plotcolors <- RColorBrewer::brewer.pal(
      n = max(3, length(ncond)), name = "Dark2")
    names(plotcolors) <- ncond[seq_len(length(ncond))]
  } else {
    fillname <- "strain"
    plotcolors <- foundr::CCcolors
  }
  
  # Group for plotting by animal.
  if(gpname)
    gpname <- "animal"
  else
    gpname <- fillname
  
  # Factor for condition and strain to keep in right order.
  object[[condition]] <- factor(object[[condition]], ncond)
  if("strain" %in% names(object) & condition != "term")
    object$strain <- factor(object$strain, names(foundr::CCcolors))

  p <- ggplot2::ggplot(object)
  
  if(boxplot) {
    p <- p + ggplot2::geom_boxplot(col = "gray", fill = NA,
                                   outlier.shape = NA)
  }

  if(!is.null(pairplot)) {
    # Code for trait pair plots and time plots.
    p <- strain_lines(object, p, plotcolors, fillname, gpname,
                      pair = pairplot, smooth_method = smooth_method,
                      condition = condition,
                      response = response,
                      ...)
    # Make -log10(p.value) scale further rescaled.
    if(response == "p.value")
      p <- p + ggplot2::scale_y_log10()
  } else {
    p <- p +
      ggplot2::aes(.data[[fillname]], .data[[xname]]) +
      ggplot2::ylab("")
  }
  
  p <- p +
    ggplot2::scale_fill_manual(values = plotcolors) +
    ggplot2::facet_grid(form, scales = "free_y")
  
  size <- 3
  if(!is.null(pairplot) && pairplot[1] == "time")
    size <- 1
  if(shape_sex) {
    p <- p +
      ggplot2::geom_jitter(
        ggplot2::aes(
          shape = .data$sex,
          fill = .data[[fillname]]),
        width = 0.25, height = 0,
        size = size, color = "black", alpha = 0.65) +
      ggplot2::scale_shape_manual(values = c(23, 22))
  } else {
    p <- p +
      ggplot2::geom_jitter(
        ggplot2::aes(
          fill = .data[[fillname]]),
        shape = 21, 
        width = 0.25, height = 0,
        size = size, color = "black", alpha = 0.65)
  }
  
  if(title != "")
    p <- p + ggplot2::ggtitle(title)
  
  p <- p +
    ggplot2::theme(
      legend.position = legend_position,
      legend.key.width = grid::unit(1, "strwidth","abcdefgh"),
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))
  if(legend_position == "bottom")
    p <- p + ggplot2::guides(
      color = ggplot2::guide_legend(
        nrow = legend_nrow, byrow = TRUE, title = "", label.position = "top"),
      fill = ggplot2::guide_legend(
        nrow = legend_nrow, byrow = TRUE, title = "", label.position = "top"))

  p
}

parallels <- function(
    object,
    line_strain = (response == "value"),
    parallel_lines = TRUE,
    pair = NULL,
    ...) {
  if(parallel_lines & !is.null(pair)) {
    response <- attr(object, "response")
    if(class(object[[pair[1]]]) %in% c("character","factor"))
      inter <- "*"
    else
      inter <- "+"
    
    if("sex_condition" %in% names(object)) {
      groupsex <- "sex_condition"
    } else {
      groupsex <- "sex"
    }
    if(line_strain) {
      form <- formula(paste0("`", pair[2], "` ~ `", pair[1],
                             "` ", inter, " strain * `", groupsex, "`"))
      bys <- c(pair, "strain", groupsex)
    } else {
      form <- formula(paste0("`", pair[2], "` ~ `", pair[1],
                             "` ", inter, " `", groupsex, "`"))
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
    gpname = fillname,
    line_strain = (response == "value"),
    parallel_lines = TRUE,
    smooth_method = attr(object, "smooth_method"),
    pair = NULL,
    span = 0.4,
    condition = "sex",
    response = "value",
    ...) {
  
  if(is.null(pair))
    return(p)
  
  if(is.null(smooth_method))
    smooth_method <- "lm"
#  if(length(unique(object[[pair[1]]])) < 4 &
  if(smooth_method == "loess")
    smooth_method <- "line"
  
  # Set x and y to the pair of traits.
  if(smooth_method == "loess")
    p <- p + ggplot2::aes(jitter(.data[[pair[1]]], 0.2), .data[[pair[2]]]) +
    ggplot2::xlab(pair[1])
  else
    p <- p + ggplot2::aes(.data[[pair[1]]], .data[[pair[2]]])
  
  if(line_strain) {
    # Always include overall regression line.
    p <- p +
      ggplot2::geom_smooth(
        method = "lm", se = FALSE, formula = "y ~ x",
        linewidth = 2, col = "darkgrey")
    
    if(parallel_lines) {
      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(
            group = .data[[gpname]], col = .data[[fillname]],
            y = .data$.fitted),
          linewidth = 1) +
        ggplot2::scale_color_manual(values = plotcolors)
    } else {
      if(smooth_method == "line") {
        # Used for stats.
        p <- p +
          ggplot2::geom_line(
            ggplot2::aes(
              group = .data[[gpname]], col = .data[[fillname]]),
            linewidth = 1) +
          ggplot2::scale_color_manual(values = plotcolors)
        
      } else {
        p <- p +
          ggplot2::geom_smooth(
            ggplot2::aes(
              group = .data[[gpname]], col = .data[[fillname]]),
            method = smooth_method, se = FALSE, formula = "y ~ x",
            span = span, linewidth = 1) +
          ggplot2::scale_color_manual(values = plotcolors)
      }
    }
  } else {
    if(parallel_lines) {
      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(y = .data$.fitted),
          span = span, linewidth = 1, col = "darkgrey")
      
    } else {
      if(smooth_method == "line") {
        # Not used.
        p <- p +
          ggplot2::geom_line(
            linewidth = 1, col = "darkgrey") +
          ggplot2::scale_color_manual(values = plotcolors)
        
      } else {
        p <- p +
          ggplot2::geom_smooth(
            method = smooth_method, se = FALSE, formula = "y ~ x",
            span = span, linewidth = 1, col = "darkgrey")
      }
    }
  }
  p
}

