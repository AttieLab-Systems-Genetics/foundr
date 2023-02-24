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
#' ggplot_traitData(sampleData)
#' 
ggplot_traitData <- function(object,
                             ...) {
  
  if("condition" %in% names(object)) {
    if(all(is.na(object$condition)))
      object$condition <- NULL
  }
  if(!("condition" %in% names(object)))
    return(ggplot_onetrait(object, ...))
  
  # Find all condition groupings (including NA)
  object <- left_join(
    object,
    object %>%
      dplyr::group_by(datatype, trait) %>%
      dplyr::summarize(
        condgroup = paste(unique(condition), collapse = ";")) %>%
      dplyr::ungroup(),
    by = c("datatype", "trait"))

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
  # Allow for datatype grouping for traits
  if("datatype" %in% names(object)) {
    tmp <- dplyr::distinct(object, datatype, trait)
    datatype <- tmp$datatype
    trait <- tmp$trait
    ltrait <- length(trait)
    form <- "datatype + trait ~"
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

ggplot_otraitData <- function(object,
                       facet_strain = FALSE,
                       shape_sex = FALSE,
                       boxplot = FALSE,
                       horizontal = FALSE,
                       ...) {
  
  # Allow for datatype grouping for traits
  if("datatype" %in% names(object)) {
    tmp <- dplyr::distinct(object, datatype, trait)
    datatype <- tmp$datatype
    trait <- tmp$trait
    ltrait <- length(trait)
    form <- "datatype + trait ~"
    title <- paste0("data for trait",
                    ifelse(ltrait > 1, "s ", " "),
                    paste(datatype,
                          abbreviate(trait, ceiling(60 / ltrait)),
                          sep = ":", collapse = ", "))
    
  } else {
    trait <- unique(object$trait)
    ltrait <- length(trait)
    form <- "trait ~"
    title <- paste0("data for trait",
                    ifelse(ltrait > 1, "s ", " "),
                    paste(abbreviate(trait, ceiling(60 / ltrait)),
                          collapse = ", "))
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
        ggplot2::aes(value, .data[[condition]], fill = .data[[condition]])
    } else {
      p <- p +
        ggplot2::aes(.data[[condition]], value, fill = .data[[condition]])
    }
    
    p <- p +
      ggplot2::facet_grid(form, scales = "free_y") +
      ggplot2::scale_fill_manual(values = cond_colors)
  } else {
    
    if(horizontal) {
      p <- p +
        ggplot2::aes(value, strain, fill = strain) +
        ggplot2::xlab(ifelse(ltrait == 1, trait, "Trait Value"))
    } else {
      p <- p +
        ggplot2::aes(strain, value, fill = strain) +
        ggplot2::ylab(ifelse(ltrait == 1, trait, "Trait Value"))
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
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1)) +
    ggplot2::ggtitle(title)
}
#' @export
#' @rdname ggplot_traitData
strainplot <- function(object, ...) {
  strainplot(object, ...)
}
#' @export
#' @rdname ggplot_traitData
#' @method autoplot traitData
autoplot.traitData <- function(object, ...) {
  ggplot_traitData(object, ...)
}
#' @export
#' @rdname ggplot_traitData
#' @method plot traitData
plot.traitData <- function(object, ...) {
  ggplot_traitData(object, ...)
}
