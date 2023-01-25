#' GGplot by strain and condition
#'
#' @param datatraits data frame to be plotted
#' @param facet_strain facet by strain if `TRUE` 
#' @param condition name of column with condition
#' @param shape_sex use different shape by sex if `TRUE`
#' @param boxplot overlay boxplot if `TRUE`
#' @param horizontal flip vertical and horizontal axis if `TRUE`
#'
#' @return object of class ggplot
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats formula
#' @importFrom dplyr distinct
#' @importFrom ggplot2 aes element_text facet_grid geom_jitter ggplot ggtitle
#'             scale_fill_manual scale_shape_manual theme ylab
#' @export
#'
#' @examples
strainplot <- function(datatraits,
                               facet_strain = FALSE,
                               condition = "",
                               shape_sex = FALSE,
                               boxplot = FALSE,
                               horizontal = FALSE) {
  
  # Allow for datatype grouping for traits
  if("datatype" %in% names(datatraits)) {
    tmp <- dplyr::distinct(datatraits, datatype, trait)
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
    trait <- unique(datatraits$trait)
    ltrait <- length(trait)
    form <- "trait ~"
    title <- paste0("data for trait",
                    ifelse(ltrait > 1, "s ", " "),
                    paste(abbreviate(trait, ceiling(60 / ltrait)),
                          collapse = ", "))
  }
  
  # Could have no condition.
  if(nocond <- !all(condition %in% names(datatraits))) {
    facet_strain <- FALSE
  }

  p <- ggplot2::ggplot(datatraits)
  
  if(boxplot) {
    p <- p + ggplot2::geom_boxplot(col = "gray", fill = NA)
  }
  
  if(facet_strain) {
    # The condition could be multiple columns; unite as one.
    if(length(condition) > 1) {
      tmp <- paste(condition, sep = "_")
      datatraits <- tidyr::unite(
        datatraits,
        tmp,
        tidyr::all_of(condition),
        na.rm = TRUE)
      condition <- tmp
    }
    ncond <- sort(unique(datatraits[[condition]]))
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

    if(!nocond) {
      form <- stats::formula(paste(form, condition))
      p <- p + ggplot2::facet_grid(form, scales = "free_y")
    }
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
