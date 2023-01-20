#' GGplot by strain and condition
#'
#' @param datatraits data frame to be plotted
#' @param facet_strain facet by strain if `TRUE` 
#' @param condition name of column with condition
#'
#' @return object of class ggplot
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats formula
#' @importFrom dplyr distinct
#' @importFrom ggplot2 aes element_text facet_grid geom_jitter ggplot ggtitle scale_fill_manual theme ylab
#' @export
#'
#' @examples
ggplot_strain_cond <- function(datatraits,
                               facet_strain = FALSE,
                               condition = "sex_diet") {
  if("datatype" %in% names(datatraits)) {
    tmp <- dplyr::distinct(datatraits, datatype, trait)
    datatype <- tmp$datatype
    trait <- tmp$trait
    ltrait <- length(trait)
    form <- "datatype + trait ~"
    title <- paste0(paste(datatype, collapse = ","),
                    " data for trait",
                    ifelse(ltrait > 1, "s ", " "),
                    paste(abbreviate(trait, ceiling(60 / ltrait)),
                          collapse = ", "))
    
  } else {
    trait <- unique(datatraits$trait)
    ltrait <- length(trait)
    form <- "trait ~"
    title <- paste0("data for trait",
                    ifelse(ltrait > 1, "s ", " "),
                    paste(abbreviate(trait, ceiling(60 / ltrait)),
                          collapse = ", "))
  } 

  p <- ggplot2::ggplot(datatraits)
  
  if(facet_strain) {
    ncond <- sort(unique(datatraits[[condition]]))
    cond_colors <- RColorBrewer::brewer.pal(n = length(ncond), name = "Dark2")
    names(cond_colors) <- ncond
    form <- stats::formula(paste(form, "strain"))
    
    p <- p +
      ggplot2::aes(.data[[condition]], value, fill = .data[[condition]]) +
      ggplot2::geom_jitter(size = 3, shape = 21, color = "black", alpha = 0.65) +
      ggplot2::facet_grid(form, scales = "free_y") +
      ggplot2::scale_fill_manual(values = cond_colors)
  } else {
    form <- stats::formula(paste(form, condition))
    
    p <- p +
      ggplot2::aes(strain, value, fill = strain) +
      ggplot2::geom_jitter(size = 3, shape = 21, color = "black", alpha = 0.65) +
      ggplot2::facet_grid(form, scales = "free_y") +
      ggplot2::scale_fill_manual(values = CCcolors)
  }
  
  p +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1)) +
    ggplot2::ylab(ifelse(ltrait == 1, trait, "Trait Value")) +
    ggplot2::ggtitle(title)
}
