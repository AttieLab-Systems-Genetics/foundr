#' GGplot of evidence vs spread
#'
#' This includes volcano plot and other plots that compare data spread to
#' strength of evidence. The strength of evidence is often a `p.value`
#' (typically plotted as `-log10(p.value)`) but need not be.
#' The spread may be differences of mean values or
#' SDs (square root of MS for model term).
#'  
#' @param object data frame with optional attributes
#' @param xlab replacement horizontal label if not `NULL`
#' @param ntraits number of traits to plot
#' @param ordername column name to order entries
#' @param plottype 
#' @param ... additional parameters ignored
#'
#' @return GG plot object
#' @export
#' @importFrom dplyr filter mutate rename
#' @importFrom ggplot2 aes geom_jitter geom_vline ggplot scale_fill_manual xlab ylab
#'             
ggplot_evidence_spread <- function(object,
                                   xlab = NULL,
                                   plottype = c("dotplot", "volcano", "biplot"),
                                   ordername = attr(object, "ordername"),
                                   ntraits = 20,
                                   ...) {
  
  if(is.null(object))
    return(plot_null("no spread-evidence data"))
  
  termname <- attr(object, "termname")
  if(is.null(termname)) termname <- "signal"
  axes <- attr(object, "axes") # `strain` or `term`
  if(is.null(axes)) axes <- "strain"
  if(is.null(ordername))
    ordername <- "p.value"
  
  plottype <- match.arg(plottype)
  switch(plottype,
         biplot = { # Biplot
           p <- condition_biplot(object, ordername, xlab, ...)
         },
         volcano = { # Volcano Plot
           p <- volcano(
             dplyr::mutate(
               dplyr::rename(object, SD = "value"),
               term = termname),
             "signal", facet = TRUE, traitnames = FALSE,
             ordername = ordername, ...)
         },
         dotplot = { # Plot contrasts of strains by trait.
           # Pick top traits to plot
           
           sord <- -1
           if(ordername == "kME")
             sord <- 1
           
           object <- 
             dplyr::filter(
               dplyr::mutate(
                 object,
                 trait = abbreviate(
                   paste(.data$dataset, .data$trait, sep = ": "), 40),
                 trait = reorder(.data$trait, sord * abs(.data[[ordername]]))),
               .data$trait %in% rev(levels(.data$trait))[seq_len(ntraits)])
           
           textsize <- 12
           p <- ggplot2::ggplot(object) +
             ggplot2::aes(.data$value, .data$trait, fill = .data[[axes]]) +
             ggplot2::geom_vline(xintercept = 0, col = "darkgrey") +
             ggplot2::geom_jitter(height = 0.2, width = 0, color = "black",
                                  size = 3, shape = 21, alpha = 0.9) +
             ggplot2::scale_fill_manual(values = foundr::CCcolors) +
             ggplot2::ylab("")
           
           p <- theme_template(p)
         })
  
  if(plottype != "biplot" & !is.null(xlab)) {
    p <- p + ggplot2::xlab(xlab)
  }
  p
}
