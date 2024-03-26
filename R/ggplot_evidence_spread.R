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
#' @param plottype type of plot
#' @param ordername column name to order entries
#' @param ntrait number of traits to plot
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
                                   ntrait = 20,
                                   ...) {
  
  if(is.null(object))
    return(plot_null("no evidence-spread data"))
  
  termname <- attr(object, "termname")
  axes <- attr(object, "axes") # `strain` or `term`
  if(is.null(axes)) axes <- "strain"
  if(is.null(termname)) {
    if(axes == "strain")
      termname <- "signal"
    else
      termname <- object$term[1]
  }
  if(is.null(ordername) || ordername == "")
    ordername <- "p.value"
  
  plottype <- match.arg(plottype)
  p <- switch(plottype,
    biplot = { # Biplot
      biplot_evidence(object, ordername, xlab, ...)
    },
    volcano = { # Volcano Plot
      volcano_evidence(object, ordername, termname, ...)
    }, 
    dotplot = { # Dotplot
      dotplot(object, ordername, ntrait, axes)
    })
  
  if(plottype != "biplot" & !is.null(xlab)) {
    p <- p + ggplot2::xlab(xlab)
  }
  p
}
dotplot <- function(object, ordername, ntrait = 20, axes = "strain") {
  # Plot contrasts of strains by trait.
  # Pick top traits to plot
  
  if(is.null(object)) return(NULL)
  
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
      .data$trait %in% rev(levels(.data$trait))[seq_len(ntrait)])

  # Switch between contrasts and stats.
  if(axes == "strain") {
    xval <- "value"
    term_colors <- foundr::CCcolors
  } else {
    xval <- "SD"
    termnames <- unique(object$term)
    term_colors <- RColorBrewer::brewer.pal(n = max(3, length(termnames)),
                                            name = "Dark2")
    names(term_colors)[seq_along(termnames)] <- termnames
  }
  
  textsize <- 12
  p <- ggplot2::ggplot(object) +
    ggplot2::aes(.data[[xval]], .data$trait, fill = .data[[axes]]) +
    ggplot2::geom_vline(xintercept = 0, col = "darkgrey") +
    ggplot2::geom_jitter(height = 0.2, width = 0, color = "black",
                         size = 3, shape = 21, alpha = 0.9) +
    ggplot2::scale_fill_manual(values = term_colors) +
    ggplot2::ylab("")
  
  theme_template(p)
}
ggplot_stat_evidence <- function(object) {
  
  ggplot_evidence_spread(object)
}
