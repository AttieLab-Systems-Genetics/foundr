#' GGplot of WGCNA Modules
#'
#' @param object object of class `wgcnaModules`
#' @param response response for dendrogram and primary color band
#' @param main title of plot
#' @param ... additional parameters
#'
#' @return ggplot2 object
#' @importFrom ggdendro ggdendrogram
#' @importFrom ggplot2 autoplot ggtitle ylim
#' @importFrom cowplot plot_grid
#' 
#' @export
#'
ggplot_wgcnaModules <- function(object,
                                response = names(object),
                                main = paste("Dendrogram for", response, "with module colors"),
                                ...) {
  response <- match.arg(response)
                                  
  p <- list()
  
  miny <- min(object[[response]]$dendro$height)
  p[[1]] <- 
    cowplot::plot_grid(
      plot_null("Height = Dissimilarity", angle = 90),
      ggdendro::ggdendrogram(object[[response]]$dendro, labels = FALSE, ...) +
        ggplot2::ylim(c(miny, 1)) +
        ggplot2::ggtitle(main), # This causes message; how to pass as arg?
      rel_widths = c(1,12))
  
  modband <- module_band(object, response)
  
  p[[2]] <- ggplot_module_band(modband)
  
  cowplot::plot_grid(plotlist = p, nrow = length(p))
}
#' Autoplot of wgcnaModules
#'
#' @param object 
#' @param ... 
#'
#' @return ggplot2 object
#' @rdname ggplot_wgcnaModules
#' @method autoplot wgcnaModules
#' @export
#'
autoplot.wgcnaModules <- function(object, ...) {
  ggplot_wgcnaModules(object, ...)
}
#' GGplot of List of WGCNA Modules
#'
#' @param object object of class `wgcnaModules`
#' @param response response for dendrogram and primary color band
#' @param main title of plot
#' @param ... additional parameters
#'
#' @return ggplot2 object
#' @importFrom ggdendro ggdendrogram
#' @importFrom ggplot2 autoplot element_text ggtitle theme ylim
#' @importFrom cowplot plot_grid
#' 
#' @export
#' @rdname ggplot_wgcnaModules
#'
ggplot_listof_wgcnaModules <- function(object,
                                response = names(object),
                                main = paste("Dendrogram for", response, "with module colors"),
                                ...) {
  
  if(is.null(object))
    return(plot_null())
  
  response <- match.arg(response)
  
  p <- list()
  
  miny <- min(object[[response]]$dendro$height)
  p[[1]] <- 
    cowplot::plot_grid(
      plot_null("Height = Dissimilarity", size = 3, angle = 90),
      ggdendro::ggdendrogram(object[[response]]$dendro, labels = FALSE, ...) +
        ggplot2::ylim(c(miny, 1)) +
        ggplot2::ggtitle(main) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)),
      rel_widths = c(1,15))

  modband <- module_band(object, response)
  
  p[[2]] <- ggplot_module_band(modband)
  
  cowplot::plot_grid(plotlist = p, nrow = length(p))
}
#' Autoplot of wgcnaModules
#'
#' @param object 
#' @param ... 
#'
#' @return ggplot2 object
#' @rdname ggplot_wgcnaModules
#' @method autoplot listof_wgcnaModules
#' @export
#'
autoplot.listof_wgcnaModules <- function(object, ...) {
  ggplot_listof_wgcnaModules(object, ...)
}
