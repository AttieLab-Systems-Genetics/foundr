#' GGplot by strain and condition
#'
#' @param object,x data frame to be plotted
#' @param facet_strain facet by strain if `TRUE` 
#' @param shape_sex use different shape by sex if `TRUE`
#' @param boxplot overlay boxplot if `TRUE`
#' @param horizontal flip vertical and horizontal axis if `TRUE`
#' @param ... additional parameters (ignored)
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
#'
#' @export
#' @examples
#' ggplot_traitSolos(traitSolos(sampleData))
#' 
ggplot_traitSolos <- function(object,
                              ...) {
  
  if(is.null(object) || !length(object))
    return(plot_null("No Traits to Plot."))
  
  response <- attr(object, "response")
  
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
          .data$dataset, .data$trait),
        condgroup = paste(unique(.data$condition), collapse = ";"))),
    by = c("dataset", "trait"))

  # Split object by condition grouping
  object <- split(
    dplyr::select(
      object,
      -.data$condgroup),
    object$condgroup)
  
  attr(object, "response") <- response
  
  ggplot_template(object, ..., drop_xlab = TRUE)
}
#' @export
#' @importFrom ggplot2 autoplot
#' @rdname ggplot_traitSolos
#' @method autoplot traitSolos
autoplot.traitSolos <- function(object, ...) {
  ggplot_traitSolos(object, ...)
}
#' @export
#' @rdname ggplot_traitSolos
#' @method plot traitSolos
plot.traitSolos <- function(x, ...) {
  autoplot.traitSolos(x, ...)
}

