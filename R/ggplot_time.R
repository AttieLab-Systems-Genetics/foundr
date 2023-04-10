#' GGplot of object over time
#'
#' @param object of class `strain_time`
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
  if(is.null(object) || !nrow(object[[1]]))
    return(plot_null("No Time Plots."))
  
  ggplot_template(
    object,
    line_strain = TRUE,
    parallel_lines = FALSE,
    facet_strain = facet_strain,
    xlab = xlab,
    facet_time = facet_time,
    drop_xlab = TRUE,
    ...)
}
