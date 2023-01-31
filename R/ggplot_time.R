#' GGplot of object over time
#'
#' @param object 
#'
#' @return object of class ggplot2
#' 
#' @importFrom ggplot2 aes facet_wrap geom_jitter geom_smooth ggplot
#'                     scale_color_manual scale_fill_manual
#' @export
#'
#' @examples
ggplot_time <- function(object) {
  ggplot2::ggplot(object) +
    ggplot2::aes(week, value, fill = strain, color = strain) +
    ggplot2::geom_jitter(shape = 21, size = 2, color = "black") +
    ggplot2::geom_smooth(se = FALSE) +
    ggplot2::facet_wrap(sex ~ condition) +
    ggplot2::scale_color_manual(values = CCcolors) +
    ggplot2::scale_fill_manual(values = CCcolors)
  
}
