#' Scatter plot by trait values
#'
#' @param data data frame
#' @param x name of first trait
#' @param y name of second trait
#' @param shape_sex use different shape by `sex` if `TRUE`
#' @param line_strain show separate lines by `strain` if `TRUE`
#' @param title title for plot
#'
#' @return
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes facet_grid geom_point geom_smooth ggplot
#'                     ggtitle scale_fill_manual scale_shape_manual
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' scatplot(sampleData, "A", "B")
scatplot <- function(data, x, y,
                     shape_sex = TRUE,
                     line_strain = TRUE,
                     title = paste(x, "vs", y)) {
  p <- ggplot2::ggplot(data) +
    ggplot2::aes(.data[[x]], .data[[y]], fill = strain)
  if(line_strain) {
    p <- p +
      ggplot2::geom_smooth(
        method = "lm", se = FALSE, formula = 'y ~ x',
        aes(group = strain, col = strain))
  } else {
    # Because we specify fill in aes, we need to include it here.
    p <- p +
      ggplot2::geom_smooth(
        method = "lm", se = FALSE, formula = 'y ~ x',
        fill = "darkgrey", col = "darkgrey")
  }
  p <- p +
    ggplot2::scale_fill_manual(values = CCcolors) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(title)
  
  if(shape_sex) {
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(shape = sex), size = 3, color = "black", alpha = 0.65) +
      ggplot2::scale_shape_manual(values = c(23, 22))
  } else {
    p <- p +
      ggplot2::geom_point(size = 3, shape = 21, color = "black", alpha = 0.65)
  }
  
  p
}
