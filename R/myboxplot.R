#' Boxplot overlaid by jittered data
#'
#' @param data data frame
#' @param x title
#'
#' @return
#' @importFrom ggplot2 aes facet_grid geom_boxplot geom_jitter ggplot ggtitle
#' @export
#'
#' @examples
myboxplot <- function(data, x) {
  ggplot2::ggplot(data) +
    ggplot2::aes(Strain, value, color = Strain) +
    ggplot2::facet_grid(Sex ~ Condition) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter() +
    scale_fill_manual(values = CCcolors) +
    ggplot2::ggtitle(x)
}
