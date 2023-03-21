plot_null <- function (msg = "no data", size = 10, angle = 0) 
{
  ggplot2::ggplot(data.frame(x = 1, y = 1)) +
    ggplot2::aes(
      .data$x, 
      .data$y,
      label = msg) +
    ggplot2::geom_text(size = size, angle = angle) + 
    ggplot2::theme_void()
}
