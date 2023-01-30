#' Title
#'
#' @param out1 
#' @param summary_type 
#' @param datsub 
#'
#' @return
#' @export
#'
#' @examples
plot_best <- function(out1, summary_type, datsub) {
  ggplot(out1 %>% filter(type == summary_type)) +
    aes(x, y, fill = strain) +
    geom_smooth(method = "lm", se = FALSE, fill = "white", col = "darkgray", formula = "y ~ x") +
    geom_point(
      ggplot2::aes(shape = sex), size = 3, color = "black", alpha = 0.65) +
    ggplot2::scale_shape_manual(values = c(23, 22)) +
    ggplot2::scale_fill_manual(values = CCcolors) +
    facet_wrap(~ sex + diet) +
    xlab(datsub$traitc) +
    ylab(datsub$traitr) +
    ggtitle(paste(
      summary_type, "for",
      "raw cor", round(datsub$raw, 2),
      ", adj cor", round(datsub$adj, 2),
      ", logpvals", 
      paste(round(c(datsub$pvalr, datsub$pvalc), 2), collapse = ", "))) +
    theme(legend.position = "none")
}
