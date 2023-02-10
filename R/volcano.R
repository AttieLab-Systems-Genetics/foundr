#' One-sided Volcano Plot
#'
#' @param object data frame with `trait` names, `term` names, `SD` and `p.value`
#' @param termname name of `term` to show
#' @param threshold named vector for `SD` and `p.value`
#' @param interact prepare for interactive if `TRUE`
#'
#' @return
#' @export
#' @importFrom dplyr matches mutate select
#' @importFrom ggplot2 aes geom_hline geom_point geom_vline geom_text ggplot
#'             scale_color_manual theme theme_minimal
#' @importFrom ggrepel geom_text_repel
#' @importFrom rlang .data
#'
#' @examples
volcano <- function(object, termname = terms[1],
                    threshold = c(SD = 1, p = 0.01),
                    interact = FALSE) {
  # See https://biocorecrg.github.io/CRG_RIntroduction/volcano-plots.html
  
  # Prefer one of terms, but could be any p-value term
  terms <- termStats(object)
  uterm <- unique(object$term)
  if(!(termname %in% uterm))
    termname <- uterm[1]
  
  object <-
    dplyr::mutate(
      dplyr::filter(
        object,
        term == termname),
      foldchange = "NO",
      foldchange = ifelse(
        SD > threshold["SD"] & p.value < threshold["p"],
        "UP", foldchange),
      label = ifelse(
        SD > threshold["SD"] & p.value < threshold["p"] &
          (SD > 2 * threshold["SD"] | p.value < threshold["p"] / 10),
        trait, NA))
    
  # Convert directly in the aes()
  p <- ggplot2::ggplot(object) +
    ggplot2::aes(SD, -log10(p.value), col = foldchange, label = label) +
    ggplot2::geom_point() +
    # Add more simple "theme"
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    # Add vertical lines for log2FoldChange thresholds, and one horizontal line for the p-value threshold 
    ggplot2::geom_vline(xintercept = threshold["SD"], col = "red") +
    ggplot2::geom_hline(yintercept = -log10(threshold["p"]), col = "red") +
    # The significantly differentially expressed traits are in the upper quadrats.
    ggplot2::scale_color_manual(values=c(DOWN = "blue", NO = "black", UP = "red"))
  
  if(interact) {
    p + ggplot2::geom_text()
  } else {
    p + ggrepel::geom_text_repel(max.overlaps = 20)
  }
}
