#' One-sided Volcano Plot
#'
#' @param object data frame from `strainstats`
#' @param termname name of `term` to show
#' @param threshold named vector for `SD` and `p.value`
#' @param interact prepare for interactive if `TRUE`
#' @param traitnames include trait names if `TRUE`
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
#' sampleStats <- strainstats(sampleData)
#' volcano(sampleStats)
volcano <- function(object,
                    termname = terms[1],
                    threshold = c(SD = 1, p = 0.01),
                    interact = FALSE,
                    traitnames = TRUE) {
  # See https://biocorecrg.github.io/CRG_RIntroduction/volcano-plots.html
  
  CB_colors <- RColorBrewer::brewer.pal(n = 3, name = "Dark2")
  
  if(!("term" %in% names(object))) {
    object <- strainstats(object)
  }
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
      foldchange = ifelse(
        -SD > threshold["SD"] & p.value < threshold["p"],
        "DOWN", foldchange),
      label = ifelse(
        (abs(SD) > threshold["SD"] & p.value < threshold["p"]) |
          (abs(SD) > 2 * threshold["SD"] | p.value < threshold["p"] / 10),
        paste(dataset, trait, sep = ": "), NA))
  
  if(any(object$foldchange == "DOWN"))
    SDT <- c(-1,1)
  else
    SDT <- 1
  
  # Prettify x label
  xlab <- paste("deviations for", termname)
  if(termname == "sex")
    xlab <- "Female - deviations + Male"
    
  # Convert directly in the aes()
  p <- ggplot2::ggplot(object) +
    ggplot2::aes(SD, -log10(p.value), col = foldchange, label = label) +
    ggplot2::geom_point() +
    # Add more simple "theme"
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab(xlab) +
    # Add vertical lines for log2FoldChange thresholds, and one horizontal line for the p-value threshold 
    ggplot2::geom_vline(xintercept = SDT * threshold["SD"], col = CB_colors[2]) +
    ggplot2::geom_hline(yintercept = -log10(threshold["p"]), col = CB_colors[2]) +
    # The significantly differentially expressed traits are in the upper quadrats.
    ggplot2::scale_color_manual(values=c(DOWN = CB_colors[1], NO = CB_colors[3], UP = CB_colors[2]))
  
  if(traitnames) {
    if(interact) {
      p + ggplot2::geom_text()
    } else {
      p + ggrepel::geom_text_repel(max.overlaps = 20)
    }
  } else {
    p
  }
}
