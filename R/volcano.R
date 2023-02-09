#' One-sided Volcano Plot
#'
#' @param object data frame with trait names, p-value and sd terms
#' @param term name of term to show
#' @param threshold named vector for `sd` and `p`
#' @param interact prepare for interactive if `TRUE`
#' @param rescaleSD rescale SD by `rawSD` if `TRUE`
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
volcano <- function(object, term = p_terms,
                    threshold = c(sd = 0.6, p = 0.05),
                    interact = FALSE, rescaleSD = TRUE) {
  # See https://biocorecrg.github.io/CRG_RIntroduction/volcano-plots.html
  
  # Prefer one of p_terms, but could be any p-value term
  p_terms <- termStats(object)
  term <- match.arg(term[1], p_terms)
  
  object <-
    dplyr::mutate(
      dplyr::select(
        object,
        dplyr::matches(c("trait", "rawSD", paste0(c("p_", "sd_"), term)))),
      foldchange = "NO",
      foldchange = ifelse(
        .data[[paste0("sd_", term)]] > threshold["sd"] &
          .data[[paste0("p_", term)]] < threshold["p"], "UP", foldchange),
      label = ifelse(
        .data[[paste0("sd_", term)]] > threshold["sd"] &
          .data[[paste0("p_", term)]] < threshold["p"] &
          (.data[[paste0("sd_", term)]] > 2 * threshold["sd"] |
             .data[[paste0("p_", term)]] < threshold["p"] / 10), trait, NA))
  
  if(rescaleSD) {
    object[[paste0("sd_", term)]] <- object[[paste0("sd_", term)]] / object$rawSD
  }

    
  # Convert directly in the aes()
  p <- ggplot2::ggplot(object) +
    ggplot2::aes(.data[[paste0("sd_", term)]],
                 -log10(.data[[paste0("p_", term)]]),
                 col = foldchange, label = label) +
    ggplot2::geom_point() +
    # Add more simple "theme"
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    # Add vertical lines for log2FoldChange thresholds, and one horizontal line for the p-value threshold 
    ggplot2::geom_vline(xintercept = threshold["sd"], col = "red") +
    ggplot2::geom_hline(yintercept = -log10(threshold["p"]), col = "red") +
    # The significantly differentially expressed traits are in the upper quadrats.
    ggplot2::scale_color_manual(values=c(DOWN = "blue", NO = "black", UP = "red"))
  
  if(interact) {
    p + ggplot2::geom_text()
  } else {
    p + ggrepel::geom_text_repel()
  }
}

#' Terms in Stats Object
#'
#' @param object object from `broomit`
#'
#' @return
#' @export
#' @importFrom stringr str_remove
#'
#' @examples
termStats <- function(object) {
  # Return the strain terms with condition if present
  p_terms <- names(object)
  p_terms1 <- p_terms[grepl("p_", p_terms)]
  p_terms1 <- p_terms1[!grepl("p_signal", p_terms1)]
  p_terms1 <- stringr::str_remove(p_terms1, "p_")
  
  if(any(grepl("condition", p_terms)))
    p_terms1[grepl(".*strain.*condition", p_terms1)]
  else
    p_terms1[grepl(".*strain", p_terms1)]
}