#' One-sided Volcano Plot
#'
#' @param object data frame from `strainstats`
#' @param termname name of `term` to show
#' @param threshold named vector for `SD` and `p.value`
#' @param interact prepare for interactive if `TRUE`
#' @param traitnames include trait names if `TRUE`
#' @param facet facet on `strain` if `TRUE`
#' @param ordername name of column for Y ordering.
#' @param xlab,ylab axis labels
#' @param ... additional parameters ignored
#'
#' @return ggplot object
#' @export
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 aes element_text geom_hline geom_point geom_vline
#'             geom_text ggplot scale_color_manual theme theme_minimal xlab ylab
#' @importFrom ggrepel geom_text_repel
#' @importFrom rlang .data
#'
#' @examples
#' sampleStats <- strainstats(sampleData)
#' volcano(sampleStats)
volcano <- function(object,
                    termname = terms[1],
                    threshold = threshold_default,
                    interact = FALSE,
                    traitnames = TRUE,
                    facet = FALSE,
                    ordername = c("p.value", "kME", "module"),
                    xlab = xlab_default, ylab = ylab_default, ...) {
  # See https://biocorecrg.github.io/CRG_RIntroduction/volcano-plots.html
  
  if(is.null(object))
    return(NULL)
  
  ordername <- match.arg(ordername)
  if(!(ordername %in% names(object)))
    return(NULL)
  
  # Allow some flexibility in threshold setting.
  threshold_default <- c(SD = 1, p.value = 0.01, kME = 0.5, module = 10)
  nth <- names(threshold)
  if(is.null(nth)) {
    if(length(threshold) > length(threshold_default)) {
      # Only take lead values.
      threshold <- threshold[seq_along(threshold_default)]
    }
    # Assume threshold values in order of defaults
    names(threshold) <- names(threshold_default)[seq_along(threshold)]
  }
  nth <- match(names(threshold_default), names(threshold))
  if(any(is.na(nth))) {
    threshold <- c(threshold, threshold_default[is.na(nth)])
  }
  
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
        .data$SD >= threshold["SD"] &
          .data[[ordername]] <= threshold[ordername],
        "UP", foldchange),
      foldchange = ifelse(
        -.data$SD >= threshold["SD"] &
          .data[[ordername]] <= threshold[ordername],
        "DOWN", foldchange),
      label = ifelse(
        abs(.data$SD) >= threshold["SD"] &
          .data[[ordername]] <= threshold[ordername],
        paste(.data$dataset, .data$trait, sep = ": "), NA))
  
  if(any(object$SD < 0))
    SDT <- c(-1,1)
  else
    SDT <- 1
  
  if(interact) {
    object <- dplyr::filter(object, foldchange != "NO")
  }
  
  # Set up Y intercept, ylab
  if(ordername == "p.value") {
    # Transform `p.value` to `-log10(p.value)`.
    yinterceptor <- -log10(threshold["p.value"])
    ylab_default = "-log10(p.value)"
    object <- dplyr::mutate(object, p.value = -log10(p.value))
  } else {
    yinterceptor <- threshold[ordername]
    ylab_default = ordername
  }
  
  # Prettify x label
  xlab_default <- paste("deviations for", termname)
  if(termname == "sex")
    xlab_default <- "Female - deviations + Male"
    
  # Convert directly in the aes()
  CB_colors <- RColorBrewer::brewer.pal(n = 3, name = "Dark2")
  p <- ggplot2::ggplot(object) +
    ggplot2::aes(.data$SD, .data[[ordername]],
                 col = .data$foldchange, label = .data$label) +
    ggplot2::geom_point() +
    # Add more simple "theme"
    ggplot2::theme_minimal() +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    # Add vertical lines for log2FoldChange thresholds, and one horizontal line for the p-value threshold 
    ggplot2::geom_vline(xintercept = SDT * threshold["SD"], col = CB_colors[2]) +
    ggplot2::geom_hline(yintercept = yinterceptor, col = CB_colors[2]) +
    # The significantly differentially expressed traits are in the upper quadrats.
    ggplot2::scale_color_manual(values=c(DOWN = CB_colors[1], NO = CB_colors[3], UP = CB_colors[2]))
  
  p <- theme_template(p, "none")
  
  if(facet && "strain" %in% names(object)) {
    p <- p + ggplot2::facet_wrap(~ .data$strain)
  }
  
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
