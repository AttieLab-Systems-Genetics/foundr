#' Effect Plots
#'
#' @param object data frame from `strainstats()`
#' @param traitnames `traitnames` to show
#'
#' @return
#' @export
#' @importFrom dplyr filter mutate rename
#' @importFrom ggplot2 aes element_text facet_grid geom_point ggplot theme
#' @importFrom tidyr pivot_longer
#'
#' @examples
effectplot <- function(object, traitnames = udatatraits) {
  
  if(is.null(object))
    return(plot_null("No effect data."))

  object <- tidyr::unite(
    object,
    datatraits,
    dataset, trait,
    sep = ": ", remove = FALSE)
  udatatraits <- unique(object$datatraits)
  
  if(is.null(traitnames) || !length(traitnames))
    return(plot_null("Need to specify at least one trait."))
  
  object <- tidyr::pivot_longer(
    dplyr::mutate(
      dplyr::rename(
        object,
        terms = "term",
        log10_p = "p.value"),
      terms = factor(terms, unique(terms)),
      log10_p = -log10(log10_p)),
    SD:log10_p,
    names_to = "stats",
    values_to = "value")
  
  object <- dplyr::arrange(
    dplyr::mutate(
      object,
      selected = ifelse(datatraits %in% traitnames,
                        datatraits, "other"),
      selected = factor(selected, c("other", traitnames)),
      sizes = ifelse(datatraits %in% traitnames,
                     1, 0.5)),
    selected)
  
  selected_colors <- rep(c(
    "gray",
    RColorBrewer::brewer.pal(n = max(3, length(traitnames)),
                             name = "Dark2")),
    length = 1 + length(traitnames))
  names(selected_colors) <- c("other", traitnames)
  
  ggplot2::ggplot(object) +
    ggplot2::aes(terms, value,
                 col = selected, size = sizes, stroke = sizes) +
    ggplot2::geom_jitter(shape = 1, width = 0.2, height = 0) +
    ggplot2::scale_size(range = c(0.5,2), guide = "none") +
    ggplot2::facet_grid(stats ~ ., scales = "free") +
    ggplot2::ylab("") +
    ggplot2::scale_color_manual(values = selected_colors, name = "Traits") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))
}
