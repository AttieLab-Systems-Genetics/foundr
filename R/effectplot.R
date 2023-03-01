#' Effect Plots
#'
#' @param object data frame with `trait` names, `term` names, `SD` and `p.value`
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
        dplyr::filter(
          object,
          datatraits %in% traitnames),
        terms = "term",
        log10pvalue = "p.value"),
      terms = factor(terms, unique(terms)),
      log10pvalue = -log10(log10pvalue)),
    SD:log10pvalue,
    names_to = "stats",
    values_to = "value")
  
  ggplot2::ggplot(object) +
    ggplot2::aes(terms, value) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_grid(stats ~ dataset + trait, scales = "free") +
    ggplot2::ylab("") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))
}
