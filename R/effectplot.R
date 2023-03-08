#' Effect Plots
#'
#' @param object data frame from `strainstats()`
#' @param traitnames `traitnames` to show
#' @param correlated correlated `traitnames` to overlay on boxplot
#' @param ... additional parameters
#'
#' @return
#' @export
#' @importFrom dplyr filter mutate rename
#' @importFrom ggplot2 aes element_text facet_grid geom_point ggplot theme
#' @importFrom tidyr pivot_longer
#'
#' @examples
effectplot <- function(object, traitnames = NULL,
                       correlated = NULL, ...) {
  
  if(is.null(object))
    return(plot_null("No effect data."))

  object <- tidyr::unite(
    object,
    datatraits,
    dataset, trait,
    sep = ": ", remove = FALSE)

  if(is.null(traitnames) || !length(traitnames))
    traitnames <- NULL
#    return(plot_null("Need to specify at least one trait."))
  
  if(is.null(correlated)) {
    correlated <- effecthelper(...)
  }
  if(!is.null(correlated)) {
    if(!all(correlated %in% unique(object$datatraits))) {
      # kludge for traits of form condition:trait
      correlated <- stringr::str_replace(
        correlated, ": .*:", ": ")
    }
    if(!all(correlated %in% unique(object$datatraits))) {
      correlated <- NULL
    }
  }
  
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
      model = ifelse(terms %in% c("signal", "cellmean", "rest", "noise"),
                     "model parts", "model terms"),
      selected = ifelse(datatraits %in% traitnames,
                        datatraits, NA),
      selected = ifelse(datatraits %in% correlated,
                        "correlated", datatraits),
      selected = factor(selected, c("correlated", traitnames)),
      sizes = ifelse(datatraits %in% traitnames,
                     1, 0.5)),
    selected)
  
  selected_colors <- rep(c(
    "gray50",
    RColorBrewer::brewer.pal(n = max(3, length(traitnames)),
                             name = "Dark2")),
    length = 1 + length(traitnames))
  names(selected_colors) <- c("correlated", traitnames)
  
  ggplot2::ggplot(object) +
    ggplot2::aes(terms, value,
                 col = selected) +
    ggplot2::geom_boxplot(color = "black", outlier.size = 0.5, outlier.color = "gray80") +
    ggplot2::geom_jitter(inherit.aes = FALSE,
                         data = subset(object, !is.na(selected)),
                         ggplot2::aes(terms, value,
                                      col = selected,
                                      size = sizes,
                                      stroke = sizes),
                         shape = 1, width = 0.2, height = 0) +
    ggplot2::scale_size(range = c(0.5,2), guide = "none") +
    ggplot2::facet_grid(stats ~ model, scales = "free") +
    ggplot2::ylab("") +
    ggplot2::scale_color_manual(values = selected_colors, name = "Traits") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))
}
effecthelper <- function(corobj = NULL, mincor = NULL, ...) {
  # but it is possible trait is compound of condition:trait in some cases.
  # can ignore that to begin.
  
  if(is.null(corobj) | is.null(mincor))
    return(NULL)
  
  tidyr::unite(
    dplyr::filter(
      corobj,
      absmax >= mincor),
    datatraits,
    dataset, trait,
    sep = ": ")$datatraits
}
