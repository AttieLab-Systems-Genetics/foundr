#' Effect Plots
#'
#' @param object data frame from `strainstats()`
#' @param traitnames `traitnames` to show
#' @param correlated correlated `traitnames` to overlay on boxplot (optional)
#' @param ... additional parameters
#'
#' @return ggplot object
#' @export
#' @importFrom dplyr arrange filter mutate rename
#' @importFrom ggplot2 aes element_text facet_grid geom_boxplot
#'             geom_jitter ggplot scale_size theme
#' @importFrom tidyr pivot_longer unite
#' @importFrom rlang .data
#'
#' @examples
#' sampleStats <- strainstats(sampleData)
#' effectplot(sampleStats, trait_names(sampleStats, "C"))
#' 
effectplot <- function(object, traitnames = NULL,
                       correlated = NULL, ...) {
  
  if(is.null(object))
    return(plot_null("No effect data."))

  object <- tidyr::unite(
    object,
    datatraits,
    .data$dataset, .data$trait,
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
      terms = factor(.data$terms, unique(.data$terms)),
      log10_p = -log10(.data$log10_p)),
    .data$SD:.data$log10_p,
    names_to = "stats",
    values_to = "value")
  
  # Add columns for `model`, `selected` and `sizes`.
  object <- dplyr::arrange(
    dplyr::mutate(
      object,
      model = ifelse(
        .data$terms %in% c("signal", "cellmean", "rest", "noise"),
        "model parts", "model terms"),
      selected = ifelse(
        .data$datatraits %in% traitnames,
        .data$datatraits, NA),
      selected = ifelse(
        .data$datatraits %in% correlated,
        "correlated", .data$datatraits),
      selected = factor(.data$selected, c("correlated", traitnames)),
      sizes = ifelse(.data$datatraits %in% traitnames,
                     1, 0.5)),
    .data$selected)
  
  selected_colors <- rep(c(
    "gray50",
    RColorBrewer::brewer.pal(n = max(3, length(traitnames)),
                             name = "Dark2")),
    length = 1 + length(traitnames))
  names(selected_colors) <- c("correlated", traitnames)
  
  ggplot2::ggplot(object) +
    ggplot2::aes(.data$terms, .data$value,
                 col = .data$selected) +
    # Boxplot for all traits.
    ggplot2::geom_boxplot(color = "black", outlier.size = 0.5, outlier.color = "gray80") +
    # Add jittered points for `selected` traits coming from `correlated` or `corobj`
    ggplot2::geom_jitter(inherit.aes = FALSE,
                         data = subset(object, !is.na(selected)),
                         ggplot2::aes(.data$terms, .data$value,
                                      col = .data$selected,
                                      size = .data$sizes,
                                      stroke = .data$sizes),
                         shape = 1, width = 0.2, height = 0) +
    ggplot2::scale_size(range = c(0.5,2), guide = "none") +
    # Facet on `stats` (type of stats) and `model` (parts and terms)
    ggplot2::facet_grid(.data$stats ~ .data$model, scales = "free") +
    ggplot2::ylab("") +
    ggplot2::scale_color_manual(values = selected_colors, name = "Traits") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))
}
effecthelper <- function(corobj = NULL, mincor = NULL, ...) {
  # Extract datatraits column (`dataset: trait`) from corobj if provided.
  
  if(is.null(corobj) | is.null(mincor))
    return(NULL)
  
  tidyr::unite(
    dplyr::filter(
      corobj,
      absmax >= mincor),
    datatraits,
    .data$dataset, .data$trait,
    sep = ": ")$datatraits
}
