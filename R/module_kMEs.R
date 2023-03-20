#' Extract Modules and kMEs
#'
#' @param object object of class `listof_wgcnaModules`
#'
#' @return object of class `module_kMEs`
#' @export
#' @importFrom purrr transpose
#' @importFrom dplyr as_tibble full_join
#'
module_kMEs <- function(object) {
  # Transpose twice to get module and kME with respect to responses.
  mods <- lapply(
    purrr::transpose(
      purrr::transpose(object)$modules),
    dplyr::as_tibble)
  
  # Add column for trait names.
  mods$module$trait <- mods$trait$individual
  mods$kME$trait <- mods$trait$individual
  
  out <- dplyr::full_join(
    mods$module,
    mods$kME,
    by = "trait",
    suffix = c("_col","_kME"))
  class(out) <- c("module_kMEs", class(out))
  out
}
#' GGplot of Module kMEs
#'
#' @param object object of class `module_kMEs`
#' @param x name of x response
#' @param y name of y response
#' @param abskME plot absolute values if `TRUE`
#' @param title title of plot
#' @param ... additional parameters
#'
#' @return ggplot object 
#' @export
#' @importFrom ggplot2 aes autoplot facet_wrap geom_abline geom_point ggplot ggtitle
#'             scale_color_manual element_text
#' @importFrom dplyr arrange count desc
#' @importFrom rlang .data
#' @rdname module_kMEs
#'
ggplot_module_kMEs <- function(object, x, y,
                               abskME = FALSE,
                               title = paste("facet by", y, "with color by", x),
                               ...) {
  xcol <- paste0(x, "_col")
  ycol <- paste0(y, "_col")
  xkME <- paste0(x, "_kME")
  ykME <- paste0(y, "_kME")
  
  if(abskME) {
    for(i in c(xkME, ykME))
      object[[i]] <- abs(object[[i]])
  }
  
  # Module colors are factors ordered by count.
  modcolors <- levels(object[[xcol]])
  names(modcolors) <- modcolors
  
  ggplot2::ggplot(object) +
    ggplot2::aes(.data[[xkME]], .data[[ykME]], col = .data[[xcol]],
                 trait = trait) +
    ggplot2::geom_abline(slope = 1, intercept = 0, col = "darkgrey") +
    ggplot2::geom_point(shape = 1) +
    ggplot2::scale_color_manual(values = modcolors) +
    ggplot2::facet_wrap(~ .data[[ycol]]) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  
}

module_factors <- function(object, module) {
  modcolors <- dplyr::arrange(
    dplyr::count(
      object,
      .data[[module]]),
    dplyr::desc(n))[[module]]
  
  object[[module]] <- factor(object[[module]], modcolors)
  object
}

#' @rdname module_kMEs
#' @export
#' @method autoplot module_kMEs
autoplot.module_kMEs <- function(object, ...) {
  ggplot_module_kMEs(object, ...)
}