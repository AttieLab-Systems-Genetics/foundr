#' Extract Modules and kMEs
#'
#' @param object object of class `listof_wgcnaModules`
#'
#' @return object of class `module_kMEs`
#' @export
#' @importFrom purrr transpose
#' @importFrom dplyr as_tibble bind_cols everything full_join select
#'
module_kMEs <- function(object) {
  if(is.null(object))
    return(NULL)
  
  object <- transpose(object)
  if(is.null(object))
    return(NULL)
  
  # Transpose to get module and kME with respect to responses.
  mods <- lapply(
    purrr::transpose(
      object$modules),
    dplyr::as_tibble)

  if(is_tree <- !is.null(object$geneTree)) {
    # Transpose to get order from module tree.
    ords <- 
      dplyr::as_tibble(
        purrr::transpose(
          object$geneTree)$order)
    names(ords) <- paste0(names(ords), "_ord")
  }

  # Add column for trait names.
  mods$module$trait <- mods$trait[[1]]
  mods$kME$trait <- mods$trait[[1]]

  out <- dplyr::full_join(
    mods$module,
    mods$kME,
    by = "trait",
    suffix = c("_col","_kME"))
  
  if(is_tree) {
    out <- dplyr::bind_cols(out, ords)
  }
  
  out <- dplyr::select(
    out,
    trait, dplyr::everything())
  
  class(out) <- c("module_kMEs", class(out))
  out
}
#' GGplot of Module kMEs
#'
#' @param object object of class `module_kMEs`
#' @param facetname name of facet response
#' @param colorname name of color response
#' @param abskME plot absolute values if `TRUE`
#' @param title title of plot
#' @param ... additional parameters
#'
#' @return ggplot object 
#' @export
#' @importFrom ggplot2 aes autoplot element_text facet_wrap geom_abline
#'             geom_point ggplot ggtitle scale_color_manual theme
#' @importFrom dplyr arrange count desc
#' @importFrom rlang .data
#' @rdname module_kMEs
#'
ggplot_module_kMEs <- function(object, facetname, colorname, 
                               abskME = FALSE,
                               title = paste("facet by", facetname, "with", colorname, "color"),
                               ...) {

  object <- subset(object, facetname, colorname, ...)
  if(is.null(object))
    return(plot_null("no module_kMEs object"))

  colorcol <- paste0(colorname, "_col")
  facetcol <- paste0(facetname, "_col")
  colorkME <- paste0(colorname, "_kME")
  facetkME <- paste0(facetname, "_kME")
  
  if(abskME) {
    for(i in c(colorkME, facetkME))
      object[[i]] <- abs(object[[i]])
  }
  
  # Module colors are factors ordered by count.
  modcolors <- levels(object[[colorcol]])
  names(modcolors) <- modcolors
  
  ggplot2::ggplot(object) +
    ggplot2::aes(.data[[colorkME]], .data[[facetkME]], col = .data[[colorcol]],
                 key = trait) +
    ggplot2::geom_abline(slope = 1, intercept = 0, col = "darkgrey") +
    ggplot2::geom_point(shape = 1) +
    ggplot2::scale_color_manual(values = modcolors) +
    ggplot2::facet_wrap(~ .data[[facetcol]]) +
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

#' Subset module and kMEs
#' 
#' @param x 
#' @param facetname name of facet response
#' @param colorname name of color response
#' @param facetmodules names of color modules to keep
#' @param colormodules names of color modules to keep
#' @param ... 
#'
#' @return data frame 
#' 
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr all_of filter mutate select
#' @rdname module_kMEs
#'
subset_module_kMEs <- function(x,
                               facetname,
                               colorname,
                               facetmodules = flev,
                               colormodules = clev,
                               ...) {
  if(is.null(x) || !is.data.frame(x))
    return(NULL)
  
  x <- x[c("trait",
           names(x)[grep(paste(facetname, colorname, sep = "|"), names(x))])]
  
  fcol <- paste0(facetname, "_col")
  ccol <- paste0(colorname, "_col")
  flev <- levels(x[[fcol]])
  clev <- levels(x[[ccol]])
  fkme <- paste0(facetname, "_kME")
  ford <- paste0(facetname, "_ord")
  fsel <- c(fcol, fkme)
  if(is_ord <- (ford %in% names(x)))
    fsel <- c(fsel, ford)
  
  x <- dplyr::select(
    dplyr::filter(
      x,
      .data[[fcol]] %in% facetmodules,
      .data[[ccol]] %in% colormodules),
    trait, dplyr::all_of(fsel), dplyr::everything())
  
  if(is_ord) {
    x <- dplyr::arrange(x, .data[[ford]])
  }
  
  x[[fcol]] <- factor(x[[fcol]], flev[flev %in% x[[fcol]]])
  x[[ccol]] <- factor(x[[ccol]], clev[clev %in% x[[ccol]]])

  x
}
#' @export
#' @rdname module_kMEs
#' @method subset module_kMEs
#'
subset.module_kMEs <- function(x, ...)
  subset_module_kMEs(x, ...)
