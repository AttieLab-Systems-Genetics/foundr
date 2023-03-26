#' Compare correlation matrices using upper triangle
#'
#' @param traitStats data frame with trait summaries
#' @param cor1,cor2 correlation matrices to compare
#' @param ... additional parameters (not used)
#'
#' @return tibble of data frame
#' @export
#' @importFrom dplyr as_tibble filter
#'
cor_compare <- function(traitStats, cor1, cor2, ...) {
  # Typically traitStats will be summary across traits for a particular datatype.
  
  # Pull p-values for signal and organize by rows and columns of cor matrices.
  tmp <- dplyr::filter(
    traitStats,
    term == "signal")$p.value
  pvalr <- matrix(tmp, byrow = TRUE, nrow = length(tmp), ncol = length(tmp))
  pvalr <- -log10(pvalr[upper.tri(pvalr)])
  pvalc <- matrix(tmp, byrow = FALSE, nrow = length(tmp), ncol = length(tmp))
  pvalc <- -log10(pvalc[upper.tri(pvalc)])
  
  
  dplyr::as_tibble(
    data.frame(
      cor1 = cor1[upper.tri(cor1)],
      cor2 = cor2[upper.tri(cor2)],
      logpvalr = pvalr,
      logpvalc = pvalc))
}

#' Extreme correlation comparison summaries
#'
#' @param traitStats data frame with trait summaries
#' @param object object from `cor_compare()`
#' @param ... additional parameters
#' @param cormin minimum correlation to keep
#' @param minlogp minimul log of p-value to keep
#'
#' @return data frame
#' @export
#' @importFrom dplyr across filter mutate rename row_number select
#' @importFrom purrr map transpose
#'
#' @examples
cor_extreme <- function(traitStats,
                        object = cor_compare(traitStats, ...),
                        ...,
                        cormin = 0.8,
                        minlogp = 2) {
  if(is.null(traitStats))
    return(NULL)
  if(is.null(object))
    retrun(NULL)
  
  traits <- as.character(traitStats$trait)
  
  object <- object %>%
    dplyr::mutate(
      index = dplyr::row_number()) %>%
    dplyr::filter(
      cor1 > cormin | cor2 > 0.8,
      pmax(logpvalr, logpvalc) > minlogp) %>%
    dplyr::filter(
      cor2 - cor1 == max(cor2 - cor1) |
        cor2 - cor1 == min(cor2 - cor1))
  
  cc <- matrix(NA, length(traits), length(traits))
  rr <- row(cc)[upper.tri(cc)][object$index]
  cc <- col(cc)[upper.tri(cc)][object$index]
  
  object %>%
    dplyr::mutate(traitc = traits[cc],
           traitr = traits[rr]) %>%
    purrr::transpose() %>%
    purrr::map(function(x) {
      if(x$logpvalc > x$logpvalr) {
        # switch row and columne
        tmp <- x$traitc
        x$traitc <- x$traitr
        x$traitr <- tmp
        tmp <- x$logpvalc
        x$logpvalc <- x$logpvalr 
        x$logpvalr <- tmp
      }
      x
    }) %>%
    purrr::transpose() %>%
    lapply(unlist) %>%
    as.data.frame() %>%
    dplyr::rename(logpvalx = "logpvalr",
           logpvaly = "logpvalc",
           traitx = "traitr",
           traity = "traitc") %>%
    dplyr::select(traitx, traity, everything()) %>%
    dplyr::select(traitx, logpvalx, traity, logpvaly, cor1, cor2) %>%
    dplyr::mutate(dplyr::across(is.numeric, signif, 3))
}

#' GGplot of extreme correlation data
#'
#' @param object data frame with appropriate data summaries
#' @param summary_type type of summary to extract from `object`
#' @param dat_extreme data frame with extreme correlation summaries
#' @param ... additional parameters
#' @param condition factors in form `"a + b"` to use for option faceting
#'
#' @return ggplot2 object
#' @export
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom ggplot2 facet_wrap
#'
#' @examples
ggplot_extreme <- function(object, summary_type,
                         dat_extreme = cor_extreme(...)[1,],
                         ...,
                         condition = "") {
  if(is.null(object) || is.null(dat_extreme) || !nrow(dat_extreme))
    return(plot_null("No data or extremes."))
  
  if(nrow(dat_extreme > 1)) {
    warning("dat_extreme should have only one row")
    dat_extreme <- dat_extreme[1,]
  }
  
  object <- object %>%
    dplyr::filter(trait %in% c(dat_extreme$traitx, dat_extreme$traity)) %>%
    tidyr::pivot_longer(signal:mean, names_to = "type", values_to = "value") %>%
    tidyr::pivot_wider(names_from = "trait", values_from = "value")

  p <- scatplot(
    dplyr::filter(object, type == summary_type),
    dat_extreme$traitx, dat_extreme$traity,
    line_strain = FALSE,
    title = paste(
      summary_type, "for",
      "cor_raw", round(dat_extreme$cor1, 2),
      ", cor_adj", round(dat_extreme$cor2, 2),
      ", logpvals", 
      paste(round(c(dat_extreme$logpvalx, dat_extreme$logpvaly), 2),
            collapse = ", ")))
  if(condition != "") {
    p <- p +
      ggplot2::facet_wrap(stats::formula(paste("~", condition)))
  }
  p
}