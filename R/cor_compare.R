#' Compare correlation matrices using upper triangle
#'
#' @param traitStats data frame with trait summaries
#' @param cor1,cor2 correlation matrices to compare
#' @param ... additional parameters (not used)
#'
#' @return tibble of data frame
#' @export
#' @importFrom dplyr as_tibble filter
#' @importFrom rlang .data
#'
cor_compare <- function(traitStats, cor1, cor2, ...) {
  # Typically traitStats will be summary across traits for a particular datatype.
  
  # Pull p-values for signal and organize by rows and columns of cor matrices.
  tmp <- dplyr::filter(
    traitStats,
    .data$term == "signal")$p.value
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
#' @importFrom dplyr across everything filter mutate rename row_number select
#' @importFrom purrr map transpose
#'
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
  
  object <- 
    dplyr::filter(
      dplyr::filter(
        dplyr::mutate(
          object,
          index = dplyr::row_number()),
        .data$cor1 > cormin | .data$cor2 > 0.8,
        pmax(.data$logpvalr, .data$logpvalc) > minlogp),
      .data$cor2 - .data$cor1 == max(.data$cor2 - .data$cor1) |
        .data$cor2 - .data$cor1 == min(.data$cor2 - .data$cor1))
  
  cc <- matrix(NA, length(traits), length(traits))
  rr <- row(cc)[upper.tri(cc)][object$index]
  cc <- col(cc)[upper.tri(cc)][object$index]
  
  dplyr::mutate(
    dplyr::select(
      dplyr::select(
        dplyr::rename(
          as.data.frame(
            lapply(
              purrr::transpose(
                purrr::map(
                  purrr::transpose(
                    dplyr::mutate(
                      object,
                      traitc = .data$traits[cc],
                      traitr = .data$traits[rr])),
                  function(x) {
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
                  })),
              unlist)),
          logpvalx = "logpvalr",
          logpvaly = "logpvalc",
          traitx = "traitr",
          traity = "traitc"),
        .data$traitx, .data$traity, dplyr::everything()),
      traitx, logpvalx, traity, logpvaly, cor1, cor2),
    dplyr::across(is.numeric, signif, 3))
}
