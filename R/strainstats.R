#' Use Broom to Find Stats for Model Summaries
#'
#' @param object data frame in long format with trait data
#' @param trait name of column with trait names
#' @param value name column with trait values
#' @param signal signal factor combination as string for `formula`
#' @param ancillary ancillary factor combination as string for `formula`
#' @param calc_sd calculate SDs by `term` if `TRUE` (default)
#'
#' @return data frame with summaries by trait
#' 
#' @importFrom dplyr bind_rows filter select
#' @importFrom tidyr pivot_wider
#' @importFrom tibble as_tibble
#' @importFrom stringr str_remove
#' @importFrom stats drop1 formula lm sd
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importfrom rlang .data
#' 
#' @export
#'
#' @examples
strainstats <- function(object,
                    trait = "trait",
                    value = "value",
                    signal = "strain * sex * condition",
                    ancillary = "strain * sex + sex * condition",
                    calc_sd = TRUE) {

  out <- dplyr::bind_rows(
    purrr::map(
      split(object, object[[trait]]),
      function(traitdata) {
        sig <- tryCatch(signalfit(traitdata, value, signal, ancillary),
                       error = function(e) NULL)
        if(is.null(sig))
          return(NULL)
        form <- stats::formula(paste(value, "~", signal))
        fit <- stats::lm(form, traitdata)
        tibble::as_tibble(data.frame(
          rawSD = stats::sd(traitdata[[value]], na.rm = TRUE),
          p_signal = sig,
          strain_stats(fit, calc_sd)))
        }),
    .id = trait)
  
  # Rorder to agree with data object
  o <- dplyr::distinct(object, trait)$trait
  
  m <- match(o, out$trait)
  out[m,]
}

signalfit <- function(traitdata, value, signal, ancillary) {
  formful <- stats::formula(paste(value, "~", signal))
  formred <- stats::formula(paste(value, "~", ancillary))
  fitful <- stats::lm(formful, traitdata)
  fitred <- stats::lm(formred, traitdata)
  dplyr::select(
    broom::tidy(
      stats::anova(fitred, fitful)),
      p.value)[2,]$p.value
}

strain_stats <- function(fit, calc_sd = TRUE) {
  drops <- dplyr::select(
    dplyr::mutate(
      dplyr::filter(
        broom::tidy(stats::drop1(fit, fit, test = "F")),
        !grepl("<none>", .data$term)),
      # change term from a.b to p_a_b
      term = stringr::str_replace_all(
        paste0(term), ":", "_")),
    term, df, sumsq, p.value)
  
  pvals <- tidyr::pivot_wider(
    dplyr::select(
      dplyr::mutate(
        drops,
        term = paste0("p_", term)),
      -df, -sumsq),
    names_from = "term",
    values_from = "p.value")
  if(calc_sd) {
    sds <- tidyr::pivot_wider(
      dplyr::select(
        dplyr::mutate(
          drops,
          sumsq = sqrt(sumsq / df),
          term = paste0("sd_", term)),
        -p.value, -df),
      names_from = "term",
      values_from = "sumsq")
    
    bind_cols(pvals, sds)
  }
  else {
    pvals
  }
}

