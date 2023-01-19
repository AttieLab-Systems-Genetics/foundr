#' Use Broom to Find P-values for Model Summaries
#'
#' @param object data frame in long format with trait data
#' @param trait name of column with trait names
#' @param value name column with trait values
#' @param signal signal factor combination as string for `formula`
#' @param ancillary ancillary factor combination as string for `formula`
#' @param interact interacting factor to select for summary table
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
broomit <- function(object,
                    trait = "trait",
                    value = "value",
                    signal = "strain * sex * diet",
                    ancillary = "strain * sex + sex * diet",
                    interact = stringr::str_remove(signal, " .*")) {
  if(is.null(interact)) {
    myfun <- function(fit) {
      broom::tidy(
        stats::drop1(fit, fit, test = "F"))
    }
  } else {
    myfun <- function(fit) {
      dplyr::filter(
        broom::tidy(
          stats::drop1(fit, fit, test = "F")),
        grepl(interact, .data$term))
    }
  }
  
  dplyr::bind_rows(
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
          signal = sig,
          tidyr::pivot_wider(
            dplyr::select(
              myfun(fit),
              term, p.value),
            names_from = "term",
            values_from = "p.value")))
        }),
    .id = trait)
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

