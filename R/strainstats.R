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
#' @examples
#' strainstats(sampleData)
strainstats <- function(object,
                    trait = "trait",
                    value = "value",
                    signal = ifelse(
                      is_condition,
                      "strain * sex * condition",
                      "strain * sex"),
                    ancillary = ifelse(
                      is_condition,
                      "strain * sex + sex * condition",
                      "sex"),
                    calc_sd = TRUE) {
  
  # Is condition in the object (and not all NA)?
  is_condition <- ("condition" %in% names(object))
  if(is_condition) {
    is_condition <- !all(is.na(object$condition))
  }
  
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
        rawSD <-  stats::sd(traitdata[[value]], na.rm = TRUE)
        
        dplyr::mutate(
          dplyr::bind_rows(
            sig,
            strain_stats(stats::drop1(fit, fit, test = "F"))),
          SD = SD / rawSD)
      }),
    .id = trait)
  
  # Reorder to agree with data object
  o <- dplyr::distinct(object, trait)$trait
  dplyr::mutate(
    dplyr::arrange(
      dplyr::mutate(
        out,
        trait = factor(trait, o)),
      trait),
    trait = as.character(trait))
}
#' Order Stats by Selected term name
#'
#' @param object data frame from `strainstats`
#' @param termname name of element in `term` column to order by
#'
#' @return
#' @export
#' @importFrom dplyr arrange filter mutate
#'
traitOrderStats <- function(object, termname) {
  o <- dplyr::arrange(
    dplyr::filter(
      object,
      term == termname),
    p.value)$trait
  # Arrange traits in this order
  object <- dplyr::mutate(
    dplyr::arrange(
      dplyr::mutate(
        object,
        trait = factor(trait, o)),
      trait),
    trait = as.character(trait))
}


#' Terms in Stats Object
#'
#' @param object object from `strainstats`
#'
#' @return
#' @export
#' @importFrom stringr str_remove
#'
#' @examples
termStats <- function(object) {
  terms <- unique(object$term)

  # Return the strain terms with condition if present
  if(any(grepl("condition", terms)))
    c("signal", terms[grepl(".*strain.*condition", terms)])
  else
    c("signal", terms[grepl(".*strain", terms)])
}

signalfit <- function(traitdata, value, signal, ancillary) {
  formful <- stats::formula(paste(value, "~", signal))
  formred <- stats::formula(paste(value, "~", ancillary))
  fitful <- stats::lm(formful, traitdata)
  fitred <- stats::lm(formred, traitdata)
  
  strain_stats(stats::anova(fitred, fitful), "signal")
}

strain_stats <- function(fitsum, termname = "") {
  if(all(termname == ""))
    termfn <- function(term) stringr::str_replace_all(paste0(term), ":", "_")
  else
    termfn <- function(term) termname
  
  dplyr::select(
    dplyr::rename(
      dplyr::mutate(
        broom::tidy(fitsum)[-1,],
        sumsq = sqrt(sumsq / df),
        term = termfn(term)),
      SD = "sumsq"),
    term, SD, p.value)
}

