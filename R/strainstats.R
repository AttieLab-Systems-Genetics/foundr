#' Use Broom to Find Stats for Model Summaries
#'
#' @param object data frame in long format with trait data
#' @param signal signal factor combination as string for `formula`
#' @param rest rest factor combination as string for `formula`
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
                    signal = ifelse(
                      is_condition,
                      "strain * sex * condition",
                      "strain * sex"),
                    rest = ifelse(
                      is_condition,
                      "strain * sex + sex * condition",
                      "sex"),
                    calc_sd = TRUE) {
  
  # Is condition in the object (and not all NA)?
  is_condition <- ("condition" %in% names(object))
  if(is_condition) {
    is_condition <- !all(is.na(object$condition))
  }
  
  if(!("dataset" %in% names(object)))
    object$dataset <- "unknown"
  if(!("datatraits" %in% names(object)))
    object <- tidyr::unite(
      object,
      datatraits,
      dataset, trait,
      sep = ": ")
  
  out <- dplyr::bind_rows(
    purrr::map(
      split(object, object$datatraits),
      fitsplit, signal, rest),
    .id = "datatraits")
  
  # Reorder to agree with data object
  o <- dplyr::distinct(object, datatraits)$datatraits
  tidyr::separate_wider_delim(
    dplyr::arrange(
      dplyr::mutate(
        out,
        datatraits = factor(datatraits, o)),
      datatraits),
    datatraits,
    ": ",
    names = c("dataset", "trait"))
}
fitsplit <- function(traitdata, signal, rest) {
  sig <- tryCatch(signalfit(traitdata, "value", signal, rest),
                  error = function(e) NULL)
  if(is.null(sig))
    return(NULL)
  form <- stats::formula(paste("value", "~", signal))
  fit <- stats::lm(form, traitdata)
  rawSD <-  stats::sd(traitdata$value, na.rm = TRUE)
  
  dplyr::mutate(
    dplyr::bind_rows(
      sig,
      strain_stats(stats::drop1(fit, fit, test = "F"))),
    SD = SD / rawSD)
}

#' Order Stats by p-value of selected term name
#'
#' @param object data frame from `strainstats`
#' @param termname name of element in `term` column to order by
#'
#' @return
#' @export
#' @importFrom dplyr arrange filter mutate
#'
traitOrderStats <- function(object, termname) {
  dplyr::arrange(
    dplyr::filter(
      object,
      term == termname),
    p.value)
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
termStats <- function(object, signal = TRUE) {
  terms <- unique(object$term)

  if(signal) {
    # Return the strain terms with condition if present
    if(any(grepl("condition", terms)))
      terms <- c("signal", terms[grepl(".*strain.*condition", terms)])
    else
      terms <- c("signal", terms[grepl(".*strain", terms)])
  }
  terms
}

signalfit <- function(traitdata, value, signal, rest) {
  formful <- stats::formula(paste(value, "~", signal))
  formred <- stats::formula(paste(value, "~", rest))
  fitful <- stats::lm(formful, traitdata)
  fitred <- stats::lm(formred, traitdata)
  
  # Get extra terms from full and reduced fits
  sumful <- summary(fitful)
  sumred <- summary(fitred)
  Fful <- sumful$fstatistic
  Fred <- sumred$fstatistic
  extra <- data.frame(
    term = c("cellmean", "rest", "noise"),
    SD = c(sqrt(Fful[1]) * sumful$sigma,
           sqrt(Fred[1]) * sumred$sigma,
           sumful$sigma),
    p.value = c(
      stats::pf(Fful[1],
                Fful[2], Fful[3],
                lower.tail = FALSE),
      stats::pf(Fred[1] * (sumred$sigma / sumful$sigma)^2,
                Fred[2], Fful[3],
                lower.tail = FALSE),
      NA))
  dplyr::bind_rows(
    strain_stats(stats::anova(fitred, fitful), "signal"),
    extra)
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

