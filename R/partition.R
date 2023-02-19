#' Partition Trait into Signal, Ancillary and Noise
#'
#' Partition a trait value into three parts based on ancillary and signal factors.
#' The signal is the main interest for comparison across traits, while the ancillary
#' concerns aspects of the experiment that are controlled. Noise is unexplained variation.
#' The three parts will be orthogonal to each other.
#' The `signal` factors, entered as terms for `formula`, are conditioned by the `ancillary` factors.
#' 
#' @param object data frame in long format with trait data
#' @param trait name of column with trait names
#' @param value name column with trait values
#' @param signal signal factor combination as string for `formula`
#' @param ancillary ancillary factor combination as string for `formula`
#'
#' @return data frame with added columns `ancillary`, `signal`, `noise`
#' 
#' @importFrom dplyr across arrange bind_rows distinct filter mutate select
#' @importFrom purrr map
#' @importFrom stats formula lm
#' 
#' @export
#'
#' @examples
partition <- function(object,
                    trait = "trait",
                    value = "value",
                    signal = ifelse(
                      is_condition,
                      "strain * sex * condition",
                      "strain * sex"),
                    ancillary = ifelse(
                      is_condition,
                      "strain * sex + sex * condition",
                      "sex")) {
  
  # Is condition in the object (and not all NA)?
  is_condition <- ("condition" %in% names(object))
  if(is_condition) {
    !is_conditon <- all(is.na(object$condition))
  }
  
  # Somehow this give extra entries when there are missing values.
  
  redfit <- function(object) {
    formred <- stats::formula(paste(value, "~", ancillary))
    fitred <- stats::lm(formred, object)
    resids <- rep(NA, nrow(object))
    resids[!is.na(object[[value]])] <- resid(fitred)
    preds <- rep(NA, nrow(object))
    preds[!is.na(object[[value]])] <- predict(fitred)
    object <- 
      dplyr::mutate(
        object,
        residred = resids,
        ancillary = preds)
    formful <- stats::formula(paste("residred", "~", signal))
    fitful <- stats::lm(formful, object)
    resids <- rep(NA, nrow(object))
    resids[!is.na(object[[value]])] <- resid(fitful)
    preds <- rep(NA, nrow(object))
    preds[!is.na(object[[value]])] <- predict(fitful)
    object <- dplyr::mutate(
      dplyr::select(object, -residred),
      signal = preds,
      noise = resids)
  }
  
  signal_terms <- 
    stringr::str_trim(
      unlist(
        stringr::str_split(signal, " *\\*|\\+|: *")))

  traits <- unique(object$trait)
  
  out <- dplyr::bind_rows(
    purrr::map(
      split(object, object[[trait]]),
      function(object) {
        # Use NULL if not enough data to fit.
        tryCatch(redfit(object), error = function(e) NULL)
      }),
    .id = trait)
  
  # All fits fail.
  if(!nrow(out)) 
    return(NULL)
  
  out <- dplyr::select(
    dplyr::mutate(
      dplyr::distinct(
        dplyr::filter(
          out,
          !is.na(.data[[value]])),
        dplyr::across(c(signal_terms, "trait", "signal", "ancillary"))),
      mean = signal + ancillary),
    -ancillary)
  
  dplyr::arrange(
    dplyr::mutate(
      out,
      trait = factor(trait, traits)),
    trait)
}
