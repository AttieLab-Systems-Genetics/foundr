#' Partition Trait into Signal, Design and Noise
#'
#' Partition a trait value into three parts based on design and signal factors.
#' The signal is the main interest for comparison across traits, while the design
#' concerns aspects of the experiment that are controlled. Noise is unexplained variation.
#' The three parts will be orthogonal to each other.
#' The `signal` factors, entered as terms for `formula`, are conditioned by the `design` factors.
#' 
#' @param object data frame in long format with trait data
#' @param trait name of column with trait names
#' @param value name column with trait values
#' @param signal signal factor combination as string for `formula`
#' @param design design factor combination as string for `formula`
#'
#' @return
#' 
#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr map
#' @importFrom stats formula lm
#' 
#' @export
#'
#' @examples
residit <- function(object,
                    trait = "trait",
                    value = "value",
                    signal = "strain * sex * diet",
                    design = "strain * sex + sex * diet") {
  
  # Somehow this give extra entries when there are missing values.
  
  redfit <- function(object) {
    formred <- stats::formula(paste(value, "~", design))
    fitred <- stats::lm(formred, object)
    resids <- rep(NA, nrow(object))
    resids[!is.na(object[[value]])] <- resid(fitred)
    preds <- rep(NA, nrow(object))
    preds[!is.na(object[[value]])] <- predict(fitred)
    object <- 
      dplyr::mutate(
        object,
        residred = resids,
        predred = preds)
    formful <- stats::formula(paste("residred", "~", signal))
    fitful <- stats::lm(formful, object)
    resids <- rep(NA, nrow(object))
    resids[!is.na(object[[value]])] <- resid(fitful)
    preds <- rep(NA, nrow(object))
    preds[!is.na(object[[value]])] <- predict(fitful)
    object <- dplyr::mutate(
      object, 
      residful = resids,
      predful = preds)
  }
  
  dplyr::bind_rows(
    purrr::map(
      split(object, object[[trait]]),
        function(object) {
          # Use NULL if not enough data to fit.
          tryCatch(redfit(object), error = function(e) NULL)
        }),
    .id = trait)
}
