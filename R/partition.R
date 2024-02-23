#' Partition Trait into Signal, Rest and Noise
#'
#' Partition a trait value into three parts based on rest and signal factors.
#' The signal is the main interest for comparison across traits, while the rest
#' concerns aspects of the experiment that are controlled. Noise is unexplained variation.
#' The three parts will be orthogonal to each other.
#' The `signal` factors, entered as terms for `formula`, are conditioned by the `rest` factors.
#' 
#' @param object data frame in long format with trait data
#' @param trait name of column with trait names
#' @param value name column with trait values
#' @param signal signal factor combination as string for `formula`
#' @param rest rest factor combination as string for `formula`
#'
#' @return data frame with added columns `signal`, `cellmean`
#' 
#' @importFrom dplyr across arrange bind_rows distinct everything filter mutate select
#' @importFrom purrr map
#' @importFrom stats formula lm predict resid
#' @importFrom broom augment
#' @importFrom rlang .data
#' 
#' @export
#'
#' @examples
#' partition(sampleData)
partition <- function(object,
                    trait = "trait",
                    value = "value",
                    signal = ifelse(
                      is_condition,
                      "strain * sex * condition",
                      "strain * sex"),
                    rest = ifelse(
                      is_condition,
                      "strain * sex + sex * condition",
                      "sex")) {
  
  # Is condition in the object (and not all NA)?
  is_condition <- ("condition" %in% names(object))
  if(is_condition) {
    is_condition <- !all(is.na(object$condition))
  }
  
  # Somehow this give extra entries when there are missing values.

  redfit <- function(object) {
    object <- dplyr::filter(object, !is.na(.data[[value]]))
    if(!nrow(object))
      return(NULL)
    
    # Reduced model: value ~ rest
    formred <- stats::formula(paste(value, "~", rest))
    fitred <- stats::lm(formred, object)
    object <- 
      dplyr::select(
        dplyr::rename(
          broom::augment(fitred, object),
          rest = ".fitted",
          residred = ".resid"),
        strain:residred)

    # Full model: value ~ signal + rest, or resids ~ signal
    formful <- stats::formula(paste("residred", "~", signal))
    fitful <- stats::lm(formful, object)
    dplyr::select(
      dplyr::rename(
        broom::augment(fitful, object),
        signal = ".fitted"),
      strain:signal, -residred)
  }
  
  signal_terms <- 
    stringr::str_trim(
      unlist(
        stringr::str_split(signal, " *\\*|\\+|: *")))

  traits <- unique(object$trait)
  
  if(is_dataset <- ("dataset" %in% names(object))) {
    # Temporarily for datatraits (called here trait) as `dataset: trait`
    object <- tidyr::unite(
      object,
      trait,
      .data$dataset, .data$trait,
      sep = ": ")
  }
  
  # For each trait, run redfit()
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
        dplyr::select(
          dplyr::filter(
            out,
            !is.na(.data[[value]])),
          -animal, -value),
        dplyr::across(c(signal_terms, "trait")), .keep_all = TRUE),
      cellmean = .data$signal + .data$rest),
    -rest)
  
  # *** At this point have duplicated data for ***
  # sex   condition strain
  #   M     HF_LC   WSB
  # with Emax, HalfLife, Tmax, AUC for many traits.
  # Look at area_under_curve(?)
  
  if(is_dataset) {
    # If dataset was in object, restore it from `dataset: trait`
    out <- 
      dplyr::select(
        tidyr::separate(
          out,
          .data$trait,
          c("dataset", "trait"), sep = ": "),
        .data$dataset, dplyr::everything())
  }
  
  dplyr::arrange(
    dplyr::mutate(
      out,
      trait = factor(.data$trait, traits)),
    .data$trait)
}
