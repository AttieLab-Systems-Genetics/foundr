#' Use Broom to Find Stats for Model Summaries
#'
#' @param object data frame in long format with trait data
#' @param signal signal factor combination as string for `formula`
#' @param rest rest factor combination as string for `formula`
#' @param calc_sd calculate SDs by `term` if `TRUE` (default)
#' @param condition_name name of `condition` column if present.
#' @param ... parameters not used.
#'
#' @return data frame with summaries by trait
#' 
#' @importFrom dplyr arrange as_tibble bind_rows distinct filter mutate select
#' @importFrom tidyr pivot_wider separate_wider_delim unite
#' @importFrom stringr str_detect str_replace_all
#' @importFrom stats anova coef drop1 formula lm pf sd
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom rlang .data
#' 
#' @export
#' @examples
#' out <- strainstats(sampleData)
#' summary(out, "deviation", "parts")
#' summary(out, "log10.p", "terms")
strainstats <- function(object,
                    signal = ifelse(
                      is_condition,
                      paste("strain * sex *", condition_name),
                      "strain * sex"),
                    rest = ifelse(
                      is_condition,
                      paste("strain * sex + sex *", condition_name),
                      "sex"),
                    calc_sd = TRUE,
                    condition_name = "condition",
                    ...) {
  
  # Is condition in the object (and not all NA)?
  is_condition <- (condition_name %in% names(object))
  if(!is_condition) {
    # Rename `condition` column if needed.
    if("condition" %in% names(object)) {
      names(object)[names(object) == "condition"] <- condition_name
      is_condition <- TRUE
    }
  }
  
  if(is_condition) {
    is_condition <- !all(is.na(object[[condition_name]]))
  }
  
  # Construct `datatraits` from `dataset` and `trait`
  if(!("dataset" %in% names(object)))
    object$dataset <- "unknown"
  if(!("datatraits" %in% names(object)))
    object <- tidyr::unite(
      object,
      datatraits,
      .data$dataset, .data$trait,
      sep = ": ")
  
  # Construct fits of each `datatraits` using `fitsplit()`.
  out <- dplyr::bind_rows(
    purrr::map(
      split(object, object$datatraits),
      fitsplit, signal, rest, condition_name),
    .id = "datatraits")
  
  # Reorder to agree with data object
  o <- dplyr::distinct(object, datatraits)$datatraits
  out <- tidyr::separate_wider_delim(
    dplyr::arrange(
      dplyr::mutate(
        out,
        datatraits = factor(.data$datatraits, o)),
      .data$datatraits),
    .data$datatraits,
    ": ",
    names = c("dataset", "trait"))
  
  class(out) <- c("strainstats", class(out))
  out
}
fitsplit <- function(traitdata, signal, rest, condition_name = "condition") {
  sig <- tryCatch(signalfit(traitdata, "value", signal, rest),
                  error = function(e) NULL)
  if(is.null(sig))
    return(NULL)
  form <- stats::formula(paste("value", "~", signal))
  fit <- stats::lm(form, traitdata)
  rawSD <-  stats::sd(traitdata$value, na.rm = TRUE)
  
  # Find sign of coefficients for condition and sex
  coefs <- stats::coef(fit)
  if(condition_name %in% names(traitdata) && length(unique(traitdata[[condition_name]])) == 2) {
    condsign <- sign(coefs[stringr::str_detect(names(coefs), condition_name) &
                             !stringr::str_detect(names(coefs), ":")])
  } else
    condsign <- 1
  sexsign <- sign(coefs[stringr::str_detect(names(coefs), "sex") &
                          !stringr::str_detect(names(coefs), ":")])
  
  out <- dplyr::mutate(
    dplyr::bind_rows(
      sig,
      strain_stats(stats::drop1(fit, fit, test = "F"))),
    SD = .data$SD / rawSD,
    SD = ifelse(.data$term == condition_name, .data$SD * condsign, .data$SD),
    SD = ifelse(.data$term == "sex", .data$SD * sexsign, .data$SD))
  out
}

traitOrderStats <- function(object, termname) {
  dplyr::arrange(
    dplyr::filter(
      object,
      .data$term == termname),
    .data$p.value)
}

termStats <- function(object, signal = TRUE, condition_name = "condition") {
  terms <- unique(object$term)

  if(signal) {
    # Return the strain terms with condition if present
    if(any(grepl(condition_name, terms)))
      terms <- c("signal", terms[grepl(paste0(".*strain.*", condition_name), terms)])
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
    termfn <- function(term) term # stringr::str_replace_all(paste0(term), ":", "_")
  else
    termfn <- function(term) termname
  
  dplyr::select(
    dplyr::rename(
      dplyr::mutate(
        broom::tidy(fitsum)[-1,],
        sumsq = ifelse(.data$df > 0, .data$sumsq / .data$df, 0),
        sumsq = sqrt(pmax(0, .data$sumsq)),
        term = termfn(.data$term)),
      SD = "sumsq"),
    .data$term, .data$SD, .data$p.value)
}
#' Summary of Strain Statistics
#'
#' @param object object of class `strainstats`
#' @param terms terms to include (overrides `model` and `stats`)
#' @param stats choice of `deviation` or `log10.p`
#' @param model choice of model `parts` or `terms`
#' @param threshold named vector for `SD` and `p.value`
#' @param ... not used
#'
#' @return data frame
#' @export
#' @importFrom dplyr arrange desc filter mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' 
#' @rdname strainstats
#'
summary_strainstats <- function(object,
                                terms = NULL,
                                stats = c("deviation", "log10.p"),
                                model = c("parts", "terms"),
                                threshold = c(SD = 1, p = 0.01),
                                ...) {
  
  if(is.null(object))
    return(NULL)
  
  # Set thresholds.
  if(!is.null(threshold) && length(threshold)) {
    if(!("log10.p" %in% names(threshold))) {
      if("p" %in% names(threshold))
        threshold["log10.p"] <- -log10(threshold["p"])
      else
        threshold["log10.p"] <- -Inf
    }
    if(!("deviance" %in% names(threshold))) {
      if("SD" %in% names(threshold))
        threshold["deviance"] <- threshold["SD"]
      else
        threshold["deviance"] <- 0
    }
  } else {
    threshold <- c(deviance = 0, log10.p = -Inf)
  }
  
  # Rename deviance = "SD" and log10.p = -log10("p.value").
  object <-
    dplyr::mutate(
      dplyr::filter(
        dplyr::mutate(
          dplyr::rename(
            object,
            deviance = "SD",
            log10.p = "p.value"),
          log10.p = -log10(.data$log10.p)),
        abs(.data$deviance) >= threshold["deviance"],
        .data$log10.p >= threshold["log10.p"]),
      dplyr::across(
        dplyr::where(is.numeric),
        function(x) signif(x, 4)))
  
  if(!is.null(terms)) {
    dplyr::arrange(
      dplyr::filter(
        object,
        .data$term %in% terms),
      dplyr::desc(log10.p))
  } else {
    if(length(model) == 1) {
      object <-
        switch(
          model,
          parts = {
            dplyr::filter(
              object,
              .data$term %in% c("signal", "cellmean", "rest", "noise"))
          },
          terms ={
            dplyr::filter(
              object,
              !(.data$term %in% c("signal", "cellmean", "rest", "noise")))
          })
    }
    
    if(length(stats) == 1) {
      switch(stats,
             deviation = {
               tidyr::pivot_wider(
                 dplyr::select(
                   dplyr::filter(
                     object,
                     .data$term != "noise"),
                   -.data$log10.p),
                 names_from = "term", values_from = "deviance")
             },
             log10.p = {
               tidyr::pivot_wider(
                 dplyr::select(
                   object,
                   -deviance),
                 names_from = "term", values_from = "log10.p")
             })
    } else {
      dplyr::arrange(
        object,
        dplyr::desc(log10.p))
    }
  }
}
#' @export
#' @rdname strainstats
#' @method summary strainstats
summary.strainstats <- function(object, ...)
  summary_strainstats(object, ...)

