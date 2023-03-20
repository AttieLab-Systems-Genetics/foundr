#' Select Traits and Strains for Response
#'
#' @param traitData data frame
#' @param traitSignal data frame
#' @param traitnames names of `dataset: trait` combinations to subset
#' @param strains names of strains to subset
#' @param response name of response to return
#' @param abbrev abbreviate names if `TRUE`
#'
#' @return data frame
#' @export
#' @importFrom dplyr all_of filter left_join mutate select
#' @importFrom tidyr unite
#' @importFrom rlang .data
#'
#' @examples
traitSolos <- function(traitData, traitSignal,
                       traitnames = unique(unite_datatraits(traitSignal)),
                       response = c("individual", "cellmean", "signal", "ind_signal"),
                       strains = names(CCcolors),
                       abbrev = FALSE,
                       sep = ": ") {
  
  if("condition" %in% names(traitData)) {
    bys <- c("dataset","strain","sex","condition","trait")
  } else {
    bys <- c("dataset","strain","sex","trait")
  }
  
  response <- match.arg(response)
  if(response %in% c("individual", "ind_signal")) {
    if(response == "ind_signal") {
      
      # Include columns for signal and value = individual - cellmean + signal
      traitData <- join_signal(traitData, traitSignal)
    } else {
      # Include columns for cellmean and value = individual
      traitData <-
        dplyr::select(
          dplyr::left_join(
            traitData,
            traitSignal,
            by = bys),
          -signal)
    }
    
    traitData <- 
      dplyr::filter(
        unite_datatraits(traitData, traitnames, TRUE),
        strain %in% strains)

  } else {
    traitData <- selectSignal(traitSignal, traitnames, response, strains)
  }
  
  # Want to preserve order of traitnames into traitData
  tmp <- tidyr::unite(
    traitData,
    datatraits, dataset,
    trait, sep = sep, remove = FALSE)
  m <- which(!duplicated(tmp$datatraits))
  names(m) <- tmp$datatraits[m]
  m <- m[traitnames]

  if(abbrev) {
    ltrait <- length(traitnames)
    # Temporary. Need to address dataset as well.
    
    traitData <- dplyr::mutate(
      traitData,
      trait = abbreviate(trait, ceiling(60 / ltrait)))
  }
  traitData <- dplyr::mutate(
    traitData,
    trait = factor(trait, unique(trait[m])),
    dataset = factor(dataset, unique(dataset[m])))
  

  class(traitData) <- c("traitSolos", class(traitData))
  attr(traitData, "response") <- response
  attr(traitData, "strains") <- strains
  attr(traitData, "traitnames") <- traitnames
  
  traitData
}
selectSignal <- function(object, traitnames, response,
                         strains = names(CCcolors)) {
  # The response must be either "cellmean" or "signal".
  
  if("condition" %in% names(object)) {
    bys <- c("dataset","strain","sex","condition","trait")
  } else {
    bys <- c("dataset","strain","sex","trait")
  }
  
  dplyr::mutate( # significant digits for key columns
    dplyr::distinct( # get distinct entries for key columns
      dplyr::select( # select only key columns
        dplyr::mutate(
          dplyr::filter(
            unite_datatraits(object, traitnames, TRUE),
            strain %in% strains),
          value = .data[[response]]),
        dplyr::all_of(c(bys, "value"))),
      .keep_all = TRUE),
    strain = factor(strain, names(CCcolors)),
    value = signif(value, 4))
}
#' Summary of traitSolos object
#'
#' @param object object of class `traitSolos`
#' @param customSettings list of custom settings (including "condition_name" and "dataset")
#' @param ... additional parameters
#'
#' @return summary table
#' @export
#' @rdname traitSolos
#'
summary_traitSolos <- function(object,
                               customSettings = NULL,
                               ...) {
  
  if(is.null(object) || !nrow(object))
    return(NULL)
  
  traitnames <- attr(object, "traitnames")
  strains <- attr(object, "strains")
  response <- attr(object, "response")
  
  uobject <- dplyr::arrange(
    dplyr::mutate(
      dplyr::distinct(
        tidyr::unite(
          object,
          datatraits, dataset, trait, sep = ": ", remove = FALSE),
        datatraits, dataset, trait),
      datatraits = factor(datatraits, traitnames)),
    datatraits)
  
  
  if(response %in% c("individual", "ind_signal")) {
    if(response == "individual")
      response <- "cellmean"
    if(response == "ind_signal")
      response <- "signal"
    object <- dplyr::select(object, -value)

    object <- selectSignal(object,
                           traitnames,
                           response,
                           strains)
  }

  nobject <- c(names(object), levels(object$strain))
  nobject <- nobject[!(nobject %in% c("strain", "value"))]
  object <- dplyr::arrange(
    dplyr::select(
      tidyr::pivot_wider(
        object,
        names_from = "strain", values_from = "value"),
      dplyr::all_of(nobject)),
    trait, sex)
  
  # Use custom condition name if present
  if(!is.null(customSettings$condition) &
     !is.na(m <- match("condition", names(object)))) {
    names(object)[m] <- customSettings$condition
  }
  
  # Mutate dataset to have longer name if present.
  if(!is.null(customSettings$dataset)) {
    object <- mutate_datasets(
      object,
      customSettings$dataset)
    uobject <- mutate_datasets(
      uobject,
      customSettings$dataset)
  }
  
  # Make sure traits have order requested.
  object <- dplyr::arrange(
    dplyr::mutate(
      object,
      dataset = factor(dataset, unique(uobject$dataset)),
      trait = factor(trait, unique(uobject$trait))),
    dataset, trait)
  
  object
}
#' Summary of traitSolos object
#'
#' @param object object of class `traitSolos`
#' @param ... additional parameters
#'
#' @return summary table
#' @export
#' @rdname traitSolos
#' @method summary traitSolos
#'
summary.traitSolos <- function(object, ...) {
  summary_traitSolos(object, ...)
}
