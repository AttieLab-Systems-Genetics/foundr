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
#' @seealso \code{\link{ggplot_traitSolos}}
#' @export
#' @importFrom dplyr all_of arrange distinct filter left_join mutate select
#' @importFrom tidyr pivot_wider unite
#' @importFrom rlang .data
#'
#' @examples
#' out <- traitSolos(sampleData)
#' summary(out)
#' plot(out)
traitSolos <- function(traitData, 
                       traitSignal = partition(traitData),
                       traitnames = trait_names(traitData),
                       response = c("value", "cellmean", "signal"),
                       strains = names(foundr::CCcolors),
                       abbrev = FALSE,
                       sep = ": ") {
  
  if("condition" %in% names(traitData)) {
    bys <- c("dataset","strain","sex","condition","trait")
  } else {
    bys <- c("dataset","strain","sex","trait")
  }
  
  response <- match.arg(response)
  if(response == "value") {
    # Include columns for cellmean and value
    traitData <-
      dplyr::select(
        dplyr::left_join(
          traitData,
          traitSignal,
          by = bys),
        -.data$signal)
    
    traitData <- 
      dplyr::filter(
        unite_datatraits(traitData, traitnames, TRUE),
        .data$strain %in% strains)

  } else {
    traitData <- selectSignal(traitSignal, traitnames, response, strains)
  }
  
  # Want to preserve order of traitnames into traitData
  tmp <- tidyr::unite(
    traitData,
    datatraits,
    .data$dataset, .data$trait,
    sep = sep, remove = FALSE)
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
    trait = factor(.data$trait, unique(.data$trait[m])),
    dataset = factor(.data$dataset, unique(.data$dataset[m])))
  

  class(traitData) <- c("traitSolos", class(traitData))
  attr(traitData, "response") <- response
  attr(traitData, "strains") <- strains
  attr(traitData, "traitnames") <- traitnames
  
  traitData
}
selectSignal <- function(object, traitnames, response,
                         strains = names(foundr::CCcolors)) {
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
            .data$strain %in% strains),
          value = .data[[response]]),
        dplyr::all_of(c(bys, "value"))),
      .keep_all = TRUE),
    strain = factor(.data$strain, names(foundr::CCcolors)),
    value = signif(.data$value, 4))
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
          datatraits,
          .data$dataset, .data$trait,
          sep = ": ", remove = FALSE),
        .data$datatraits, .data$dataset, .data$trait),
      datatraits = factor(.data$datatraits, traitnames)),
    .data$datatraits)
  
  
  if(response == "value") {
    response <- "cellmean"
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
    .data$trait, .data$sex)
  
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
      dataset = factor(.data$dataset, unique(uobject$dataset)),
      trait = factor(.data$trait, unique(uobject$trait))),
    .data$dataset, .data$trait)
  
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
