#' @importFrom utils read.csv
#' @importFrom tools file_ext
#' @importFrom readxl read_excel
#' @importFrom tidyr unite
#' @importFrom dplyr group_by mutate rename ungroup
#' @importFrom rlang .data
newTraitData <- function(datapath, condition_name, dataset_name) {
  newdata <- switch(
    tools::file_ext(datapath),
    csv = utils::read.csv(datapath),
    xls, xlsx = readxl::read_excel(datapath),
    rds = readRDS(datapath))
  
  trnames <- names(newdata)
  
  # Need error handling here with useful message.
  cnames <- c("dataset","trait", "strain", "sex", "animal", "condition", "value")
  if(!all(cnames[-c(1,6)] %in% trnames))
    return(NULL)
  
  if(condition_name %in% trnames) {
    if("condition" %in% trnames & "condition" != condition_name) {
      # Another condition--merge them for now
      newdata <- tidyr::unite(
        newdata,
        "condition",
        .data[[condition_name]], .data$condition,
        remove = TRUE)
    } else {
      # Change to name condition
      m <- match(condition_name, trnames)
      if(!is.na(m)) {
        names(newdata)[m] <- "condition"
      }
    }
  }
  
  if(!"dataset" %in% names(newdata)) {
    if("datatype" %in% names(newdata))
      newdata <- dplyr::rename(newdata, dataset = "datatype")
    else
      newdata$dataset <- dataset_name
  }
  
  m <- match(cnames, names(newdata))
  newdata <- newdata[cnames[!is.na(m)]]

  # Normal scores with jitter for new data
  dplyr::ungroup(
    dplyr::mutate(
      dplyr::group_by(newdata, .data$dataset, .data$trait),
      value = nqrank(.data$value, jitter = TRUE)))
}
bindNewTraitData <- function(newdata = NULL, traitdata = NULL, ...) {
  # Trait Data: <dataset>, strain, sex, animal, <condition>, trait, value 
  if(!is.null(newdata)) {
    if(is.null(traitdata)) {
      traitdata <- newdata
    } else {
      # Append new data
      trnames <- names(traitdata)
      newtrnames <- names(newdata)
      keepcol <- match(newtrnames, trnames, nomatch = 0)
      if(any(keepcol == 0)) {
        # Drop unused columns (see verifyColumns for column handling)
        newtrnames[newtrnames[keepcol == 0]] <- NULL
      }
      traitdata <- dplyr::bind_rows(
        traitdata,
        newdata[trnames[keepcol]])
    }
  }
  traitdata
}

bindNewTraitStats <- function(newdata = NULL, traitdata = NULL, traitstats = NULL, ...) {
  # Trait Stats: <dataset>, trait, term, SD, p.value
  
  # Create stats for new data
  if(!is.null(newdata)) {
    newtraitstats <- strainstats(newdata)
    if(!is.null(newtraitstats)) {
      traitstats <- dplyr::bind_rows(
        traitstats,
        newtraitstats)
    }
  }
  if(!is.null(traitstats)) {
    if(!"dataset" %in% names(traitstats))
      traitstats$dataset <- unique(traitdata$dataset)[1]
  }
  traitstats
}

bindNewTraitSignal <- function(newdata = NULL, traitsignal = NULL, ...) {
  # Trait Signal: <dataset>, strain, sex, <condition>, trait, signal, cellmean

  # Create signal for new data
  if(!is.null(newdata)) {
    newtraitsignal <- partition(newdata)
    if(!is.null(newtraitsignal)) {
      traitsignal <- dplyr::bind_rows(
        traitsignal,
        newtraitsignal)
    }
  }
  traitsignal
}

checkdata <- function(data) {
  dplyr::filter(
    dplyr::summarise(
      dplyr::group_by(
        data,
        dataset, sex, condition, trait, strain),
      n = dplyr::n(),
      .groups = "drop"),
    n > 1L)
}
