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
  cnames <- c("dataset","trait", "strain", "sex", "condition", "value")
  if(!all(cnames[-c(1,5)] %in% trnames))
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