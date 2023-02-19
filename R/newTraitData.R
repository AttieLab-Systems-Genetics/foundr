newTraitData <- function(datapath, condition_name, datatype_name = "uploaded") {
  newdata <- switch(
    tools::file_ext(datapath),
    csv = read.csv(datapath),
    xls, xlsx = readxl::read_excel(datapath),
    rds = readRDS(datapath))
  
  trnames <- names(newdata)
  
  # Need error handling here with useful message.
  cnames <- c("datatype","trait", "strain", "sex", "condition", "value")
  if(!all(cnames[-c(1,5)] %in% trnames))
    return(NULL)
  
  if(condition_name %in% trnames) {
    if("condition" %in% trnames & "condition" != condition_name) {
      # Another condition--merge them for now
      newdata <- tidyr::unite(
        newdata,
        "condition",
        .data[[condition_name]], condition,
        remove = TRUE)
    } else {
      # Change to name condition
      m <- match(condition_name, trnames)
      if(!is.na(m)) {
        names(newdata)[m] <- "condition"
      }
    }
  }
  
  if(!"datatype" %in% names(newdata))
    newdata$datatype <- datatype_name
  
  m <- match(cnames, names(newdata))
  newdata <- newdata[cnames[!is.na(m)]]

  # Normal scores with jitter for new data
  dplyr::ungroup(
    dplyr::mutate(
      dplyr::group_by(newdata, datatype, trait),
      value = nqrank(value, jitter = TRUE)))
  
}