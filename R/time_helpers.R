time_units <- function(timetrait_all) {
  # Find time units in datasets
  timeunits <- NULL
  if("week" %in% timetrait_all$timetrait)
    timeunits <- c("week","week_summary")
  if("minute" %in% timetrait_all$timetrait)
    timeunits <- c(timeunits, "minute","minute_summary")
  timeunits
}
time_trait_subset <- function(object, timetrait_all) {
  object <- tidyr::unite(object,
                         datatraits,
                         .data$dataset, .data$trait,
                         remove = FALSE, sep = ": ")
  timetrait_all <- tidyr::unite(timetrait_all,
                                datatraits,
                                .data$dataset, .data$trait,
                                remove = FALSE, sep = ": ")
  dplyr::select(
    dplyr::filter(
      object,
      .data$datatraits %in% timetrait_all$datatraits),
    -datatraits)
}