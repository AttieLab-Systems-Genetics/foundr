#' Compare Dataset
#'
#' @param traitModule module from `moduleHarmony`
#' @param dataset dataset in `traitModule`
#' @param response response in `dataset`
#' @param ds1 first dataset within `dataset`
#' @param ds2 second dataset within `dataset`
#' @param ... additional arguments ignored
#'
#' @return object of class `module_kMEs`
#' @export
#' @importFrom dplyr inner_join mutate rename
#' @importFrom stringr str_remove str_replace
#' @importFrom rlang .data
#'
comp_datasets <- function(traitModule,
                          ds1 = datasets, ds2 = datasets[datasets != ds1],
                          dataset = "Metab", response = "value", ...) {
  
  # Construct `newmod` from `response` for `dataset` in `traitModule`.
  # Assumes (for now) that `trait` = `dataset`_`trait`_NNNN.
  newmod <-
    dplyr::mutate(
      dplyr::rename(
        traitModule[[dataset]][[response]]$modules,
        col = "module"),
      dataset = stringr::str_remove(.data$trait, "_.*$"),
      #    number = stringr::str_remove(.data$trait, "^.*_"),
      trait = stringr::str_remove(
        stringr::str_remove(.data$trait, "_[0-9]+$"),
        "^.*_"))
  tmp <- newmod$dataset
  newmod$dataset <- NULL
  newmod <- split(newmod, tmp)
  rm(tmp)
  
  # ***Hardwired for now.***
  # Dataset names recorded are long form: "Plasma.0min", etc.
  names(newmod) <- c("LivMet", "PlaMet0", "PlaMet120")
  
  # Take datasets from `newmod`.
  datasets <- names(newmod)
  ds1 <- match.arg(ds1)
  ds2 <- match.arg(ds2)
  
  # Construct `compmods` to have columns
  # `trait`, `ds1_col`, `ds1_kME`, `ds2_col`, `ds2_kME`.
  # Need to do contortions to flip names.
  compmods <- dplyr::inner_join(
    newmod[[ds1]],
    newmod[[ds2]],
    by = c("trait"),
    suffix = paste0("_", c(ds1, ds2))
  )
  names(compmods)[2:5] <- 
    stringr::str_replace(
      names(compmods)[2:5],
      "^(.*)_(.*)$", "\\2_\\1")
  
  class(compmods) <- c("module_kMEs", class(compmods))
  compmods
}

#' GGplot including creation of compare dataset object
#'
#' @param object from `moduleHarmony`
#' @param ds pair of datasets to plot
#' @param abs absolute kME if `TRUE`
#'
#' @return GGplot object
#' @export
#' @rdname comp_datasets
#'
ggplot_comp_datasets <- function(object, ds = c("PlaMet0","PlaMet120"), abs = FALSE,
                                 ...,
                                 compmods = comp_datasets(object, ds[1], ds[2], ...)) {
  ggplot_module_kMEs(compmods, ds[1], ds[2], abs, ...)
}

