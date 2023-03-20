#' Summary of List of WGCNA Modules
#'
#' @param object object of class `listof_wgcnaModules`
#' @param ... additional parameters
#'
#' @return data frame
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom purrr map set_names
#' 
#' @rdname summary_wgcnaModules
#' @method summary listof_wgcnaModules
#'
summary.listof_wgcnaModules <- function(object, ...) {
  dplyr::bind_rows(
    purrr::set_names(
      purrr::map(
        names(object),
        function(x) summary(object[[x]])),
      names(object)),
    .id = "response")
}

#' @param object,x object of class `wgcnaModules`
#' @param main title for plot
#' @param ... additional parameters

#' @export
summary_wgcnaModules <- function(object, ...) {
  dplyr::arrange(
    dplyr::ungroup(
      dplyr::summarize(
        dplyr::group_by(
          dplyr::mutate(
            object$modules,
            module = as.character(module)),
          module),
        count = dplyr::n(),
        maxkME = signif(max(kME), 4),
        minkME = signif(min(kME), 4))),
    dplyr::desc(count))
}

#' @export
#' @rdname summary_wgcnaModules
#' @method summary wgcnaModules
summary.wgcnaModules <- function(object, ...)
  summary_wgcnaModules(object, ...)

