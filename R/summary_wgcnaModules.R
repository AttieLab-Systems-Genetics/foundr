#' Summary of List of WGCNA Modules
#'
#' @param object object of class `listof_wgcnaModules`
#' @param ... additional parameters
#'
#' @return data frame
#' @export
#' @importFrom dplyr arrange bind_rows desc group_by mutate n summarize ungroup
#' @importFrom purrr map set_names
#' @importFrom rlang .data
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
            module = as.character(.data$module)),
          .data$module),
        count = dplyr::n(),
        maxkME = signif(max(.data$kME), 4),
        minkME = signif(min(.data$kME), 4))),
    dplyr::desc(.data$count))
}

#' @export
#' @rdname summary_wgcnaModules
#' @method summary wgcnaModules
summary.wgcnaModules <- function(object, ...)
  summary_wgcnaModules(object, ...)

