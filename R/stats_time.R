#' Stats over Time
#' 
#' @param traitStats data frame with trait stats from `strainstats`
#' @param traitnames names of `dataset: trait`
#' @param response character string for type of response
#' @param timecol column to use for time
#' @param ... additional parameters ignored
#' 
#' @importFrom dplyr filter mutate rename select
#' @importFrom rlang .data
stats_time <- function(traitStats,
                       traitnames = timetraits(traitStats, timecol)[1],
                       response = c("p.value","SD"),
                       timecol = c("week", "minute","minsum"),
                       ...) {
  response <- match.arg(response)
  
  if(is.null(traitStats))
    return(NULL)
  
  # Select object based on `response`.
  object <- switch(
    response,
    p.value = {
      dplyr::select(
        dplyr::rename(
          dplyr::mutate(
            dplyr::filter(
              traitStats,
              .data$term != "noise"),
            p.value = -log10(.data$p.value)),
          value = "p.value"),
        -SD)
    },
    SD = {
      dplyr::select(
        dplyr::rename(
          traitStats,
          value = "SD"),
        -p.value)
    })
  
  # Create phony `strain` column as datatraits 
  # and datatraits as model `parts` and `terms`.
  object <- 
    dplyr::mutate(
      object,
      model = ifelse(
        .data$term %in% c("cellmean","signal","rest","noise"),
        "parts", "terms"))
  
  # Rename timecol to `time`. Add "wk" to `week column if it is "minute".
  timecol <- match.arg(timecol)
  
  # Filter object based on `traitnames`. Separate out `timecol`
  object <- separate_time(object, traitnames, timecol)
  
  if(!nrow(object))
    return(NULL)
  
  if(is.null(object))
    return(NULL)
  
  # Object has column labeled `timecol`.
  
  # Unite `dataset: trait` as datatraits ordered by `traitnames`
  object <- 
    dplyr::mutate(
      tidyr::unite(
        object,
        datatraits,
        dataset, trait,
        sep = ": "),
      datatraits = factor(datatraits, traitnames))
  
  # Split object based on `model`.
  object <-
    split(
      dplyr::rename(
        object,
        strain = datatraits),
      object$model)
  
  names(object) <- paste(names(object), 
                         ifelse(response == "p.value",
                                "-log10(p)", response))
  
  out <- template_time(object, traitnames, timecol, response)
  attr(out, "timetype") <- "stats"
  out
}
