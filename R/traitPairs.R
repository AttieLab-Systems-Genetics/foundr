#' Prepare pairs of traits for plotting
#'
#' @param object object of class `traitSolos`
#' @param traitnames trait names as `dataset: trait`
#' @param pair vector of trait name pairs, each joined by `sep`
#' @param sep pair separator
#' @param ... ignored
#'
#' @return object of class `traitPairs`
#' @export
#' @importFrom tidyr unite
#' @importFrom dplyr across count everything filter left_join mutate
#' @importFrom ggplot2 aes element_text facet_grid geom_line geom_point geom_smooth 
#'             ggplot ggtitle scale_color_manual scale_fill_manual scale_shape_manual theme
#' @importFrom rlang .data
#' 
#' @examples
#' out <- traitSolos(sampleData)
#' out2 <- traitPairs(out)
#' plot(out2, parallel_lines = TRUE)
traitPairs <- function(object,
                       traitnames = attr(object, "traitnames"),
                       pair = paste(traitnames[1:2], collapse = sep),
                       sep = " ON ",
                       ...) {
  
  if(is.null(object))
    return(NULL)
  if(length(traitnames) < 2)
    return(NULL)
  
  response <- attr(object, "response")
  
  # Allow user to either enter trait pairs or a number of trait pairs
  
  traits <- unique(unlist(stringr::str_split(pair, sep)))
  
  object <- tidyr::unite(
    object,
    datatraits,
    .data$dataset, .data$trait,
    sep = ": ", remove = FALSE)
  
  if(!all(traits %in% object$datatraits)) {
    return(NULL)
  }
  
  
  out <- purrr::map(
    purrr::set_names(pair),
    pairsetup, object, response, sep, ...)
  class(out) <- c("traitPairs", class(out))
  attr(out, "sep") <- sep
  attr(out, "response") <- response
  
  out
}
pairsetup <- function(x, object,
                      response,
                      sep = " ON ",
                      ...) {
  # Split trait pair by colon. Reduce to traits in x.
  x <- stringr::str_split(x, sep)[[1]][2:1]
  object <- dplyr::filter(object, datatraits %in% x)
  
  out <- pivot_pair(object, x)
  
  is_indiv <- (response == "value")
  if(is_indiv & nrow(out) < 2) {
    # Problem of nrow<2 likely from traits having different subjects.
    # Reduce to response
    response <- "cellmean"
    out <- selectSignal(object, x, response)
    out <- tidyr::unite(
      out,
      datatraits,
      .data$dataset, .data$trait,
      sep = ": ", remove = FALSE)
    
    # Create columns for each trait pair with trait means.
    out <- pivot_pair(out, x)
  }
  
  if("condition" %in% names(out)) {
    if(!all(is.na(out$condition))) {
      out <- tidyr::unite(
        out,
        sex_condition,
        .data$sex, .data$condition,
        remove = FALSE,
        na.rm = TRUE)
    } else {
      out$condition <- NULL
    }
  }
  
  attr(out, "pair") <- x
  attr(out, "response") <- response
  
  out  
}

trait_pairs <- function(traitnames, sep = " ON ") {
  as.vector(
    unlist(
      dplyr::mutate(
        as.data.frame(utils::combn(traitnames, 2)),
        dplyr::across(
          dplyr::everything(), 
          function(x) {
            c(paste(x, collapse = sep),
              paste(rev(x), collapse = sep))
          }))))
}

#' GGplot of trait pairs
#'
#' @param object,x object of class `traitPairs`
#' @param ... 
#' @param drop_xlab drop xlab for all but last plot if `TRUE`
#' @param legend_position position of legend ("none" for none)
#' @param legend_nrow number of rows for legend
#'
#' @return ggplot object
#' @export
#' @importFrom purrr map
#' @importFrom cowplot plot_grid
#'
ggplot_traitPairs <- function(object,
                              ...,
                              drop_xlab = TRUE,
                              legend_position = "bottom",
                              legend_nrow = 1) {

  ggplot_template(
    object,
    drop_xlab = drop_xlab,
    legend_position = legend_position,
    legend_nrow = 1,
    ...)
}
#' @export
#' @rdname traitPairs
#' @method autoplot traitPairs
autoplot.traitPairs <- function(object, ...) {
  ggplot_traitPairs(object, ...)
}
#' @export
#' @rdname traitPairs
#' @method plot traitPairs
plot.traitPairs <- function(x, ...) {
  autoplot.traitPairs(x, ...)
}

