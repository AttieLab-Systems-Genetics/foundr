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
#'
#' @return ggplot object
#' @export
#' @importFrom purrr map
#' @importFrom cowplot plot_grid
#'
ggplot_traitPairs <- function(object, ...) {

  if(is.null(object) || !nrow(object[[1]]))
    return(plot_null("No Trait Pairs to Plot."))

  plots <- purrr::map(
    object,
    ggplot_onerow,
    ...)
  
  # Patch plots together by rows
  cowplot::plot_grid(plotlist = plots, nrow = length(plots))
}
pairplots <- function(object,
                      sep = attr(object, "sep"), 
                      shape_sex = TRUE,
                      parallel_lines = FALSE,
                      line_strain = (response == "value"),
                      title = paste(pair[1], "vs", pair[2]),
                      ...) {
  # Get trait pair
  pair <- attr(object, "pair")
  response <- attr(object, "response")
  
  object <- parallels(object, pair[1], pair[2],
                      line_strain, parallel_lines)

  # create plot
  p <- ggplot2::ggplot(object) +
    ggplot2::aes(.data[[pair[1]]], .data[[pair[2]]])
  
  p <- strain_lines(p, line_strain, parallel_lines)
  
  p <- p +
    ggplot2::scale_fill_manual(values = foundr::CCcolors) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1)) +
    ggplot2::ggtitle(title)
  
  if(shape_sex) {
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(fill = .data$strain, shape = .data$sex),
        size = 3, color = "black", alpha = 0.65) +
      ggplot2::scale_shape_manual(values = c(23, 22))
  } else {
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(fill = .data$strain),
        size = 3, shape = 21, color = "black", alpha = 0.65)
  }
  
  # Facet if there are data
  if("sex_condition" %in% names(object)) {
    ct <- dplyr::count(object, .data$sex_condition)$n
    if(length(ct) > 1)
      p <- p + ggplot2::facet_grid(. ~ .data$sex_condition)
  } else {
    ct <- dplyr::count(object, .data$sex)$n
    if(length(ct) > 1)
      p <- p + ggplot2::facet_grid(. ~ .data$sex)
  }
  p
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

parallels <- function(
    object,
    line_strain = (response == "value"),
    parallel_lines = TRUE,
    pair = NULL,
    ...) {
  if(parallel_lines & !is.null(pair)) {
    response <- attr(object, "response")
    
    if("sex_condition" %in% names(object)) {
      groupsex <- "sex_condition"
    } else {
      groupsex <- "sex"
    }
    if(line_strain) {
      form <- formula(paste0("`", pair[2], "` ~ `", pair[1],
                             "` + strain * `", groupsex, "`"))
      bys <- c(pair, "strain", groupsex)
    } else {
      form <- formula(paste0("`", pair[2], "` ~ `", pair[1],
                             "` + `", groupsex, "`"))
      bys <- c(pair, groupsex)
    }
    dplyr::left_join(
      object,
      broom::augment(lm(form, object)),
      by = bys)
  } else {
    object
  }
}
strain_lines <- function(
    object,
    p,
    plotcolors = foundr::CCcolors,
    fillname = "strain",
    line_strain = (response == "value"),
    parallel_lines = TRUE,
    smooth_method = attr(object, "smooth_method"),
    pair = NULL,
    ...) {
  
  if(is.null(pair))
    return(p)
  
  if(is.null(smooth_method))
    smooth_method <- "lm"
  
  response <- attr(object, "response")
  
  # Set x and y to the pair of traits.
  p <- p +
    ggplot2::aes(.data[[pair[1]]], .data[[pair[2]]])
  
  if(line_strain) {
    if(parallel_lines) {
      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(
            group = .data[[fillname]], col = .data[[fillname]],
            y = .data$.fitted),
          linewidth = 1) +
        ggplot2::scale_color_manual(values = plotcolors)
    } else {
      p <- p +
        ggplot2::geom_smooth(
          ggplot2::aes(
            group = .data[[fillname]], col = .data[[fillname]]),
          method = smooth_method, se = FALSE, formula = "y ~ x",
          linewidth = 1) +
        ggplot2::scale_color_manual(values = plotcolors)
    }
  } else {
    if(parallel_lines) {
      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(y = .data$.fitted),
          linewidth = 1, col = "darkgrey")
      
    } else {
      p <- p +
        ggplot2::geom_smooth(
          method = "lm", se = FALSE, formula = "y ~ x",
          linewidth = 1, col = "darkgrey")
    }
  }
  p
}
