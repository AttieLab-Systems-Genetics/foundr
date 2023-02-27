#' GGplot of pairs of traits
#'
#' @param traitData 
#' @param traitnames 
#' @param pair 
#' @param sep 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
traitPairs <- function(traitData,
                       traitnames,
                       pair = trait_pairs(traitnames, sep),
                       response = c("value","mean","signal"),
                       sep = " ON ",
                       ...) {
  
  if(is.null(traitData))
    return(NULL)
  
  traits <- unique(unlist(stringr::str_split(pair, sep)))
  
  traitData <- tidyr::unite(
    traitData,
    datatraits,
    dataset, trait,
    sep = ": ", remove = FALSE)
  
  if(!all(traits %in% traitData$datatraits)) {
    return(NULL)
  }
  
  response <- match.arg(response)
  
  pairsetup <- function(x, traitData,
                        sep = " ON ",
                        ...) {
    # Split trait pair by colon. Reduce to traits in x.
    x <- stringr::str_split(x, sep)[[1]][2:1]
    traitData <- dplyr::filter(traitData, datatraits %in% x)
    
    if(response == "value") {
      # Create columns for each trait pair with full data.
      out <- pivot_pair(traitData, x)
    } else {
      out <- traitData
    }
    
    if("sex_condition" %in% names(traitData)) {
      groupsex <- "sex_condition"
      conds <- c("sex", "condition")
    } else {
      groupsex <- "sex"
      conds <- "sex"
    }
    
    if(response != "value" | nrow(out) < 2) { # Reduce to mean.
      line_strain <- FALSE
      # Problem of nrow<2 likely from traits having different subjects.
      out <- 
        dplyr::ungroup(
          dplyr::summarize(
            dplyr::group_by(
              traitData,
              dplyr::across(c("dataset", "trait", "strain", conds))),
            value = mean(value, na.rm = TRUE)))
      
      # pivot_pair not working when misaligned sex_condition
      
      # Create columns for each trait pair with trait means.
      out <- pivot_pair(out, x)
      if(groupsex == "sex_condition") {
        out <- tidyr::unite(
          out,
          sex_condition, sex, condition,
          remove = FALSE,
          na.rm = TRUE)
      }
    }
    attr(out, "pair") <- x
    out  
  }
  
  out <- purrr::map(
    purrr::set_names(pair),
    pairsetup, traitData, sep, ...)
  class(out) <- c("traitPairs", class(out))
  attr(out, "sep") <- sep
  
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

#' Title
#'
#' @param object 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
ggplot_traitPairs <- function(object, ...) {

  if(is.null(object))
    return(plot_null("No Trait Pairs Object."))
  plots <- purrr::map(object, pairplots, sep = attr(object, "sep"), ...)
  
  # Patch plots together by rows
  patchwork::wrap_plots(plots, nrow = length(plots))
}
pairplots <- function(object,
                      sep = attr(object, "sep"), 
                      shape_sex = TRUE,
                      line_strain = TRUE,
                      title = paste(pair[1], "vs", pair[2]),
                      ...) {
  # Get trait pair
  pair <- attr(object, "pair")

  # create plot
  p <- ggplot2::ggplot(object) +
    ggplot2::aes(.data[[pair[1]]], .data[[pair[2]]], fill = strain)
  if(line_strain) {
    p <- p +
      ggplot2::geom_smooth(
        method = "lm", se = FALSE, formula = 'y ~ x',
        aes(group = strain, col = strain))
  } else {
    # Because we specify fill in aes, we need to include it here.
    p <- p +
      ggplot2::geom_smooth(
        method = "lm", se = FALSE, formula = 'y ~ x',
        fill = "darkgrey", col = "darkgrey")
  }
  p <- p +
    ggplot2::scale_fill_manual(values = CCcolors) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(title)
  
  if(shape_sex) {
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(shape = sex), size = 3, color = "black", alpha = 0.65) +
      ggplot2::scale_shape_manual(values = c(23, 22))
  } else {
    p <- p +
      ggplot2::geom_point(size = 3, shape = 21, color = "black", alpha = 0.65)
  }
  
  # Facet if there are data
  if("sex_condition" %in% names(object)) {
    ct <- dplyr::count(object, sex_condition)$n
    if(length(ct) > 1)
      p <- p + ggplot2::facet_grid(. ~ sex_condition)
  } else {
    ct <- dplyr::count(object, sex)$n
    if(length(ct) > 1)
      p <- p + ggplot2::facet_grid(. ~ sex)
  }
  p
}

