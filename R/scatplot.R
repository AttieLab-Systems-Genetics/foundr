#' Scatter plot by trait values
#'
#' @param data data frame
#' @param x name of first trait
#' @param y name of second trait
#' @param shape_sex use different shape by `sex` if `TRUE`
#' @param line_strain show separate lines by `strain` if `TRUE`
#' @param title title for plot
#'
#' @return
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes facet_grid geom_point geom_smooth ggplot
#'                     ggtitle scale_fill_manual scale_shape_manual
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' scatplot(sampleData, "A", "B")
scatplot <- function(data, x, y,
                     shape_sex = TRUE,
                     line_strain = TRUE,
                     title = paste(x, "vs", y)) {
  p <- ggplot2::ggplot(data) +
    ggplot2::aes(.data[[x]], .data[[y]], fill = strain)
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
  
  p
}


ggplot_scatplot <- function(traitData,
                            traitnames,
                            pair = traitpairs(traitnames, sep),
                            sep = " ON ", ...) {
  
  traits <- unique(unlist(stringr::str_split(pair, sep)))
  if(!all(traits %in% traitData$trait)) {
    return(NULL)
  }
  
  dat <- purrr::map(pair, scatplots, traitData, sep, ...)
  
  # Patch plots together by rows
  patchwork::wrap_plots(dat, nrow = length(dat))
}

scatplots <- function(x, traitData,
                      sep = " ON ", 
                      shape_sex = TRUE,
                      response = c("value","mean","signal"),
                      line_strain = TRUE,
                      ...) {
  
  response <- match.arg(response)
  
  # Split trait pair by colon. Reduce to traits in x.
  x <- stringr::str_split(x, sep)[[1]][2:1]
  traitData <- dplyr::filter(traitData, trait %in% x)
  
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
            dplyr::across(c("datatype", "trait", "strain", conds))),
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
  
  # create plot
  p <- scatplot(out, x[1], x[2],
                shape_sex = shape_sex, line_strain = line_strain)
  
  # Facet if there are data
  if("sex_condition" %in% names(traitData)) {
    ct <- dplyr::count(out, sex_condition)$n
    if(length(ct) > 1)
      p <- p + ggplot2::facet_grid(. ~ sex_condition)
  } else {
    ct <- dplyr::count(out, sex)$n
    if(length(ct) > 1)
      p <- p + ggplot2::facet_grid(. ~ sex)
  }
  p
}

