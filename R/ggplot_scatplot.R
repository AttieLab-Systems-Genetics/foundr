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
