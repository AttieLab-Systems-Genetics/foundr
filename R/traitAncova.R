#' Trait Analysis of Covariance
#'
#' @param traitData data frame of trait data
#' @param traitStats data frame of trait stats
#' @param covarData data frame of covariate data
#' @param focus focus of mediation
#' @param trait_name character string of focal trait name
#' @param condition_name character string of condition name
#'
#' @return object of class `traitAncova`
#' @export
#' @importFrom dplyr filter inner_join left_join rename
#'
traitAncova <- function(traitData, traitStats,
                        covarData, 
                        focus = c("Driver", "Target"),
                        trait_name,
                        condition_name = "diet") {
  
  focus <- match.arg(focus)
  
  switch(
    focus,
    Target = {
      # Set up traitData; filter on `trait_name`
      traitData <- dplyr::filter(traitData, trait == trait_name)
      if(!nrow(traitData))
        return(NULL)
      traitData$trait <- NULL
    },
    Driver = {
      # Set up covarData; filter on `trait_name`
      covarData <- dplyr::filter(covarData, trait == trait_name)
      if(!nrow(covarData))
        return(NULL)
      covarData$trait <- NULL
    })
  names(covarData)[names(covarData) == "value"] <- "covar"
  
  traitData <- dplyr::inner_join(traitData, covarData,
                             by = c("strain","sex","animal","condition"))
  
  # change condition to `condition_name`
  names(traitData)[names(traitData) == "condition"] <- condition_name
  
  # Fit analysis of covariance. Slow step.
  traitAncova <- 
    dplyr::filter(
      strainstats(
        traitData, condition_name = condition_name,
        rest = paste("covar", "+ strain * sex + sex *", condition_name)),
      # Filter to term `signal`
      .data$term == "signal")

  switch(
    focus,
    Target = {
      # Reduce `traitStats` to `trait_name`.
      # Join `traitAncova` to `traitStats`.
      traitAncova <- dplyr::left_join(
        traitAncova,
        dplyr::select(
          dplyr::filter(traitStats, .data$trait == trait_name),
          -trait),
        by = c("dataset", "term"))
    },
    Driver = {
      # Join `traitAncova` to `traitStats`.
      traitAncova <- dplyr::left_join(
        traitAncova,
        traitStats,
        by = c("dataset","trait","term"))
    })
  
  # Rename `SD` and `p.value` columns.
  traitAncova <- dplyr::rename(
    traitAncova,
    SD = "SD.y",
    SD.cov = "SD.x",
    p.value = "p.value.y",
    p.value.cov = "p.value.x")
  
  class(traitAncova) <- c("traitAncova", class(traitAncova))
  attr(traitAncova, "trait_name") <- trait_name
  attr(traitAncova, "condition") <- condition_name
  attr(traitAncova, "focus") <- focus
  traitAncova
}
#' Summary of Trait Ancova
#'
#' @param traitAncova object of class `traitAncova`
#' @param traits trait names to summarize
#'
#' @return data frame as summary
#' @export
#' @rdname traitAncova
#' @importFrom dplyr arrange filter mutate select
#'
summary_traitAncova <- function(traitAncova, traits = NULL) {
  if(is.null(traitAncova))
    return(NULL)
  
  out <- 
    dplyr::mutate(
      dplyr::arrange(
        dplyr::select(
          dplyr::filter(
            traitAncova,
            .data$term == "signal"),
          trait, p.value, p.value.cov),
        .data$p.value.cov),
      p.value = signif(.data$p.value, 4),
      p.value.cov = signif(.data$p.value.cov, 4))
      
  if(!is.null(traits)) {
    out <- dplyr::filter(out, .data$trait %in% traits)
  }
  
  out
}
#' GGplot of Trait Ancova
#'
#' @param traitAncova object of class `traitAncova`
#' @param traits trait names to give unique colors
#' @param signif_level significance level
#' @param width,height jitter offsets
#'
#' @return ggplot object
#' @export
#' @rdname traitAncova
#' @importFrom dplyr arrange filter mutate
#' @importFrom ggplot2 aes geom_abline geom_point ggplot
#'             scale_x_log10 scale_y_log10 xlab ylab
#'
ggplot_traitAncova <- function(traitAncova, traits = NULL,
                               signif_level = 0.05,
                               width = 0.1, height = 0) {
  
  if(is.null(traitAncova))
    return(plot_null("no ancova object"))
  
  trait_name <- attr(traitAncova, "trait_name")
  focus <- attr(traitAncova, "focus")
  
  # Set up colors for traits of interest.
  
  if(length(traits)) {
    traitAncova <- 
      dplyr::mutate(traitAncova,
                    col = 1 + match(trait, traits, nomatch = 0),
                    col = factor(col))
  } else {
    traitAncova$col <- 1
  }
  
  traitAncova <-
    dplyr::arrange(
      dplyr::filter(
        traitAncova,
        .data$term == "signal"),
      .data$col)
  
  ylab <- switch(
    focus,
    Driver = paste("p_signal adjusted by", trait_name),
    Target = paste("p_signal for", trait_name, "adjusted by all covariates"))
  
  if(!is.null(signif_level)) {
    traitAncova <- dplyr::filter(
      traitAncova,
      p.value <= signif_level,
      p.value.cov <= signif_level)
  }
  ggplot(traitAncova) +
    aes(p.value, p.value.cov, col = col) +
    geom_abline(slope = 1, intercept = 0, col = "blue") +
    geom_jitter(width = width, height = height) +
    scale_x_log10() +
    scale_y_log10() +
    xlab("p_signal") +
    ylab(ylab)
}