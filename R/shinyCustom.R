foundrIntro <- function(helppath = "") {
  if(helppath != "" && file.exists(helppath)) {
    datainfo <- shiny::includeMarkdown(helppath)
  } else {
    if(exists("userDatasets") &&
       is.function(userDatasets) &&
       all(is.list(userDatasets()))) {
      datainfo <- userDatasets()
    } else {
      datainfo <- shiny::includeMarkdown(
        system.file(file.path("shinyApp", "help.md"), package='foundr'))
    } 
  }

  renderUI({
    tagList(
      "Founder dataset consists of",
      shiny::a("8 CC mice strains,",
               href = "https://www.jax.org/news-and-insights/2009/april/the-collaborative-cross-a-powerful-systems-genetics-tool"),
      "and both sexes, possibly crossed with experimental conditions.",
      datainfo,
      shiny::br(),
      "Select one or more traits after deciding measurement set(s) and trait order. Traits window supports partial matching to find desired traits.",
      "Facet plots by strain or `sex` or `sex_condition` and subset `strain`s if desired.",
      "Plots and data means (for selected traits) and data summaries (for whole measurement set) can be downloaded.",
      "See",
      "GigHub:", shiny::a("byandell/foundr",
                          href = "https://github.com/byandell/foundr"))
  })
}

foundrScatplot <- function(traitnames,
                           traitData,
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

foundrData <- function(traitData, traitnames) {
  ltrait <- length(traitnames)
  traitData <- dplyr::mutate(
    traitData,
    trait = abbreviate(trait, ceiling(60 / ltrait)))

  if("condition" %in% names(traitData)) {
    traitData <- tidyr::unite(
      traitData,
      sex_condition, sex, condition,
      remove = FALSE,
      na.rm = TRUE)
  }
  traitData
}



foundrMean <- function(traitData) {
  if("sex_condition" %in% names(traitData)) {
    groupsex <- "sex_condition"
  } else {
    groupsex <- "sex"
  }
  dplyr::arrange(
    tidyr::pivot_wider(
      dplyr::mutate(
        dplyr::ungroup(
          dplyr::summarize(
            dplyr::group_by(traitData, strain, .data[[groupsex]], trait),
            value = mean(value, na.rm = TRUE), .groups = "drop")),
        value = signif(value, 4)),
      names_from = "strain", values_from = "value"),
    trait, .data[[groupsex]])
}
