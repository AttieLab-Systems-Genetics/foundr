foundrIntro <- function() {
  if(exists("userDatasets") &&
     is.function(userDatasets) &&
     all(is.list(userDatasets()))) {
    datainfo <- userDatasets()
  } else {
    datainfo <- ""
  } 

  renderUI({
    tagList(
      "Founder dataset consists of",
      shiny::a("8 CC mice strains,",
               href = "https://www.jax.org/news-and-insights/2009/april/the-collaborative-cross-a-powerful-systems-genetics-tool"),
      "and both sexes, possibly crossed with experimental conditions.",
      datainfo,
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
                           shape_sex = TRUE,
                           response = c("value","mean","signal"),
                           line_strain = TRUE,
                           sep = " ON ") {
  
  response <- match.arg(response)
  
  traits <- unique(unlist(stringr::str_split(pair, sep)))
  if(!all(traits %in% traitData$trait)) {
    return(NULL)
  }
  
  scatplots <- function(x) {
    # Split trait pair by colon. Reduce to traits in x.
    x <- stringr::str_split(x, sep)[[1]][2:1]
    traitData <- dplyr::filter(traitData, trait %in% x)
    
    if(response == "value") {
      # Create columns for each trait pair with full data.
      out <- pivot_pair(traitData, x)
    }
    
    if("sex_condition" %in% names(traitData)) {
      groupsex <- "sex_condition"
    } else {
      groupsex <- "sex"
    }
    
    if(response != "value" | nrow(out) < 2) { # Reduce to mean.
      line_strain <- FALSE
      # Problem of nrow<2 likely from traits having different subjects.
      out <- 
        dplyr::ungroup(
          dplyr::summarize(
            if(groupsex == "sex") {
              dplyr::group_by(
                traitData,
                datatype, trait, strain, sex)
            } else {
              dplyr::group_by(
                traitData,
                datatype, trait, strain, sex, condition)
            },
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
  
  dat <- purrr::map(pair, scatplots)
  
  # Patch plots together by rows
  patchwork::wrap_plots(dat, nrow = length(dat))
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
