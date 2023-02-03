foundrIntro <- function() {
  if(!exists("datasets" !! !all(sapply(unlist(datasets), class))) || is.null(names(datasets))) {
    datasets <- c(physio = "physiological data",
                  liver = "RNA-seq on liver",
                  plasma = "concentrations of circulating metabolites")
  }
  datatags <- shiny::tags$ul(
    lapply(
      paste(names(datasets), datasets, sep = ": "),
      function(x) shiny::tags$li(x)))
  
  renderUI({
    tagList(
      "This founder dataset consists of",
      shiny::a("8 CC mice strains,",
               href = "https://www.jax.org/news-and-insights/2009/april/the-collaborative-cross-a-powerful-systems-genetics-tool"),
      "two diets (HC_LF = high carb, low fat; HF_LC = high fat, low carb) and both sexes with three measurement sets collected on 192 mice:",
      datatags,
      "Select one or more traits after deciding measurement set(s) and trait order. Traits window supports partial matching to find desired traits.",
      "Facet plots by strain or sex_condition and subset strains if desired.",
      "Plots and data means (for selected traits) and data summaries (for whole measurement set) can be downloaded.",
      "See",
      shiny::a("Attie Lab Diabetes Database", href = "http://diabetes.wisc.edu/"),
      "for earlier study.",
      "GigHub:", shiny::a("byandell/FounderDietStudy",
                          href = "https://github.com/byandell/FounderDietStudy"))
  })
}

foundrScatplot <- function(traitnames,
                           traitData,
                           pair = foundr::traitpairs(traitnames, sep),
                           sep = " ON ") {
  dat <- 
    purrr::map(
      pair,
      function(x) {
        # Split trait pair by colon
        x <- stringr::str_split(x, sep)[[1]][2:1]
        # create out with columns for each trait pair
        out <- foundr::pivot_pair(
          tidyr::unite(traitData, sex_condition, sex, condition),
          x)

        # create plot
        foundr::scatplot(out, x[1], x[2], shape_sex = FALSE) +
          ggplot2::facet_grid(. ~ sex_condition)
      })
  
  # Patch plots together by rows
  patchwork::wrap_plots(dat, nrow = length(dat))
}

foundrData <- function(traitData, traitnames) {
  ltrait <- length(traitnames)
  tidyr::unite(
    dplyr::mutate(
      traitData,
      trait = abbreviate(traitnames, ceiling(60 / ltrait))),
    sex_condition, sex, condition)
}

foundrMean <- function(traitData) {
  dplyr::arrange(
    tidyr::pivot_wider(
      dplyr::mutate(
        dplyr::ungroup(
          dplyr::summarize(
            dplyr::group_by(traitData, strain, sex_condition, trait),
            value = mean(value, na.rm = TRUE), .groups = "drop")),
        value = signif(value, 4)),
      names_from = "strain", values_from = "value"),
    trait, sex_condition)
}
