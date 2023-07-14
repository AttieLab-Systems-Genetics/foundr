add_time_summaries <- function(GTT) {
  # Copied from Physio.R in FounderDietStudy
  # Hold off on automating for now.
  
  # Add area under curve traits for measurements over minutes.
  GTT <- out %>%
    filter(grepl("_[0-9]+_[0-9]+wk$", trait)) %>%
    # Separate out minutes and week.
    # Kludge to catch cpep ratio trait.
    separate_wider_delim(
      trait,
      delim = "_",
      names = c("cpep1", "cpep2", "gtt","trait","minute","week"),
      too_few = "align_end") %>%
    mutate(trait = ifelse(
      trait == "ratio",
      paste(cpep1, cpep2, gtt, trait, sep = "_"),
      paste(gtt, trait, sep = "_")))
  
  # Filter to traits with >1 minute measurement.
  GTTct <- GTT %>%
    distinct(trait, minute, week) %>%
    count(trait, week) %>%
    filter(n > 1)
  
  GTT <- GTT %>%
    filter(trait %in% GTTct$trait & week %in% GTTct$week) %>%
    # Calculate AUC and other summaries.
    area_under_curve("minute") %>%
    # Unite summary name with week.
    unite(trait, trait, week) %>%
    # Harmonize names.
    select(strain, sex, animal, condition, trait, value)
  
  GTT <- GTT %>%
    filter(trait %in% GTTct$trait & week %in% GTTct$week) %>%
    # Calculate AUC and other summaries.
    area_under_curve("minute") %>%
    # Unite summary name with week.
    unite(trait, trait, week) %>%
    # Harmonize names.
    select(strain, sex, animal, condition, trait, value)
  
  # Add area under curve traits for measurements over weeks.
  wks <- out %>%
    filter(grepl("_[0-9]+wk$", trait) & !grepl("_([0-9]+|tAUC|iAUC)_[0-9]+wk$", trait)) %>%
    # Kludge to use AUC routine for now by calling weeks as minutes.
    separate_wider_delim(
      trait,
      delim = "_",
      names = c("trait1","trait","week"),
      too_few = "align_end") %>%
    mutate(
      trait = ifelse(
        is.na(trait1),
        trait,
        paste(trait1, trait, sep = "_")),
      week = as.numeric(str_remove(week, "wk$")))
  
  # Filter to traits with >1 week measurement.
  wksct <- wks %>%
    distinct(trait, week) %>%
    count(trait) %>%
    filter(n > 1)
  
  wks <- wks %>%
    filter(trait %in% wksct$trait) %>%
    # Calculate AUC and other summaries.
    area_under_curve("week") %>%
    # Harmonize names.
    select(strain, sex, animal, condition, trait, value)
  
  bind_rows(out, GTT, wks)
}