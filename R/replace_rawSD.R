# Replace `SD` with `rawSD` in `termstats`
replace_rawSD <- function(termstats, rawstats = NULL) {
  # `termstats` is filtered to `term` of interest.
  # `rawstats` is all stats (for traits in `termstats`).
  if(!is.null(rawstats) && "rawSD" %in% unique(rawstats$term)) {
    # Replace SD by rawSD    
    dplyr::left_join(
      dplyr::select(termstats, -SD),
      # Extract rawSD from `rawstats`
      dplyr::select(
        dplyr::filter(
          rawstats,
          .data$term == "rawSD"),
        dataset, trait, SD),
      by = c("dataset", "trait"))
    
  } else {
    dplyr::mutate(termstats, SD = 1)
  }
}