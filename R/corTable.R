corTable <- function(key_trait, traitSignal, corterm, mincor,
                     reldataset = NULL) {

  # Select rows of traitSignal() with Key Traot or Related Datasets.
  object <- select_data_pairs(traitSignal, key_trait, reldataset)
  
  if(is.null(reldataset))
    return(dplyr::distinct(object, .data$dataset, .data$trait))
  
  # Filter by mincor
  dplyr::filter(
    bestcor(object, key_trait, corterm),
    .data$absmax >= mincor)
}