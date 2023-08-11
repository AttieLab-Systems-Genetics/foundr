corTable <- function(key_trait, traitSignal, corterm, mincor = 0,
                     reldataset = NULL) {
  
  if(is.null(key_trait) || is.null(traitSignal))
    return(NULL)
  
  if(is.null(reldataset))
    return(NULL)
  #    return(dplyr::distinct(object, .data$dataset, .data$trait))
  
  # Select rows of traitSignal() with Key Trait or Related Datasets.
  object <- select_data_pairs(traitSignal, key_trait, reldataset)

  # Filter by mincor
  dplyr::filter(
    bestcor(object, key_trait, corterm),
    .data$absmax >= mincor)
}