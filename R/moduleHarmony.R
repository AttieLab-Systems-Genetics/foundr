#' Harmonize module object
#'
#' @param datasetname name to give to dataset (here for compatibility)
#' @param object object of class traitModule
#' @param links data frame with links (not used yet)
#' @param responsename name of response to extract
#' @param ... additional parameters ignored
#'
#' @return harmonize object
#' @export
#' @importFrom dplyr bind_rows everything filter mutate select
#' @importFrom tidyr pivot_longer separate_wider_delim unite
#' @importFrom purrr transpose
#' @importFrom tibble rownames_to_column
#' @importFrom rlang .data
#'
#' @examples
moduleHarmony <- function(datasetname,
                          links,
                          object,
                          responsename = names(object[[1]]),
                          ...) { 
  responsename <- match.arg(responsename)
  
  dplyr::select(
    tidyr::unite(
      # Select only one response
      dplyr::select(
        dplyr::filter(
          # Unite carb and fat into diet = condition. **Specific to Founder Diet Study**
          tidyr::unite(
            # Separate ID into strain, sex, condition, animal.
            # But it over-separates as condition = carb_fat.
            tidyr::separate_wider_delim(
              # Process each response for each dataset.
              dplyr::bind_rows(
                lapply(
                  purrr::transpose(object),
                  # Get eigen data frame for each dataset:
                  #     put rowname in ID column,
                  #.    pivot module columns longer into trait and value,
                  #.    bind together across datasets.
                  function(x) {
                    dplyr::bind_rows(
                      lapply(
                        purrr::transpose(x)$eigen,
                        function(y) {
                          tidyr::pivot_longer(
                            tibble::rownames_to_column(y, "ID"),
                            -ID,
                            names_to = "trait", values_to = "value")
                        }),
                      .id = "dataset")
                  }),
                .id = "response"),
              .data$ID,
              delim = "_",
              names = c("strain", "sex", "carb", "fat", "animal"),
              # Responses cellmean and signal do not have `animal`.
              too_few = "align_start"),
            .data$condition, .data$carb, .data$fat),
          .data$response == responsename),
        -.data$response),
      .data$trait,
      .data$dataset, .data$trait),
    .data$strain, .data$sex, .data$animal, .data$condition, .data$trait, .data$value)
}