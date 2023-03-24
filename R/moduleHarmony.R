#' Harmonize module object
#'
#' @param object object of class traitModule
#' @param response name of response to extract
#'
#' @return harmonize object
#' @export
#' @importFrom dplyr bind_rows filter select
#' @importFrom tidyr pivot_longer separate_wider_delim unite
#' @importFrom purrr transpose
#' @importFrom tibble rownames_to_column
#'
#' @examples
moduleHarmony <- function(object, response = names(object)) { 
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
              #     put rowname in ID column
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
          ID, "_",
          names = c("strain", "sex", "carb", "fat", "animal"),
          # Responses cellmean and signal do not have `animal`.
          too_few = "align_start"),
        condition, carb, fat)
      response == "individual"),
    -response)
}