#' Create a agepro_model class
#'
#' Function wrapper to create anagepro_model R6class. This function retruns a
#' new instance of the agepro_inp_model or agepro_json_model.
#'
#' @param type Determines the type of agepro_model:
#' * **`inp`**: By Default, Create an Agepro Model R6class for Agepro Input Files (`agepro_inp_model`)
#' * **`json`**: Create an Agepro Model R6Class for the JSON Input File Format (`agepro_json_model`)
#' @template elipses
#'
#' @return R6class object containing the agepro_model class object
#'
#' @export
#' @examples
#' \dontrun{
#' # General parameters for 2019-2026 Uku Projections Base (From AGEPRO-GUI Example 4)
#' test <- create_agepro_model( type="inp", yr_start=2019, yr_end= 2026,
#' age_begin = 1, age_end = 32, num_pop_sims = 1000, num_fleets = 4,
#' num_rec_models = 1, discards_present = 0, seed = 300)
#' }
#'
create_agepro_model <- function(type = c("inp", "json"), ...) {
  #validate type
  type <- match.arg(type)

  model <- switch(
    type,
    inp = ageproR::agepro_inp_model$new(...),
    json = ageproR::agepro_json_model$new(...)
  )

  #TODO: #120 Option to create recuitment models for create_agepro_model

  return(model)
}
