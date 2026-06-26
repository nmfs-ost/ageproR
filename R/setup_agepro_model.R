#' Function wrapper to create a agepro_model
#'
#' Functcion wrapper to create agepro_model R6class. This function retruns a
#' new instance of the agepro_inp_model or agepro_json_model
#'
#' @param type Create a agepro_inp_model (default), or agpepro_json_model
#' @template elipses
#'
#' @return R6class ojject cotaining the agepro_model class object
#'
#' @export
#' @examples
#' \dontrun{
#' # General parameters for 2019-2026 Uku Projections Base (Example 4)
#' test <- setup_agepro_model(
#'   type="inp",
#'   yr_start=2019,
#'   yr_end= 2026,
#'   age_begin = 1,
#'   age_end = 32,
#'   num_pop_sims = 1000,
#'   num_fleets = 4,
#'   num_rec_models = 1,
#'   discards_present = 0,
#'   seed = 300)
#' }
#'
setup_agepro_model <- function(type = c("inp", "json"), ...) {
  #validate type
  type <- match.arg(type)

  model <- switch(
    type,
    inp = ageproR::agepro_inp_model$new(...),
    json = ageproR::agepro_json_model$new(...)
  )

  #TODO: add support for recuitment models?

  return(model)
}
