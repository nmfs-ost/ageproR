#' Import data from a AGEPRO Model Input File to create a new agepro_inp_model
#'
#' Function wrapper to create agepro_inp_model R6class. This function retruns a
#' new instance of the agepro_inp_model.
#'
#' @param file AGEPRO input file name path
#'
#' @template elipses
#'
#' @return R6class object containing the agepro_inp_model class object
#'
#' @export
#' @examples
#' \dontrun{
#'
#' # Load path of ageproR's included Example1 Input File
#' inpfile <- file.path(find.package("ageproR"),"example/Example1.INP")
#'
#' # Create agepro_inp_model and import data from inpfile
#' test <- import_agepro_model(inpfile)
#'
#' }
import_agepro_model <- function(file, ...) {
  # TODO: option to return a agepro_json_model. Assuming that file param is a
  # AGEPRO input file path. (Only exporting agepro_inp_model to agepro_json_model
  # data is available).

  # create_agepro_model, BUT print general parameters to Rconsole
  # Suppress NULL recruitment warning since read_inp is
  model <- suppressWarnings(create_agepro_model(
    type = "inp",
    enable_cat_print = FALSE,
    show_general_params = FALSE,
    ...
  ))

  #Verify agepro_inp_model
  valid_r6 <- checkmate::test_r6(
    model,
    classes = "agepro_inp_model",
    public = "read_inp"
  )
  if (isFALSE(valid_r6)) {
    coll <- checkmate::makeAssertCollection()
    coll$push(
      "Invalid 'agepro_inp_model' R6Class: does not have 'read_inp' function"
    )
    checkmate::reportAssertions(coll)
  }

  # Read input file
  return(model$read_inp(file))
}
