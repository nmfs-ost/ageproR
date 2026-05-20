#' Run the AGEPRO Cacluation Engine Binary with a AGEPRO Input File
#'
#' Wrapper function to call the Binary
#'
#' @param exepath Path of the Agepro Calcuation engine
#' @param agepro_args Typically used for AGEPRO Input Files.
#' @param outdir Output path
#' @param save_logfile Option to save AGEPRO Calcuation Engine ouput to logfile
#' @param fn_logfile Logfile Filename
#'
#' @export
#' @examples
#' \dontrun{
#'
#' inp_file <- file.path(find.package("ageproR"),"Example_UKU.INP")
#'
#' # For this example, save the Agepro Calcuation Engine Binary in
#' # the 'agepro' subdirectory of home directory 'R_USER'
#' exepath <- path.expand("~/agepro/AGEPRO.EXE")
#' if(.Platform$OS.type == "windows") {
#'   exepath <- normalizePath(exepath, winslash="/")
#' }
#'
#' ageproR::run_model(exepath, inp_file, tempdir())
#' }
#'
run_model <- function(
  exepath = "agepro.exe",
  agepro_args = "",
  outdir = getwd(),
  save_logfile = FALSE,
  fn_logfile = tempfile(format(Sys.time(), "%Y%m%d_%H%M_"), fileext = ".txt")
) {
  # Validate exepath
  checkmate::assert_character(exepath, len = 1)
  validate_calc_engine_binary(exepath)

  # Valadate outdir
  if (isFALSE(checkmate::test_directory_exists(outdir))) {
    stop("Invalid AGEPRO Output Directory")
  }

  cout <- tryCatch(
    {
      # System Call to AGEPRO Calcuation Engine Binary
      system2(
        command = exepath,
        args = agepro_args,
        stdout = ifelse(save_logfile, TRUE, ""),
        stderr = ""
      )
    },
    error = function(err) {
      message(paste0(
        "Error: \n",
        gsub("\\.$", "", conditionMessage(err))
      ))
    }
  )

  if (isTRUE(save_logfile)) {
    write_logfile(cout, file.path(outdir, fn_logfile))
  }

  return(cout)
}


write_logfile <- function(cout, fn_logfile) {
  #TODO: Verify type of cout

  writeLines(
    c(
      "###",
      "console output",
      as.character(Sys.time()),
      "###",
      " ",
      cout
    ),
    cout
  )
}


#' Validates file path for AGEPRO Calcuation Engine Binary
#'
#' @param exepath AGEPRO Calcuuation Path Binary
#'
validate_calc_engine_binary <- function(
  exepath = file.path(getwd(), "agepro.exe")
) {
  #Validate exepath: The path
  if (isFALSE(checkmate::test_file_exists(exepath, extension = "exe"))) {
    stop("AGEPRO Calcuation Engine Binary was not found")
  }
}
