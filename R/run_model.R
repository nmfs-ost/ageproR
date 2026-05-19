#' Run an AGEPRO Input File
#'
#' Function
#'
#' @param exepath Path of the Agepro Calcuation engine
#' @param agepro_args Typically used for AGEPRO Input Files.
#' @param outdir Output path
#' @param save_logfile Option to save AGEPRO Calcuation Engine ouput to logfile
#' @param fn_logfile Logfile Filename
#' @param verbose Option to show verbose output
#' @template elipses
#'
#' @export
#' @examples
#' \dontrun{
#'
#' wdir <- here::here()
#' inpfile <- file.path(wdir,)
#' ageproR::run_model(wdir,"AGEPRO.EXE",)
#' }
run_model <- function(
  exepath = "agepro.exe",
  agepro_args = "",
  outdir = getwd(),
  save_logfile = FALSE,
  fn_logfile = tempfile(format(Sys.time(), "%Y%m%d_%H%M_"), fileext = ".txt"),
  verbose = TRUE,
  ...
) {
  # Valadate outdir
  if (isFALSE(checkmate::test_directory_exists(outdir))) {
    stop("Invalid AGEPRO Output Directory")
  }

  # Validate exepath
  checkmate::assert_character(exepath, len = 1)
  validate_calc_engine_binary(exepath, verbose)

  cout <- tryCatch(
    {
      # System Call to AGEPRO Calcuation Engine Binary
      system2(
        command = exepath,
        args = agepro_args,
        stdout = ifelse(save_logfile, "", TRUE),
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
    write_logfile(cout, file.path(outdir, fn_logfile), verbose)
  }
}


write_logfile <- function(cout, fn_logfile, verbose = TRUE) {
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
  if (verbose) {
    cli::cli_alert_info("Written to ")
  }
}


#' Validates file path for AGEPRO Calcuation Engine Binary
#'
#' @param exepath AGEPRO Calcuuation Path Binary
#' @param verbose Logical option to output to RConsole.
#'
validate_calc_engine_binary <- function(
  exepath = file.path(getwd(), "agepro.exe"),
  verbose = FALSE
) {
  #Validate exepath: The path
  if (isFALSE(checkmate::test_file_exists(exepath, extension = "exe"))) {
    stop("AGEPRO Calcuation Engine Binary was not found")
  }
}
