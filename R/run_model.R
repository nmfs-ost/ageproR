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


#' Log AGEPRO Ouput to File
#'
#' Saves AGEPRO Output to Logfile
#'
#' @param cout Output connection from console
#' @param fn_logfile Filename path to save output
#'
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

#' Launches agepro_model to the calcuation engine
#'
#' This function will takes the [agepro_inp_model][ageproR::agepro_inp_model] class,
#' saves it as an AGEPRO input file, launches the AGEPRO calcuation engine with it.
#'
#' After the calcuation engine is done, AGEPRO output file will be saved to the output
#' directory. Function will also store AGEPRO calcuation engine logfile if requested.
#'
#' @details
#' This is simlar to how AGEPRO_GUI launches agepro_model to the AGEPRO calcuation engine. The bootstrap file path written
#' in the AGEPRO Input File is relative to the directory that input file is saved; The bootstrap filepath value does not have
#' to have directory paths if the Input file and bootstrap file is saved in the same directory.
#'
#' The "default directory" of the outdir will be saved at the "AGEPRO" subdirectory of the `R_USER` directory.
#'
#' @param model ["Agepro INP File Model Class Object"][ageproR::agepro_inp_model]
#' @param outdir Output path
#'
launch_model <- function(model, out_dir) {
  #start logging runtime
  run_dt <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

  #Validate agepro_model
  assert_agepro_model_class(model)

  #Assert agepro_model bootstrap file exists

  #Set default AGEPRO output directory if out_dir is missing.
  #Set "AGEPRO" subdirectory of User Home directory (R_USER)
  if (missing(out_dir)) {
    out_dir <- ifelse(
      .Platform$OS.type == "windows",
      normalizePath(file.path(Sys.getenv("R_USER"), "AGEPRO"), winslash = "/"),
      file.path(Sys.getenv("R_USER"), "AGEPRO")
    )
    if (isFALSE(dir.exists(out_dir))) {
      dir.create(out_dir)
      cli::cli_alert("{out_dir} created")
    }
  }

  #Check if Model's CASE_ID is blank
  blank_caseid <- checkmate::test_character(
    model$case_id$model_name,
    pattern = "^$|^[:blank:]]+$",
    null.ok = FALSE
  )

  if (checkmate::test_character(blank_caseid)) {
    inp_file <- "untitled_"
  } else {
    regex_invalid_fschars <- '[\\/:*?"<>|-]'
    #Check if inp_file has invalid char pattterns.
    if (
      checkmate::test_character(
        model$case_id$model_name,
        pattern = regex_invalid_fschars
      )
    ) {
      #TODO: Give a option to replace invalid char, or to give an error.
      msg_regex_invalid_fschars <- trimws(gsub(
        "\\[*\\]*\\\\",
        " ",
        regex_invalid_fschars
      ))

      stop(paste0(
        "Model Case Id has an invalid character: '",
        msg_regex_invalid_fschars,
        "'"
      ))
    }
    inp_file <- inp_file
  }

  #Set INP and BSN (Set run directory)

  #Check for Bootstrap? (Set BSN to out_dir)

  #Save to INP file?

  #run AGEPRO

  #AGEPRO output
  #create *.out file
}
