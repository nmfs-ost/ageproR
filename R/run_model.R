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
#' inp_file <- system.file("extdata/Example1.INP", package = "ageproR")
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
#' @param out_dir Output path
#' @param append_job_dt Logical option to append the date time stamp to the input files.
#'
launch_model <- function(model, out_dir, append_job_dt = TRUE) {
  # start logging runtime
  job_dt <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

  # Validate agepro_model
  assert_agepro_model_class(model)

  # If out_dir is missing, use <R_USER>/AGEPRO directory as default.
  # Set "AGEPRO" subdirectory of User Home directory (R_USER)
  if (missing(out_dir)) {
    out_dir <- ifelse(
      .Platform$OS.type == "windows",
      normalizePath(
        file.path(Sys.getenv("R_USER"), "AGEPRO"),
        winslash = "/",
        mustWork = FALSE
      ),
      file.path(Sys.getenv("R_USER"), "AGEPRO")
    )
    # If system doesn't have out_dir filepath, create it.
    if (isFALSE(dir.exists(out_dir))) {
      dir.create(out_dir)
      cli::cli_alert("{out_dir} created")
    }
  }

  # TODO: (Limited Character Length) Custom job names.
  # job_dir: <model_job_name>+"_"+(if enabled append_job_dt)<run_dt>
  model_job_name <- paste0(model$case_id$sanitize_case_id_fschar(), "_")

  job_dir <- ifelse(
    append_job_dt,
    file.path(out_dir, model_job_name, job_dt),
    file.path(out_dir, model_job_name)
  )

  # Check inp_file has *.inp

  # Save agepro_model to INP file to run directory (out_dir).

  # Assert that agepro_model bootstrap_file is not NULL.
  # Newly creatated agepro_models will have NULL bootstrap_file values

  # Assert agepro_model bootstrap file exists

  # Check the agepro_model's bootstrap filepath and then copy it to the
  # run directory (out_dir)

  # run_model to AGEPRO calculation engine

  # Create AGEPRO *.out file
}

#' Helper function to check bootstrap file locations prior AGEPRO calcuation engine launch.
#'
#' @param model AGEPRO model class for AGEPRO Input File format (`agepro_inp_model`)
#' @param bsn Bootstrap filepath
#'
model_bootstrap_check <- function(model, bsn) {
  # Check model input file path exists. Note: on
  checkmate::assert_file_exists(model$inp_filepath)

  # Check is validated if bootstrap file exists
  if (checkmate::test_file_exists(bsn)) {
    return()
  }

  # Otherwise, check if bootstrap file path is relative, saved on the
  # same location as the input file: Check the model's input file location
  # is valid. After validation, copy/paste input file dirname to bsn's file.
  # Assuming that the two files are on the same location, the path check
  # will be valid.
}

#' Job directory name setup
#'
#' Helper method that uses the agepro model's data to label the job directory
#' names.
#'
#' @param model Agepro model
#' @param type Option to name to use input model filename or the model's
#' case_id as part of the job_dir name. Defaults to "filename"
#'
#' @return
#' Returns the Character String, the contents dependent on the "type"
#' requested. Using "case_id" returns the case id model name formatted
#' for file directories. NULL or blank "filename" values return `untitled`,
#' otherwise function will return the input "filename" basename without
#' the file extension.
#'
set_job_name <- function(model, type = c("filename", "case_id")) {
  type <- match.arg(type)

  # For "case_id", use case_id$sanitize_case_id_fschar
  if (type == "case_id") {
    return(model$case_id$sanitize_case_id_fschar(), "_")
  }
  # TODO: Pivot agepro_model to be more agonsitic than just inp_filepath
  # Check Model's input model filepath is NULL (typically set with created
  # agepro models), or its a blank empty string. Return as "untitled"
  if (
    checkmate::check_null(model$inp_filepath) ||
      checkmate::test_character(
        model$inp_filepath,
        pattern = "^$|^[[:blank:]]+$"
      )
  ) {
    return("untitled")
  }
  # Otherwise use basename of model's input filename
  return(gsub("(\\.[[:alnum:]]{3})", "", basename(model$inp_filepath)))
}
