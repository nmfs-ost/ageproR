#' R6Class Wrapper for Running AGEPRO Calcuation Engine binary
#'
#' A R6class wrapper that handles exeution of the AGEPRO calcuation binary.
#' This includes varaibles to handle binary path and output logfile, and
#' a function wrapper to run the binary.
#'
#' @export
#'
#' @importFrom R6 R6Class
#'
ageproWrapper <- R6Class(
  "ageproWrapper",
  public = list(
    #' @description
    #' Initializes the binary Wrapper
    #'
    #' @param path Explicit path to directory
    #' @param warn Logical parameter to validate path at initalization. If enabled,
    #' invalid reuslts returns a warning.
    initialize = function(path = NULL, warn = FALSE) {
      # Active binding will passes "path" value through private$get_binary_path
      self$agepro_path <- path

      if (warn) {
        if (isFALSE(checkmate::test_file_exists(agepro_path))) {
          warning(paste0(
            "AGEPRO is not found at: ",
            self$agepro_path,
            "\nPlease install it or configure the path."
          ))
        }
      }
    },

    #' @description
    #' Runs the Binary
    #'
    #' @param args Program agruments
    #'
    run = function(args = character()) {
      if (isFALSE(checkmate::test_file_exists(self$agepro_path))) {
        stop("AGEPRO calcuation execuatble not found.")
      }

      cli::cli_alert_info("Running: {.code {self$agepro_path} {args}}")
      cout <- system2(
        command = self$agepro_path,
        args = args
      )

      #Return AGEPRO output object as an R object
      return(cout)
    },

    #' @description
    #' Runs the Binary with logfile
    #'
    #' This function uses the private function get_logdir
    #' to return the logfile directory. This function will return
    #' the custom output directory if it exists. Otherwise it will
    #' check if user session (.Rprofile) has an existing path. If
    #' `ageproR.run_logdir` does't work it, will fallback to the "AGEPRO"
    #' subirectory of home directory.
    #'
    #' @param args Program arguments
    #' @param out_dir Output directory for the logfile. The default
    #' blank character string will return the AGEPRO subdirectory
    #' of the home directory via get_logdir
    #'
    run_logfile = function(args = character(), out_dir = "") {
      if (isFALSE(checkmate::test_file_exists(self$agepro_path))) {
        stop("AGEPRO calcuation execuatble not found.")
      }
      # Resolve logfile dir
      dir_logfile <- private$get_logdir(out_dir)

      cout <- tryCatch(
        {
          system2(
            command = self$agepro_path,
            args = args,
            stdout = TRUE,
            stderr = TRUE
          )
        },
        error = function(err) {
          msg_err <- paste0(
            "Error: \n",
            gsub("\\.$", "", conditionMessage(err))
          )
          message(msg_err)
          return(msg_err)
        }
      )

      #Write Logfile
      fn_logfile <- normalizePath(
        file.path(
          dir_logfile,
          paste0(format(Sys.time(), "%Y-%m-%d_%H%M"), ".txt")
        ),
        winslash = "\\"
      )

      writeLines(
        c(
          "###",
          "console output",
          as.character(Sys.time()),
          "###",
          " ",
          cout
        ),
        con = fn_logfile
      )

      #Return AGEPRO output object as an R object
      return(cout)
    },

    #' Validate file path for AGEPRO Calcuation Engine Binary
    #'
    #' @param exepath AGEPRO Calcuuation Path Binary
    #'
    validate_binary = function(
      exepath = file.path(getwd(), "agepro.exe")
    ) {
      #Validate exepath: The path
      if (isFALSE(checkmate::test_file_exists(exepath, extension = "exe"))) {
        stop("AGEPRO Calcuation Engine Binary was not found")
      }
    }
  ),
  active = list(
    #' @field agepro_path
    #' AGEPRO Calculation Engine path
    agepro_path = function(value) {
      if (missing(value)) {
        return(private$.agepro_path)
      } else {
        private$.agepro_path <- private$get_binary_path(value)
      }
    },

    #' @field logdir
    #' File path reserved for logfiles for AGEPRO Calcuation Engine runs.
    logdir = function() {
      return(private$.logdir)
    }
  ),
  private = list(
    .agepro_path = NULL,
    .logdir = NULL,

    # "Multi-tiered" helper method to retrieve AGEPRO binary path. From
    # Input custom path, user's ageproR Rsesions option (ageproR.agepro_path),
    # R envronmental values (AGEPRO_PATH) [TODO: Establish a AGEPRO_PATH envriomental value],
    # default path (~/AGEPRO/AGEPRO.exe for windows)
    get_binary_path = function(path) {
      # Check "path" exists when class initialized
      if (checkmate::test_file_exists(path)) {
        return(normalizePath(path, mustWork = NA))
      }
      # Check User Session Options (.Rprofile)
      option_path <- getOption("ageproR.agepro_path")
      if (checkmate::test_file_exists(option_path)) {
        return(normalizePath(option_path, mustWork = NA))
      }
      # Check Enivromental Variables (.Renviron)
      env_path <- Sys.getenv("AGEPRO_PATH")
      if (checkmate::test_file_exists(env_path)) {
        return(normalizePath(env_path, mustWork = NA))
      }

      # Check Default Install Directory
      default_path <- file.path("~", "AGEPRO", "AGEPRO")
      # Windows: Normalize paths with exe
      if (.Platform$OS.type == "windows") {
        default_path <- paste0(default_path, ".exe")
      }

      return(normalizePath(default_path, mustWork = NA))
    },

    # Helper method to retrieve the logfile output directory:
    # From input custom path, user's ageproR setting from .Rprofile
    # (ageproR.model_logdir), or default path ('~/AGEPRO')
    get_logdir = function(logdir) {
      #Input logfile directory path: Check if custom paths exist on system.
      if (checkmate::test_directory_exists(logdir)) {
        private$.logdir <- normalizePath(logdir, mustWork = NA)
        return()
      }
      # User Session Options (.Rprofile)
      rprofile_logdir <- getOption("ageproR.model_logdir")
      if (checkmate::test_directory_exists(rprofile_logdir)) {
        private$.logdir <- normalizePath(rprofile_logdir, mustWork = NA)
        return()
      }

      # Fallback to Default Directory
      default_logdir <- file.path("~", "AGEPRO")
      # Create default_logdir subdirectory at HOME if it doesn't exist
      if (isFALSE(checkmate::test_directory_exists(default_logdir))) {
        cli::cli_alert_info(
          "Creating default_logdir directory at {.val {default_logdir}}"
        )
        dir.create(default_logdir)
      }
      private$.logdir <- normalizePath(default_logdir, mustWork = NA)
      return()
    }
  )
)
