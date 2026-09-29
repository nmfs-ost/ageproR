#' R6Class Wrapper for Running AGEPRO Calcuation Engine binary
#'
#' A R6class wrapper that handles exeution of the AGEPRO calcuation binary.
#' This includes varaibles to handle binary path and output logfile, and
#' a function wrapper to run the binary.
#'
#' @export
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
      # Active binding will passes "path" value through private$resolve_binary_path
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
    #' This function uses the private function resolve_logdir
    #' to return the logfile directory. This function will return
    #' the custom output directory if it exists. Otherwise it will
    #' check if user session (.Rprofile) has an existing path. If
    #' `ageproR.run_logdir` does't work it, will fallback to the "AGEPRO"
    #' subirectory of home directory.
    #'
    #' @param args Program arguments
    #' @param out_dir Output directory for the logfile. The default
    #' blank character string will return the AGEPRO subdirectory
    #' of the home directory via resolve_logdir
    #'
    run_logfile = function(args = character(), out_dir = "") {
      if (isFALSE(checkmate::test_file_exists(self$agepro_path))) {
        stop("AGEPRO calcuation execuatble not found.")
      }
      # Resolve logfile dir
      dir_logfile <- private$resolve_logdir(out_dir)

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
    }
  ),
  active = list(
    #' @field agepro_path
    #' AGEPRO Calculation Engine path
    agepro_path = function(value) {
      if (missing(value)) {
        return(private$.agepro_path)
      } else {
        private$.agepro_path <- private$resolve_binary_path(value)
      }
    },

    #' @field logdir
    #' File path reserved for logfiles for AGEPRO Calcuation Engine runs.
    logdir = function(value) {
      if (missing(value)) {
        return(private$.logdir)
      } else {
        private$.logdir <- private$resolve_logdir(value)
      }
    }
  ),
  private = list(
    .agepro_path = NULL,
    .logdir = NULL,

    # "Multi-tiered" helper method to get AGEPRO binary path
    resolve_binary_path = function(path) {
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

    resolve_logdir = function(logdir) {
      #Input logfile directory path: Check if custom paths exist on system.
      if (checkmate::test_file_exists(logdir)) {
        return(normalizePath(logdir, mustWork = NA))
      }
      # User Session Options (.Rprofile)
      rprofile_logdir <- getOption("ageproR.run_logdir")
      if (checkmate::test_directory_exists(rprofile_logdir)) {
        return(normalizePath(rprofile_logdir, mustWork = NA))
      }

      # Fallback to Default Directory
      default_logdir <- file.path("~", "AGEPRO")
      return(normalizePath(default_logdir, mustWork = NA))
    }
  )
)
