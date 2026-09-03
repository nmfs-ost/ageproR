#' ageproWrapper
#'
#' ageproWrapper
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
      agepro_path <- path

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

      system2(
        command = self$agepro_path,
        args = args
      )
    },

    #' @description
    #' Runs the Binary with logfile
    #'
    #' @param args Program arguments
    #'
    run_logfile = function(args = character()) {
      if (isFALSE(checkmate::test_file_exists(self$agepro_path))) {
        stop("AGEPRO calcuation execuatble not found.")
      }
      dir_logfile <- getOption("ageproR.dir_logfile")
      if (isFALSE(checkmate::test_directory_exists(dir_logfile))) {
        stop("Invalid 'ageproR.dir_logfile' path.")
      }

      cout <- tryCatch(
        {
          system2(
            command = self$agepro_path,
            args = args,
            stdout = TRUE
          )
        },
        error = function(err) {
          message(paste0(
            "Error: \n",
            gsub("\\.$", "", conditionMessage(err))
          ))
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
    }
  ),
  private = list(
    .agepro_path = NULL,

    # "Multi-tiered" helper method to get AGEPRO binary path
    resolve_binary_path = function(path) {
      # Check "path" exists when class initialized
      if (checkmate::test_file_exists(path)) {
        return(normalizePath(path, mustWork = NA))
      }
      # Check Global Options (.Rprofile)
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
    }
  )
)
