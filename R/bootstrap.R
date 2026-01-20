
#' @title Input information for bootstrap numbers at age file
#'
#' @description
#' -The Number of data values of each row must equal to the number of age
#' classes.
#' -The number of rows in a bootstrap file must be at least equal to the number
#' of bootstrap iterations containing the population of the first year in
#' the projection
#'
#' @template inp_con
#' @template nline
#' @template elipses
#' @template delimiter
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom checkmate assert_numeric test_file_exists
bootstrap <- R6Class(
  "bootstrap",
  public = list(

    #' @description
    #' Initializes the Bootstrap Class
    #'
    initialize = function() {

      div_keyword_header(self$keyword_name)
      cli_alert("Setting up Default Values")

      self$num_bootstraps <- 0
      self$pop_scale_factor <- 0

      suppressWarnings(self$bootstrap_file <- NULL)

      self$print()

    },

    #' @description
    #' Uses file dialog interface to retrieve Bootstrap file name
    #'
    #' @param bsn_path Bootstrap Filename (*.bsn) path
    set_bootstrap_filename = function(bsn_path) {

      if(missing(bsn_path)){
        bsn_path <-
            open_file_dialog(c("AGEPRO Bootstrap File", ".bsn"))
      }

      if (test_file_exists(bsn_path, access = "r", extension = "bsn")) {
        self$bootstrap_file <- bsn_path
      }else{
        local({
          #Disable cli hyperlinking
          withr::local_options(cli.hyperlink = FALSE)
          cli::cli_alert_danger(
            "Falied to reconzise as bootstrap file: {.path {bsn_path}}.")
        })

      }

    },

    #' @description
    #' Reads in BOOTSTRAP numbers and options from AGEPRO Input file
    #'
    read_inp_lines = function(inp_con, nline) {
      #Read an additional line from the file connection,
      #and split the line into 2 substrings, and ...
      inp_line <- read_inp_numeric_line(inp_con)

      #Assign substrings
      self$num_bootstraps <- inp_line[1]
      self$pop_scale_factor <- inp_line[2]

      nline <- nline + 1
      cli_alert("Line {nline}: ")
      cli::cli_div(id = "bootstrap_fields",
                   theme = list(".alert-info" = list("margin-left" = 2)))
      cli::cli_alert_info("num_bootstraps: {.val {self$num_bootstraps}}")
      cli::cli_alert_info(paste0("pop_scale_factor {.emph (BootFac)}: ",
                                 "{.val {self$pop_scale_factor}}"))
      cli::cli_end("bootstrap_fields")

      #Read another line from the file connection, and
      #assign it as bootstrap filename
      nline <- nline + 1
      suppressMessages(invisible(capture.output(
        self$bootstrap_file <- readLines(inp_con, n = 1, warn = FALSE))))
      cli::cli_alert(paste0("Line {nline}: bootstrap_file: ",
                            "{.val {self$bootstrap_file}}"))


      return(nline)

    },

    #' @description
    #' Returns BOOTSTRAP values AGEPRO input file format (*,inp)
    #'
    get_inp_lines = function(delimiter = " ") {
      #Warn if bootstrap file does not exists on system
      if (!test_file_exists(self$bootstrap_file)) {
        warning("Bootstrap filename does not exist on system.", call. = FALSE)
      }
      return(list(
        self$inp_keyword,
        paste(self$num_bootstraps, self$pop_scale_factor, sep = delimiter),
        self$bootstrap_file
      ))
    },

    #' @description
    #' Prints out BOOTSTRAP fields
    #'
    print = function(...) {
      cli::cli_par()
      cli_alert_info("num_bootstraps: {.val {self$num_bootstraps}}")
      cli_alert_info(paste0("pop_scale_factor {.emph (BootFac)}: ",
               "{.val {self$pop_scale_factor}}"))
      cli_alert_info(paste0("bootstrap_file:",
                            "{.val {self$bootstrap_file}}"))

      cli_end()
    }



  ), active = list(

    #' @field num_bootstraps
    #' Number of bootstraps replicates of initial popualion size
    num_bootstraps = function(value) {
      if (missing(value)) {
        private$.num_bootstraps
      }else {
        assert_numeric(value, lower = 0)
        private$.num_bootstraps <- value
      }
    },

    #' @field pop_scale_factor
    #' Population Scale Factor, or BootFac, that represents the multiplicative
    #' factor to convert the relative bootstrap population numbers at age to
    #' absolute numbers at age.
    pop_scale_factor = function(value) {
      if (missing(value)) {
        private$.pop_scale_factor
      }else {
        assert_numeric(value, lower = 0)
        private$.pop_scale_factor <- value
      }
    },

    #' @field bootstrap_file
    #' Bootstrap file path.
    bootstrap_file = function(value) {
      if (missing(value)) {
        private$.bootstrap_file
      }else {
        #Validate that 'value' points to a existing bootstrap file.
        private$validate_bootstrap_file(value)
        private$.bootstrap_file <- value
      }
    },

    #' @field json_list_object
    #' JSON list object for BOOTSTRAP keyword parameter
    #'
    json_list_object = function() {
      return(list(
        nboot = self$num_bootstraps,
        bootFac = self$pop_scale_factor,
        bootFile = self$bootstrap_file
      ))
    },

    #' @field keyword_name
    #' AGEPRO keyword parameter name
    keyword_name = function() {
      private$.keyword_name
    },

    #' @field inp_keyword
    #' Returns AGEPRO input-file formatted Parameter
    inp_keyword = function() {
      paste0("[",toupper(private$.keyword_name),"]")
    }

  ),
  private = list(

    .num_bootstraps = NULL,
    .pop_scale_factor = NULL,
    .bootstrap_file = NULL,
    .keyword_name = "bootstrap",

    #Validate bootstrap_file
    validate_bootstrap_file = function(value) {

      #Validate that 'value' points to a existing file.
      if (test_file_exists(value, access = "r", extension = "bsn")) {
        #If validated, assign value
        cli_alert_success("bootstrap_file: {.val {value}}")
        return()
      }

      #Warn if file path is NULL
      if (is.null(value)) {
        warning(paste0("NULL bootstrap_file path. Please provide valid ",
                       "bootstrap (*.bsn) filepath before saving input file ",
                       "or running model to calcuation engine."),
                call. = FALSE)
        return()
      }

      #Else, warn bootstrap file name does not exist
      warning(paste0(invalid_path_message(value),
                     "Please save AGEPRO input files with a vaild ",
                     "bootstrap_file, especially with running models with ",
                     "the calculation engine."),
              call. = FALSE)
    }

  )

)
