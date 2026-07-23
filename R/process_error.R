#' @title
#' Process Errors for Population and Fishery Processes
#'
#' @description
#' Generalized Class Structure for AGEPRO Keyword parameters who have process
#' errors that generate time-varying dynamics of population and fishery
#' process.
#'
#' @param num_fleets Number of Fleets. Default is 1
#'
#' @template elipses
#' @template inp_con
#' @template nline
#' @template delimiter
#' @template process_error_initialize_params
#' @template enable_cat_print
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#'
#' @export
process_error <- R6Class(
  "process_error",
  public = list(
    #' @description
    #' Initializes the class
    #'
    initialize = function(
      proj_years,
      num_ages,
      num_fleets = 1,
      input_option = 0,
      time_varying = TRUE,
      ...
    ) {
      # set and validate value
      self$input_option <- input_option
      self$time_varying <- time_varying

      # Handle instances where proj_years is passed as projection_years class
      if (checkmate::test_r6(proj_years, public = c("count", "sequence"))) {
        projection_years_class <- proj_years
      } else {
        # Uncommon instance when 'proj_years' is passed as a factor
        if (is.factor(proj_years)) {
          proj_years <- levels(proj_years)
        }

        projection_years_class <-
          ageproR::projection_years$new(as.numeric(proj_years))
      }

      #Initialize parameter and CV tables
      private$setup_parameter_tables(
        projection_years_class,
        num_ages,
        num_fleets,
        time_varying = self$time_varying
      )

      #Fallback Parameter Name
      self$parameter_title <- "Process Error Parameter At Age"
      private$.keyword_name <- "process_error"
      private$.weight_age_parameter <- FALSE
    },

    #' @description
    #' Creates an Population or Fishery process Parameter table
    #'
    #' @param fleet_yr_rows (Fleet-)Year Row
    #' @param ages_cols Age Columns
    #'
    create_parameter_table = function(fleet_yr_rows, ages_cols) {
      return(matrix(
        rep(0, (fleet_yr_rows * ages_cols)),
        nrow = fleet_yr_rows,
        ncol = ages_cols
      ))
    },

    #' @description
    #' Formatted to print out the values of the Process Error Parameter
    #'
    print = function(enable_cat_print = TRUE, ...) {
      #TODO: If (a valid) input_option is -1, -2, -3, or -4 say that
      #this parameter used a "weight-of-age"
      private$print_process_error_fields()

      cli::cli_par()
      cli::cli_alert_info("parameter_table:")
      cli::cli_text("{.emph {self$parameter_title}}")
      #Verbose flag check
      if (enable_cat_print) {
        #Allow `cli::cat_print` message
        print_parameter_table(self$parameter_table, omit_rows = TRUE)
      } else {
        #Suppress `cli::cat_print` message
        capture.output(
          x <- print_parameter_table(
            self$parameter_table,
            omit_rows = TRUE
          )
        )
      }
      cli::cli_end()

      cli::cli_par()
      cli::cli_alert_info("cv_table: ")
      cli::cli_text("{.emph Coefficient of Variation}")
      if (enable_cat_print) {
        #Allow `cli::cat_print` message
        print_parameter_table(self$cv_table, omit_rows = TRUE)
      } else {
        #Suppress `cli::cat_print` message
        capture.output(
          x <- print_parameter_table(
            self$cv_table,
            omit_rows = TRUE
          )
        )
      }

      cli::cli_end()
    },

    #' @description
    #' Reads in Process Error keyword parameter's values from AGEPRO Input file
    #'
    read_inp_lines = function(
      inp_con,
      nline,
      proj_years,
      num_ages,
      num_fleets = 1
    ) {
      cli::cli_alert(paste0(
        "Reading to {.strong {private$.keyword_name}}: ",
        "{self$parameter_title}"
      ))

      nline <- nline + 1
      cli::cli_alert("Line {nline}:")

      # Read an additional line from the file connection
      # and split into 2 substrings
      inp_line <- read_inp_numeric_line(inp_con)

      #Assign input option and time varying
      self$input_option <- inp_line[1]
      self$time_varying <- as.logical(inp_line[2])

      #Validate input option
      checkmate::assert_choice(
        self$input_option,
        private$.valid_input_options,
        .var.name = "Input Option"
      )

      #Verbose output
      cli::cli_div(
        id = "process_error_fields",
        theme = list(".alert-info" = list("margin-left" = 2))
      )
      private$print_process_error_fields()
      cli::cli_end("process_error_fields")

      # TODO: Setup instances where proj_years is passed as a projection_year class
      # Setup new instance of Parameter and CV tables. time_varying
      # value read from the AGEPRO input file. Including values for
      # projection_years. num_ages, and num_fleets.
      private$setup_parameter_tables(
        ageproR::projection_years$new(proj_years),
        num_ages,
        num_fleets,
        time_varying = self$time_varying
      )

      if (self$input_option == 1) {
        #TODO: Read from file name
        stop("NOT IMPLMENTED")
      } else if (self$input_option < 0) {
        return(nline)
      } else {
        #from interface
        nline <- self$read_inp_lines_parameter_tables(inp_con, nline)
        nline <- self$read_inp_lines_cv_table(inp_con, nline)
      }

      return(nline)
    },

    #' @description
    #' Helper function to set population or fishery process parameter
    #' tables from AGEPRO input files. Reads in an additional line (or lines)
    #' from the file connection to assign to the `parameter_table`
    #'
    read_inp_lines_parameter_tables = function(inp_con, nline) {
      #TODO: Verify inp_line is same length as num_ages

      #Non-time varying, single fleet data
      if (private$.num_fleets == 1 && !(self$time_varying)) {
        nline <- nline + 1
        inp_line <- read_inp_numeric_line(inp_con)
        cli::cli_alert(c(
          "Line {nline}: ",
          "parameter_table[\"All Years\", ]:",
          "{.val {inp_line}} ",
          "{.emph ({private$.num_ages} Age{?s})}"
        ))

        self$parameter_table["All Years", ] <- inp_line
      } else {
        #Multi-fleet or Time-Varying Single fleet
        for (i in rownames(self$parameter_table)) {
          nline <- nline + 1
          inp_line <- read_inp_numeric_line(inp_con)
          cli::cli_alert(c(
            "Line {nline}: ",
            "parameter_table[\"{i}\", ]: ",
            "{.val {inp_line}} ",
            "{.emph ({private$.num_ages} Age{?s})}"
          ))

          self$parameter_table[i, ] <- inp_line
        }
      }

      return(nline)
    },

    #' @description
    #' Internal helper function to set cv tables from
    #' AGEPRO input files. Reads in an additional line (or lines) from the
    #' file connection to assign to the `cv_table`
    #'
    read_inp_lines_cv_table = function(inp_con, nline) {
      if (private$.num_fleets == 1) {
        nline <- nline + 1
        inp_line <- read_inp_numeric_line(inp_con)
        cli::cli_alert(c(
          "Line {nline}: ",
          "cv_table[\"All Years\", ]: ",
          "{.val {inp_line}} ",
          "{.emph ({private$.num_ages} Age{?s})}"
        ))

        self$cv_table["All Years", ] <- inp_line
      } else {
        for (i in rownames(self$cv_table)) {
          nline <- nline + 1
          inp_line <- read_inp_numeric_line(inp_con)
          cli::cli_alert(c(
            "Line {nline}: ",
            "cv_table[\"{i}\", ]: ",
            "{.val {inp_line}} ",
            "{.emph ({private$.num_ages} Age{?s})}"
          ))

          self$cv_table[i, ] <- inp_line
        }
      }

      return(nline)
    },

    #' @description
    #' Returns the values for the Process Error parameter formatted
    #' to the AGEPRO input file format.
    get_inp_lines = function(delimiter = "  ") {
      if (self$input_option < 0) {
        if (!private$.weight_age_parameter) {
          stop(paste0(
            "Popluation or Fishing Process Error using ",
            "'Weight of Age' input_options"
          ))
        }

        return(list(
          self$inp_keyword,
          paste(
            self$input_option,
            as.numeric(self$time_varying),
            sep = delimiter
          )
        ))
      }

      return(list(
        self$inp_keyword,
        paste(
          self$input_option,
          as.numeric(self$time_varying),
          sep = delimiter
        ),
        paste(
          apply(self$parameter_table, 1, paste, collapse = delimiter),
          collapse = "\n"
        ),
        paste(
          apply(self$cv_table, 1, paste, collapse = delimiter),
          collapse = "\n"
        )
      ))
    }
  ),
  active = list(
    #' @field input_option
    #' Option to indicate this parameter will be read:
    #' \itemize{
    #'  \item `0` By default, done interactively via interface.
    #'  \item `1` Imported via location of an existing data file.
    #' }
    input_option = function(input_flag) {
      if (missing(input_flag)) {
        private$.input_option
      } else {
        checkmate::assert_integerish(input_flag)
        checkmate::assert_subset(input_flag, private$.valid_input_options)
        private$.input_option <- input_flag
      }
    },

    #' @field time_varying
    #' [Logical][base::logical] flag to list parameter process data by
    #' observation year
    time_varying = function(time_flag) {
      if (missing(time_flag)) {
        private$.time_varying
      } else {
        checkmate::assert_logical(time_flag)
        private$time_varying_toggle_resets_parameter_table(time_flag)
        private$.time_varying <- time_flag
      }
    },

    #' @field parameter_table This is the logic for the fish population or
    #' fishery's processes by age (and by fleet if fleets are a
    #' factor).
    parameter_table = function(value) {
      if (missing(value)) {
        private$.parameter_table
      } else {
        checkmate::assert_matrix(value, min.cols = 1, min.rows = 1)
        #TODO: Create parameter_table value Validation for upper_bound
        private$.parameter_table <- value
      }
    },

    #' @field cv_table Matrix containing the vector of the lognormal process
    #' error of the average population or fishery process parameter's at age
    #' (and by fleet if fleets are a factor).
    cv_table = function(value) {
      if (missing(value)) {
        private$.cv_table
      } else {
        checkmate::assert_matrix(value, min.cols = 1, min.rows = 1)
        private$.cv_table <- value
      }
    },

    #' @field parameter_title
    #' Name of the population or fishery process
    parameter_title = function(value) {
      if (missing(value)) {
        private$.parameter_title
      } else {
        checkmate::assert_character(value)
        private$.parameter_title <- value
      }
    },

    #' @field keyword_name
    #' AGEPRO keyword parameter name
    keyword_name = function() {
      private$.keyword_name
    },

    #' @field inp_keyword
    #' Returns AGEPRO input-file formatted Parameter name
    inp_keyword = function() {
      paste0("[", toupper(private$.keyword_name), "]")
    },

    #' @field json_list_object
    #' Returns JSON list object with Process Error Parameter values
    json_list_object = function() {
      if (self$input_option < 0) {
        if (!private$.weight_age_parameter) {
          stop(paste0(
            "Popluation or Fishing Process Error using ",
            "'Weight of Age' input_options"
          ))
        }
        return(list(
          flag = self$input_option,
          timeflag = as.numeric(self$time_varying)
        ))
      }

      return(list(
        flag = self$input_option,
        timeflag = as.numeric(self$time_varying),
        value = self$parameter_table,
        error = self$cv_table
      ))
    }
  ),
  private = list(
    .input_option = NULL,
    .time_varying = NULL,
    .parameter_datafile = NULL,
    .parameter_table = NULL,
    .cv_table = NULL,
    .upper_bounds = NULL,

    .valid_input_options = c(0, 1),
    .weight_age_parameter = NULL,
    .parameter_title = NULL,
    .keyword_name = NULL,

    .projection_years = NULL,
    .num_ages = NULL,
    .num_fleets = NULL,

    .names_input_option = list(
      "1" = "Import from auxiliary data file",
      "0" = "Read from input data",
      "-1" = "Use (Jan 1st) Stock Weights of Age",
      "-2" = "Use SSB Weights of Age",
      "-3" = "Use (Mid-year) Mean Weights of Age",
      "-4" = "Use Catch Weights of Age"
    ),

    # Initializes Parameter and CV tables
    setup_parameter_tables = function(
      projection_years,
      num_ages,
      num_fleets = 1,
      time_varying = FALSE
    ) {
      #Initialize private values
      private$.projection_years <- projection_years
      private$.num_ages <- num_ages
      private$.num_fleets <- num_fleets

      #Validate parameters
      checkmate::assert_numeric(projection_years$count, lower = 1)
      checkmate::assert_integerish(num_ages, lower = 1)
      checkmate::assert_integerish(num_fleets, lower = 1)

      #initialize tables
      private$.parameter_table <- vector("list", 1)
      private$.cv_table <- vector("list", 1)

      if (time_varying) {
        self$parameter_table <- self$create_parameter_table(
          (projection_years$count * num_fleets),
          num_ages
        )
      } else {
        #All Years
        self$parameter_table <-
          self$create_parameter_table((1 * num_fleets), num_ages)
      }

      self$cv_table <-
        self$create_parameter_table((1 * num_fleets), num_ages)

      #Rownames: Fleet-Years
      # Fleet-year rownames for Parameter of Age table
      rownames(self$parameter_table) <-
        private$setup_parameter_table_rownames(
          projection_years$sequence,
          num_fleets,
          time_varying
        )

      # Fleet-year rownames for CV. Not affected by time varying
      rownames(self$cv_table) <-
        private$setup_parameter_table_rownames(
          projection_years$sequence,
          num_fleets
        )

      #Colnames: Ages
      colnames_ages <- paste0("Age", seq(num_ages))
      colnames(self$parameter_table) <- colnames_ages
      colnames(self$cv_table) <- colnames_ages
    },

    #Rownames: Fleet-Years
    setup_parameter_table_rownames = function(
      proj_years_sequence,
      num_fleets = 1,
      time_varying = FALSE
    ) {
      #Validate num_fleets
      checkmate::check_integerish(num_fleets, lower = 1)

      if (num_fleets > 1) {
        if (time_varying) {
          # Assemble Fleet-years rownames vector by creating a sequence of
          # Fleet and projected_years sequence strings. For fleet-dependent
          # parameters, repeat each unique element of the fleet
          # sequence by the length of the time projection.
          rownames_fleetyears <-
            paste(
              paste0(
                "Fleet",
                rep(seq(num_fleets), each = length(proj_years_sequence))
              ),
              proj_years_sequence,
              sep = "-"
            )
        } else {
          rownames_fleetyears <- paste0("Fleet", seq(num_fleets))
        }
      } else {
        # If num_fleets is 1 && use the projection_years sequence as rownames,
        # Otherwise use the "All years" rowname
        if (time_varying) {
          rownames_fleetyears <- proj_years_sequence
        } else {
          rownames_fleetyears <- "All Years"
        }
      }

      return(rownames_fleetyears)
    },

    #Change in time_varying will reset parameter and CV table
    time_varying_toggle_resets_parameter_table = function(time_flag) {
      if (is.null(private$.time_varying)) {
        return()
      }

      if (time_flag != private$.time_varying) {
        private$setup_parameter_tables(
          private$.projection_years,
          private$.num_ages,
          private$.num_fleets,
          time_varying = time_flag
        )
      }
      return()
    },

    # Function helper that prints the name representing the process error's
    # input option. Also prints time varying.
    print_process_error_fields = function() {
      #Check if input option is valid
      checkmate::assert_choice(
        self$input_option,
        private$.valid_input_options,
        .var.name = "input_option"
      )

      input_option_name <-
        private$.names_input_option[[as.character(self$input_option)]]

      #input options
      cli::cli_alert_info(
        paste0(
          "input_option: {.val {self$input_option}} ",
          "{.emph {.field ({input_option_name})}}"
        )
      )
      #time varying
      cli::cli_alert_info("time_varying: {.val {self$time_varying}}")
    },

    # Function Wrapper to Print out Process Error Info at Initialization
    cli_initialize = function(enable_cat_print = TRUE, ...) {
      div_keyword_header(private$.keyword_name)
      cli_alert("Setting up Default Values")
      self$print(enable_cat_print, ...)
    }
  )
)


#' @title
#' Input information for the natural mortality rate (M) at Age
#'
#' @description
#' Generalized Class Structure for Natural Mortality rate of age
#' AGEPRO Keyword parameter.
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#'
#' @template enable_cat_print
#' @template process_error_initialize_params
#'
#' @export
natural_mortality <- R6Class(
  "natural_mortality",
  inherit = ageproR::process_error,
  public = list(
    #' @description
    #' Initializes the class
    #'
    #'
    initialize = function(
      proj_years,
      num_ages,
      input_option = 0,
      time_varying = TRUE,
      enable_cat_print = TRUE
    ) {
      super$initialize(
        proj_years,
        num_ages,
        1, #Single, non-Fleet dependent parameter.
        input_option,
        time_varying
      )

      self$parameter_title <- "Natural mortality Of Age"
      private$.keyword_name <- "natmort"

      private$cli_initialize(enable_cat_print, omit_rows = TRUE)
    }
  )
)

#' @title
#' Maturation fraction at age
#'
#' @description
#' AGEPRO keyword parameter class Structure for this fishery process with
#' multiplicative lognormal error distribution.
#'
#' @template process_error_initialize_params
#' @template enable_cat_print
#'
#' @importFrom R6 R6Class
#'
#' @export
maturity_fraction <- R6Class(
  "maturity_fraction",
  inherit = ageproR::process_error,
  public = list(
    #' @description
    #' Initializes Class
    #'
    initialize = function(
      proj_years,
      num_ages,
      input_option = 0,
      time_varying = TRUE,
      enable_cat_print = TRUE
    ) {
      super$initialize(proj_years, num_ages, 1, input_option, time_varying)

      self$parameter_title <- "Maturity Fraction at Age"
      private$.keyword_name <- "maturity"

      private$cli_initialize(enable_cat_print, omit_rows = TRUE)
    }
  )
)


#' @title
#' Fishery Selectivity at age by fleet
#'
#' @description
#' Class Structure for Fishery Selectivity at age by fleet AGEPRO Keyword
#' parameter.
#'
#' @param num_fleets Number of Fleets.
#'
#' @template process_error_initialize_params
#' @template enable_cat_print
#'
#' @importFrom R6 R6Class
#'
#' @export
fishery_selectivity <- R6Class(
  "fishery_selectivity",
  inherit = ageproR::process_error,
  public = list(
    #' @description
    #' Initializes new instance
    #'
    initialize = function(
      proj_years,
      num_ages,
      num_fleets,
      input_option = 0,
      time_varying = TRUE,
      enable_cat_print = TRUE
    ) {
      super$initialize(
        proj_years,
        num_ages,
        num_fleets,
        input_option,
        time_varying
      )

      self$parameter_title <- "Fishery Selectivity at age by fleet"
      private$.keyword_name <- "fishery"

      private$cli_initialize(enable_cat_print, omit_rows = TRUE)
    }
  )
)


#' @title
#' Discard fraction of numbers at age
#'
#' @description
#' Class Structure for discard faction at age AGEPRO Keyword parameter.
#' AGEPRO model must indicate _discards are present_, enabled via general
#' options [discards_present field][ageproR::general_params].
#'
#' @param num_fleets Number of Fleets.
#'
#' @template process_error_initialize_params
#' @template enable_cat_print
#'
#' @importFrom R6 R6Class
#'
#' @export
discard_fraction <- R6Class(
  "discard_fraction",
  inherit = ageproR::process_error,
  public = list(
    #' @description
    #' Initializes Class
    #'
    initialize = function(
      proj_years,
      num_ages,
      num_fleets,
      input_option = 0,
      time_varying = TRUE,
      enable_cat_print = TRUE
    ) {
      super$initialize(
        proj_years,
        num_ages,
        num_fleets,
        input_option,
        time_varying,
        enable_cat_print
      )

      self$parameter_title <- "Discards Fraction of Numbers at Age"
      private$.keyword_name <- "discard"

      private$cli_initialize(enable_cat_print, omit_rows = TRUE)
    }
  )
)


#' @title
#' Stock Weights on January 1st at Age
#'
#' @description
#' AGEPRO keyword parameter class Structure for this fishery process with
#' multiplicative lognormal error distribution.
#'
#' @template process_error_initialize_params
#' @template enable_cat_print
#'
#' @importFrom R6 R6Class
#'
#' @export
#'
jan_stock_weight_age <- R6Class(
  "jan_stock_weight_age",
  inherit = ageproR::process_error,
  public = list(
    #' @description
    #' Initializes class
    #'
    initialize = function(
      proj_years,
      num_ages,
      input_option = 0,
      time_varying = TRUE,
      enable_cat_print = TRUE
    ) {
      super$initialize(
        proj_years,
        num_ages,
        1,
        input_option,
        time_varying,
        enable_cat_print
      )

      self$parameter_title <- "Stock Weights on January 1st at Age"
      private$.keyword_name <- "stock_weight"
      private$.weight_age_parameter <- TRUE
      private$.valid_input_options <- c(0, 1)

      private$cli_initialize(enable_cat_print, omit_rows = TRUE)
    }
  )
)


#' @title
#' Spawning stock weight at Age
#'
#' @description
#' AGEPRO keyword parameter class Structure for this population process with
#' multiplicative lognormal error distribution.
#'
#' @template process_error_initialize_params
#' @template enable_cat_print
#'
#' @importFrom R6 R6Class
#'
#' @export
#'
spawning_stock_weight_age <- R6Class(
  "spawning_stock_weight_age",
  inherit = ageproR::process_error,
  public = list(
    #' @description
    #' Initializes class
    #'
    initialize = function(
      proj_years,
      num_ages,
      input_option = 0,
      time_varying = TRUE,
      enable_cat_print = TRUE
    ) {
      super$initialize(
        proj_years,
        num_ages,
        1,
        input_option,
        time_varying,
        enable_cat_print
      )

      self$parameter_title <- "Spawning Stock Weight of Age"
      private$.keyword_name <- "ssb_weight"

      private$.weight_age_parameter <- TRUE
      private$.valid_input_options <- c(0, 1, -1)

      private$cli_initialize(enable_cat_print, omit_rows = TRUE)
    }
  )
)

#' @title
#' Mean (or mid-year) weights at age
#'
#' @description
#' AGEPRO keyword parameter class Structure for this population process with
#' multiplicative lognormal error distribution.
#'
#' @template process_error_initialize_params
#' @template enable_cat_print
#'
#' @importFrom R6 R6Class
#'
#' @export
#'
mean_population_weight_age <- R6Class(
  "mean_population_weight_age",
  inherit = ageproR::process_error,
  public = list(
    #' @description
    #' Initializes class
    #'
    initialize = function(
      proj_years,
      num_ages,
      input_option = 0,
      time_varying = TRUE,
      enable_cat_print = TRUE
    ) {
      super$initialize(
        proj_years,
        num_ages,
        1,
        input_option,
        time_varying,
        enable_cat_print
      )

      self$parameter_title <- "Midyear Mean Population Weight of Age"
      private$.keyword_name <- "mean_weight"

      private$.weight_age_parameter <- TRUE
      private$.valid_input_options <- c(0, 1, -1, -2)

      private$cli_initialize(enable_cat_print, omit_rows = TRUE)
    }
  )
)

#' @title
#' Landed Catch weight at age by fleet
#'
#' @description
#' AGEPRO keyword parameter class Structure for this fishery process with
#' multiplicative lognormal error distribution.
#'
#' @param num_fleets Number of Fleets.
#'
#' @template process_error_initialize_params
#' @template enable_cat_print
#'
#' @importFrom R6 R6Class
#'
#' @export
#'
landed_catch_weight_age <- R6Class(
  "landed_catch_weight_age",
  inherit = ageproR::process_error,
  public = list(
    #' @description
    #' Initializes class
    #'
    initialize = function(
      proj_years,
      num_ages,
      num_fleets,
      input_option = 0,
      time_varying = TRUE,
      enable_cat_print = TRUE
    ) {
      super$initialize(
        proj_years,
        num_ages,
        num_fleets,
        input_option,
        time_varying,
        enable_cat_print
      )

      self$parameter_title <- "Catch Weights at Age by Fleet"
      private$.keyword_name <- "catch_weight"

      private$.weight_age_parameter <- TRUE
      private$.valid_input_options <- c(0, 1, -1, -2, -3)

      private$cli_initialize(enable_cat_print, omit_rows = TRUE)
    }
  )
)

#' @title
#' Discard weight at age by fleet
#'
#' @description
#' AGEPRO keyword parameter class Structure for this fishery process with
#' multiplicative lognormal error distribution.
#'
#' @param num_fleets Number of Fleets.
#'
#' @template process_error_initialize_params
#' @template enable_cat_print
#'
#' @importFrom R6 R6Class
#'
#' @export
#'
discard_weight_age <- R6Class(
  "discard_weight_age",
  inherit = ageproR::process_error,
  public = list(
    #' @description
    #' Initializes class
    #'
    initialize = function(
      proj_years,
      num_ages,
      num_fleets,
      input_option = 0,
      time_varying = TRUE,
      enable_cat_print = TRUE
    ) {
      super$initialize(
        proj_years,
        num_ages,
        num_fleets,
        input_option,
        time_varying,
        enable_cat_print
      )

      self$parameter_title <- "Discard Weights at Age by Fleet"
      private$.keyword_name <- "disc_weight"

      private$.weight_age_parameter <- TRUE
      private$.valid_input_options <- c(0, 1, -1, -2, -3, -4)

      private$cli_initialize(enable_cat_print, omit_rows = TRUE)
    }
  )
)
