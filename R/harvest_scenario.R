#' @title
#' Harvest intensity (of fishing mortality or landings quota) by fleet
#'
#' @description
#' Class Structure containing the Harvest values and Harvest Specifications
#'
#' @template elipses
#' @template inp_con
#' @template nline
#' @template enable_cat_print
#'
#' @param proj_years [Projection years][ageproR::projection_years]:
#' Input can be Sequence of years in from first to last year of
#' projection or the number of years in the time projection.
#' @param num_fleets Number of Fleets. Default is 1
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#'
#' @keywords projection_analyses
#'
#' @export
#'
harvest_scenario <- R6Class(
  "harvest_scenario",
  public = list(
    #' @description
    #' Initializes Class
    #'
    #' @param projection_years [Projection years][ageproR::projection_years]:
    #' Input can be Sequence of years in from first to last year of
    #' projection or the number of years in the time projection.
    #' @param num_fleets Number of Fleets. Default is 1
    #'
    #'
    initialize = function(projection_years, num_fleets = 1, ...) {
      # 'recruit' cli messages at initialization
      div_keyword_header(self$keyword_name)
      cli_alert("Creating Default Harvest Scenario Values ,,,")

      self$setup_harvest_scenario_variables(projection_years, num_fleets)
      self$print(...)
    },

    #' @description
    #' Formatted to print out the Harvest Scenario Table
    #'
    print = function(enable_cat_print = TRUE) {
      cli::cli_par()
      cli::cli_alert_info("harvest_scenario_table")
      #Verbose flag check
      if (enable_cat_print) {
        #Allow `cli::cat_print` message
        print_parameter_table(self$harvest_scenario_table, omit_rows = TRUE)
      } else {
        #Suppress `cli::cat_print` message
        capture.output(
          x <- print_parameter_table(
            self$harvest_scenario_table,
            omit_rows = TRUE
          )
        )
      }
      cli::cli_end()
    },

    #' @description
    #' Helper function to setup harvest_scenario's variables
    #' harvest_specifications, harvest_value, harvest_scenario_table
    #'
    #' @param proj_years [Projection years][ageproR::projection_years] object
    #'
    #' @param num_fleets Number of Fleets. Defaults to 1
    #'
    setup_harvest_scenario_variables = function(proj_years, num_fleets = 1) {
      #Validate parameters
      if (checkmate::test_r6(proj_years, public = c("count", "sequence"))) {
        proj_years_class <- proj_years
      } else {
        proj_years_class <- ageproR::projection_years$new(proj_years)
      }
      checkmate::assert_numeric(proj_years_class$count, lower = 1)
      checkmate::assert_integerish(num_fleets, lower = 1)

      #Initialize private values
      private$.projection_years <- proj_years_class
      private$.num_fleets <- num_fleets

      #initialize tables
      private$.harvest_specifications <- vector("list", 1)
      private$.harvest_value <- vector("list", 1)
      private$.harvest_scenario_table <- vector("list", 1)

      # Create harvest_specifications w/ default value
      self$harvest_specifications <-
        matrix(
          rep(0, proj_years_class$count),
          nrow = proj_years_class$count,
          ncol = 1,
          dimnames = list(private$.projection_years$sequence, "specification")
        )

      harvest_value_colnames <-
        private$setup_harvest_value_colnames(private$.num_fleets)

      self$harvest_value <-
        create_blank_parameter_table(
          num_rows = proj_years_class$count,
          num_cols = num_fleets,
          dimnames = list(
            private$.projection_years$sequence,
            harvest_value_colnames
          ),
          useNA = FALSE
        )

      private$.harvest_scenario_table <-
        cbind(self$harvest_specifications, self$harvest_value)
    },

    #' @description
    #' Reads in Harvest Scenario keyword parameter's values from the
    #' AGEPRO Input file
    #'
    read_inp_lines = function(inp_con, nline, proj_years, num_fleets = 1) {
      #Setup new instances of harvest_scenario values
      self$setup_harvest_scenario_variables(proj_years, num_fleets)

      cli::cli_alert("Reading to {.strong {private$.keyword_name}}")

      nline <- nline + 1

      # Read an additional line from the file connection and delimit into
      # substring and assign to harvest_specifications
      inp_line <- read_inp_numeric_line(inp_con)

      self$harvest_specifications[, 1] <- inp_line

      cli::cli_alert(c(
        "Line {nline}: ",
        "harvest_specifications: ",
        "{.val {self$harvest_specifications}} ",
        paste0(
          "{.emph ({private$.projection_years$count} ",
          "projection years)}"
        )
      ))

      for (i in 1:num_fleets) {
        nline <- nline + 1

        inp_line <- read_inp_numeric_line(inp_con)
        #Ensure inp_line substrings matches number of projection years
        checkmate::assert_numeric(
          inp_line,
          len = private$.projection_years$count,
          .var.name = "inp_line"
        )

        cli::cli_alert(c(
          "Line {nline}: ",
          "harvest_value (",
          "{i} of {private$.num_fleets} fleet{?s}): ",
          "{.val {inp_line}} ",
          paste0(
            "{.emph ({private$.projection_years$count} ",
            "Projection Year{?s})}"
          )
        ))

        self$harvest_value[, i] <- inp_line
      }

      private$.harvest_scenario_table <-
        cbind(self$harvest_specifications, self$harvest_value)
      cli::cli_alert(paste0(
        "Created {.strong harvest_scenario_table} ",
        "with {.strong harvest_specifications} ",
        "and {.strong harvest_value} values"
      ))

      return(nline)
    },

    #' @description
    #' Returns Harvest Specification and Harvest Values from the Harvest
    #' Scenario parameter formatted to the AGEPRO input file format.
    #'
    #' @template delimiter
    #'
    get_inp_lines = function(delimiter = " ") {
      # Check harvest_specifications and harvest_value is not NULL
      checkmate::assert_vector(
        self$harvest_specifications,
        .var.name = "harvest_specifications"
      )
      checkmate::assert_vector(self$harvest_value, .var.name = "harvest_value")

      return(c(
        self$inp_keyword,
        unname(apply(
          self$harvest_specifications,
          2,
          paste,
          collapse = delimiter,
          simplify = FALSE
        )),
        unname(apply(
          self$harvest_value,
          2,
          paste,
          collapse = delimiter,
          simplify = FALSE
        ))
      ))
    }
  ),
  active = list(
    #' @field harvest_specifications
    #' Contains values the Harvest Specification per projection year
    #' \describe{
    #'  \item{"0"}{F-MULT}
    #'  \item{"1"}{LANDINGS}
    #'  \item{"2"}{REMOVALS}
    #' }
    #'
    harvest_specifications = function(value) {
      if (missing(value)) {
        private$.harvest_specifications
      } else {
        checkmate::assert_matrix(
          value,
          ncols = 1,
          min.rows = 1,
          .var.name = "harvest_specifications"
        )
        # Validate harvest specification values
        private$assert_specification_type(value)
        private$.harvest_specifications <- value
      }
    },

    #' @field harvest_value
    #' Contains the harvest Amount per projection year. Can be fleet specific
    #' if more than 2 fleets are specified.
    harvest_value = function(value) {
      if (missing(value)) {
        private$.harvest_value
      } else {
        checkmate::assert_matrix(
          value,
          mode = "numeric",
          min.cols = 1,
          min.rows = 1,
          .var.name = "harvest_value"
        )
        private$.harvest_value <- value
      }
    },

    #' @field harvest_scenario_table
    #' Combines the Harvest specification and (fleet) harvest amount per
    #' projection year.
    harvest_scenario_table = function(value) {
      #TODO: Show Print harvest_specifications column as
      #F-MULT, LANDINGS, REMOVALS

      private$.harvest_scenario_table
    },

    #' @field json_list_object
    #' Returns JSON list object with Harvest Specification and Harvest Values
    json_list_object = function() {
      return(list(
        spec = c(self$harvest_specifications),
        value = unname(apply(
          self$harvest_value,
          2,
          as.vector,
          simplify = FALSE
        ))
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
      paste0("[", toupper(private$.keyword_name), "]")
    }
  ),
  private = list(
    .keyword_name = "harvest",

    .harvest_specifications = NULL,
    .harvest_value = NULL,
    .harvest_scenario_table = NULL,

    .names_specification = list(
      "0" = "F-MULT",
      "1" = "LANDINGS",
      "2" = "REMOVALS"
    ),

    #setup variables at initialization
    .projection_years = NULL,
    .num_fleets = NULL,

    # assert_specification_type
    #
    # Checks the harvest specification values matches valid specification
    # types
    assert_specification_type = function(x) {
      checkmate::assert_matrix(
        x,
        ncols = 1,
        .var.name = "harvest_specifications"
      )

      specification_types <- names(private$.names_specification)
      result_spec_match <- as.numeric(x[, 1]) %in% specification_types

      # Throw error message if any numeric is outside the specification type
      # range: {0, 1, 2}
      spec_match_error_msg <-
        paste0(
          "Invalid harvest specfication found: ",
          cli::ansi_collapse(
            x[!(result_spec_match)],
            trunc = 5,
            style = "head"
          ),
          ". Must be within {",
          paste0(specification_types, collapse = ", "),
          "}"
        )

      if (isFALSE(all(result_spec_match))) {
        stop(spec_match_error_msg)
      }
    },

    # setup_harvest_value_colnames
    #
    # Helper function that returns the column names of the Harvest Value
    # table-like matrix determined by single or multiple fleets
    setup_harvest_value_colnames = function(num_fleets = 1) {
      #Validate
      checkmate::assert_numeric(num_fleets, low = 1, .var.name = "num_fleets")

      #Check if Single or Multi Fleet
      if (isTRUE(identical(private$.num_fleets, 1))) {
        harvest_value_colnames <- "harvest_value"
      } else {
        harvest_value_colnames <- paste0("FLEET-", 1:private$.num_fleets)
      }

      return(harvest_value_colnames)
    }
  )
)
