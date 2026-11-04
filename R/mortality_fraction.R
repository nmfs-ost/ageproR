#' @title
#' Proportion of total mortality occurring prior to spawn in year t
#'
#' @description
#' Class Structure that includes proportion seasonal timing for fishing mortality & natural mortality
#' prior to spawning season
#'
#' @template proj_years_vector
#' @template inp_con
#' @template nline
#' @template enable_cat_print
#'
#' @param time_varying
#' Logical flag that enables the stochastic parameter
#' to use as a time-varying array if TRUE (or 1). Otherwise, FALSE the
#' vector will cover "all years" of the projection. Default is FALSE
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#'
#' @export
mortality_fraction_prior_spawn <- R6Class(
  "mortality_fraction_prior_spawn",
  public = list(
    #' @description
    #' Initializes the class
    #'
    #' @param default_proportion
    #' Proportion default values. Time varying will adjust the values by
    #' number of projection years.
    #'
    initialize = function(
      proj_years_vector,
      time_varying = FALSE,
      default_proportion = 0.5,
      enable_cat_print = TRUE
    ) {
      div_keyword_header(private$.keyword_name)
      cli_alert(paste0(
        "Setting values for total proportion of mortality ",
        "prior to spawn ... "
      ))
      #setup
      private$set_projection_years(proj_years_vector)
      #validate time_varying field
      private$set_time_varying(time_varying, default_proportion)

      self$print(enable_cat_print)
    },

    #' @description
    #' Formatted to print out the values of the Fraction Mortality Parameter
    #'
    #' @template enable_cat_print
    #'
    print = function(enable_cat_print = TRUE) {
      cli::cli_par()
      cli::cli_alert_info(c(
        "time_varying: {.val {private$.time_varying}} ",
        "{.emph ({as.logical(private$.time_varying)})}"
      ))

      if (enable_cat_print) {
        cli::cli_alert_info(paste0(
          "proportion_total_mortality_matrix ",
          "{.emph (Fraction Mortality prior to spawn)}"
        ))
        capture_output_as_message(
          cli::cat_print(private$.proportion_total_mortality_matrix)
        )
      }
      cli::cli_end()
    },

    #' @description
    #' Reads in the values from the keyword parameter BIOLOGICAL from the
    #' AGEPRO Input file
    #'
    read_inp_lines = function(inp_con, nline, proj_years_vector) {
      private$set_projection_years(proj_years_vector)

      cli::cli_alert_info("Reading to {.strong {private$.keyword_name}}")

      nline <- nline + 1
      inp_line <- read_inp_numeric_line(inp_con)

      private$set_time_varying(inp_line)
      cli::cli_alert(c(
        "Line {nline}: ",
        "time_varying: {.var {private$.time_varying}} ",
        "{.emph {as.logical(private$.time_varying)}}"
      ))

      if (isTRUE(private$.time_varying)) {
        names_proj_years_count <- paste0(
          private$.projection_years$count,
          " years"
        )
      } else {
        names_proj_years_count <- "All Years"
      }

      nline <- nline + 1
      tf_inp_line <- read_inp_numeric_line(inp_con)

      cli::cli_alert(paste0(
        "Line {nline}: ",
        "fishing_mortality_prior_spawn: ",
        "{.val {tf_inp_line}} ",
        "{.emph ({names_proj_years_count})}"
      ))

      private$.fishing_mortality_prior_spawn <-
        private$set_fraction_mortality_matrix(
          tf_inp_line,
          private$.time_varying,
          row_names = "fishing_mortality_prior_spawn"
        )

      nline <- nline + 1
      tm_inp_line <- read_inp_numeric_line(inp_con)
      cli::cli_alert(paste0(
        "Line {nline}: ",
        "natural_mortality_prior_spawn: ",
        "{.val {tm_inp_line}} ",
        "{.emph ({names_proj_years_count})}"
      ))

      private$.natural_mortality_prior_spawn <-
        private$set_fraction_mortality_matrix(
          tm_inp_line,
          private$.time_varying,
          row_names = "natural_mortality_prior_spawn"
        )

      cli::cli_alert("Setting proportion_total_mortality_matrix table")
      private$.proportion_total_mortality_matrix <-
        rbind(
          private$.natural_mortality_prior_spawn,
          private$.fishing_mortality_prior_spawn
        )

      return(nline)
    },

    #' @description
    #' Returns values from the mortality_fraction_prior_spawn (BIOLOGICAL)
    #' AGEPRO keyword parameter formatted as AGEPRO input file lines.
    #'
    #' @template delimiter
    #'
    get_inp_lines = function(delimiter = " ") {
      return(c(
        self$inp_keyword,
        self$time_varying,
        # Format matrix to lines of text
        paste(
          apply(
            self$proportion_total_mortality_matrix,
            1,
            paste,
            collapse = delimiter
          ),
          collapse = "\n"
        )
      ))
    }
  ),
  active = list(
    #' @field time_varying
    #' [Logical][base::logical] flag to list fishing and natural mortality per
    #' observation year if TRUE or representative of the
    time_varying = function(value) {
      if (missing(value)) {
        return(private$.time_varying)
      }
      private$set_time_varying(value)
    },

    #' @field proportion_total_mortality_matrix
    #' Proportion of total mortality occurring prior to spawning
    proportion_total_mortality_matrix = function(value) {
      if (missing(value)) {
        return(private$.proportion_total_mortality_matrix)
      }

      validation_error <- checkmate::makeAssertCollection()
      ncols_proportion_total_mortality_matrix <-
        ifelse(
          as.logical(private$.time_varying),
          private$.projection_years$count,
          1
        )

      checkmate::assert_matrix(
        value,
        ncols = ncols_proportion_total_mortality_matrix,
        add = validation_error
      )
      value |>
        map_errors(
          \(value) {
            checkmate::assert_numeric(
              value,
              upper = 1,
              lower = 0,
              add = validation_error
            )
          }
        )

      checkmate::reportAssertions(validation_error)

      private$.proportion_total_mortality_matrix <- value

      #Set matrix values to fishing and natural mortality proportions
      private$.natural_mortality_prior_spawn <-
        private$.proportion_total_mortality_matrix[
          "natural_mortality_prior_spawn",
        ]

      private$.fishing_mortality_prior_spawn <-
        private$.proportion_total_mortality_matrix[
          "fishing_mortality_prior_spawn",
        ]
    },

    #' @field natural_mortality_prior_spawn
    #' Returns the proportions of Natural mortality (TM) that occurs from
    #' January 1st to Spawning Season.
    #'
    natural_mortality_prior_spawn = function(value) {
      if (isFALSE(missing(value))) {
        stop(
          paste0(
            "Can not set natural_mortality_prior_spawn directly. \n",
            "Set natural_mortality_prior_spawn row values to the \n",
            "proportion_total_mortality_matrix."
          ),
          call. = FALSE
        )
      }
      #return(private$.natural_mortality_prior_spawn)
      return(private$.proportion_total_mortality_matrix[
        "natural_mortality_prior_spawn",
      ])
    },

    #' @field fishing_mortality_prior_spawn
    #' Returns within-year fractions of fishing mortality (TF) that occurs from
    #' January 1st to Spawning Season.
    #'
    fishing_mortality_prior_spawn = function(value) {
      if (isFALSE(missing(value))) {
        stop(
          paste0(
            "Can not set fishing_mortality_prior_spawn directly. \n",
            "Set fishing_mortality_prior_spawn row values to the \n",
            "proportion_total_mortality_matrix."
          ),
          call. = FALSE
        )
      }
      #return(private$.fishing_mortality_prior_spawn)
      return(private$.proportion_total_mortality_matrix[
        "fishing_mortality_prior_spawn",
      ])
    },

    #' @field json_list_object
    #' Returns JSON list object of values from the
    #' mortality_fraction_prior_spawn class
    json_list_object = function() {
      return(list(
        zfrac_time_flag = private$.time_varying,
        fishing_mortality_prior_spawn = as.vector(
          private$.fishing_mortality_prior_spawn
        ),
        natural_mortality_prior_spawn = as.vector(
          private$.natural_mortality_prior_spawn
        )
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
    },

    #' @field projection_years
    #' Returns the projection_years used in the mortaliy_fraction object
    projection_years = function() {
      return(private$.projection_years)
    }
  ),
  private = list(
    .keyword_name = "biological",

    .projection_years = NULL,
    .time_varying = NULL,
    .natural_mortality_prior_spawn = NULL,
    .fishing_mortality_prior_spawn = NULL,
    .proportion_total_mortality_matrix = NULL,

    set_time_varying = function(value, ...) {
      #Convert logical values as numeric
      if (checkmate::test_logical(value)) {
        value <- as.numeric(value)
      }

      validation_error <- checkmate::makeAssertCollection()
      checkmate::assert_numeric(value, add = validation_error)
      checkmate::assert_choice(value, choices = c(0, 1), add = validation_error)
      checkmate::reportAssertions(validation_error)

      # Check input 'value' is the same as time_varying field being overridden
      field_changed <- isFALSE(identical(private$.time_varying, value))

      private$.time_varying <- value

      if (field_changed) {
        private$set_default_fraction_mortality_matrix(...)
      }
    },

    # Handle proj_years that may be a single int or sequential numeric vector
    set_projection_years = function(value) {
      # Handle instances where value is passed as projection_years class
      if (checkmate::test_r6(value, public = c("count", "sequence"))) {
        private$.projection_years <- value$clone(deep = TRUE)
        invisible(value)
      }

      checkmate::assert_numeric(value)
      private$.projection_years <-
        ageproR::projection_years$new(as.numeric(value))
    },

    # Creates matrix object vector for natural_mortality_before_spawn and
    # fishing_mortality_before_spawn
    set_fraction_mortality_matrix = function(
      value,
      time_varying,
      row_names = NULL
    ) {
      #Validate if value is proportional value
      checkmate::assert_numeric(value, lower = 0, upper = 1)

      if (time_varying) {
        # Add prefix "y" to column names for projection years
        colnames_projection_years <- paste0(
          "y",
          private$.projection_years$sequence
        )
        return(
          matrix(
            value,
            nrow = 1,
            ncol = private$.projection_years$count,
            dimnames = list(row_names, colnames_projection_years)
          )
        )
      } else {
        return(
          matrix(
            value,
            nrow = 1,
            ncol = 1,
            dimnames = list(row_names, "All Years")
          )
        )
      }
    },

    set_default_fraction_mortality_matrix = function(default_proportion = 0.5) {
      #Validation
      validation_error <- checkmate::makeAssertCollection()
      checkmate::assert_numeric(
        private$.projection_years$count,
        add = validation_error
      )
      checkmate::assert_numeric(private$.time_varying, add = validation_error)
      checkmate::reportAssertions(validation_error)

      if (private$.time_varying) {
        default_proportion <-
          rep(
            default_proportion / private$.projection_years$count,
            private$.projection_years$count
          )
      }

      private$.natural_mortality_prior_spawn <-
        private$set_fraction_mortality_matrix(
          default_proportion,
          private$.time_varying,
          "natural_mortality_prior_spawn"
        )

      private$.fishing_mortality_prior_spawn <-
        private$set_fraction_mortality_matrix(
          default_proportion,
          private$.time_varying,
          "fishing_mortality_prior_spawn"
        )

      private$.proportion_total_mortality_matrix <-
        rbind(
          private$.natural_mortality_prior_spawn,
          private$.fishing_mortality_prior_spawn
        )
    }
  )
)
