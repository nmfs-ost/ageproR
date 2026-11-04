#' @title
#' Vector of retrospective bias-correction coefficients to adjust
#' to the initial population of numbers of age.
#'
#' @description
#' This is the vector of age-specific numbers at age multipliers for an
#' initial population size at age vector if retrospective bias adjustment
#' is applied.
#'
#' The logical flag `enable_retrospective_adjustments` allows the user to
#' set values to this class fields. The flag will also notify `agepro_model`
#' if this keyword parameter is allowed to be written to input file.
#'
#' If this class is initialized with default values, it is presumed that this
#' keyword parameter is not used in the agepro_model. Therefore,
#' `enable_retrospective_adjustments` is flagged as FALSE. Valid non-default
#' values will set this flag to TRUE.
#'
#' @details
#' The retrospective_adjustments class (`RETROADJUST`) is recognized as a keyword
#' parameter used in the AGEPRO input file format, but it is optional.
#'
#' @include options_flags.R
#'
#' @export
#' @importFrom R6 R6Class
#'
retrospective_adjustments <- R6Class(
  "retrospective_adjustments",
  public = list(
    #' @description
    #' Initializes the class
    #'
    #' @template enable_cat_print
    #' @param retro_adjust
    #' Vector for retrospective bias adjustment
    #'
    #' @param retro_flag
    #' R6class containing option flags to allow retrospective adjustments
    #' to be used
    #'
    initialize = function(
      retro_adjust,
      enable_cat_print = TRUE,
      retro_flag = NULL
    ) {
      #TODO: Include private (max) num_ages parameter to limit

      div_keyword_header(private$.keyword_name)

      # Validation checks in case retrospective_adjustments is initialized w/
      # non-null or invalid enable_retrospective_adjustments
      private$validate_retro_flag(retro_flag)

      #If retro_adjust is missing, assume default values.
      if (missing(retro_adjust)) {
        cli::cli_alert(paste0(
          "Setting retrospective_adjustments ",
          "default values ... "
        ))

        self$retro_adjust <- 0

        private$set_enable_retrospective_adjustments(FALSE)
      } else {
        cli::cli_alert("Setting retrospective_adjustments values: ")
        self$retro_adjust <- retro_adjust

        private$set_enable_retrospective_adjustments(TRUE)
      }
    },

    #' @description
    #' Formatted to print out retrospective_adjustments values
    #'
    #' @template enable_cat_print
    #' @template elipses
    #'
    print = function(enable_cat_print = TRUE, ...) {
      cli::cli_alert_info(
        paste0(
          "enable_retrospective_adjustments: ",
          "{.emph (Specify Retrospective Adjustment factors)}: ",
          "{.val {self$enable_retrospective_adjustments}}"
        )
      )
      cli::cli_ul(id = "retrospective_adjustments_fields")
      cli::cli_alert_info("retro_adjust: ")

      #Verbose flag check
      if (enable_cat_print) {
        #Allow `cli::cat_print` message
        print_parameter_table(self$retro_adjust, omit_rows = FALSE)
      } else {
        #Suppress `cli::cat_print` message
        capture.output(
          x <- print_parameter_table(self$retro_adjust, omit_rows = FALSE)
        )
      }

      cli::cli_end()
    },

    #' @description
    #' Reads in the values from the keyword parameter RETROADJUST from the
    #' AGEPRO Input file
    #'
    #' Note: enable_retrospective_adjustments must be set to TRUE.
    #'
    #' @template inp_con
    #' @template nline
    #' @param num_ages Model's number of ages derived from general_params
    #' num_ages active binding.
    #'
    read_inp_lines = function(inp_con, nline, num_ages) {
      if (isFALSE(self$enable_retrospective_adjustments)) {
        stop(private$unenabled_options_flag_message())
      }

      cli::cli_alert("Reading to {.strong {private$.keyword_name}}")

      nline <- nline + 1
      inp_line <- read_inp_numeric_line(inp_con)

      # Throw error if vector length of input line for retro_adjust does not
      # match num_ages
      count_ages <- length(inp_line)

      if (!isTRUE(all.equal(count_ages, num_ages))) {
        stop(paste0(
          "Length of Retrosepctive coefficeient vector does not ",
          "match model's number of ages (",
          num_ages,
          ")"
        ))
      }

      # Roundabout way to suppress 'print' and cli messages for retro_adjust
      # active binding
      suppressMessages(invisible(capture.output(self$retro_adjust <- inp_line)))

      cli::cli_alert(c(
        "Line {nline}: ",
        "retro_adjust: ",
        "{.val {inp_line}} ",
        "{.emph ({num_ages} Age{?s})}"
      ))

      return(nline)
    },

    #' @description
    #' Returns values from the class to the RETROADJUST AGEPRO keyword parameter
    #' formatted as AGEPRO input file lines.
    #'
    #' @template delimiter
    #'
    get_inp_lines = function(delimiter = "  ") {
      return(list(
        self$inp_keyword,
        paste(self$retro_adjust, collapse = "  ")
      ))
    }
  ),
  active = list(
    #' @field retro_adjust
    #' This is the vector of age-specific numbers at age multipliers for an
    #' initial population size at age vector if retrospective bias adjustment
    #' is applied.
    retro_adjust = function(value) {
      if (isTRUE(missing(value))) {
        return(private$.retro_adjust)
      } else {
        if (isFALSE(self$enable_retrospecttive_adjustments)) {
          stop(private$unenabled_options_flag_message(), call. = FALSE)
        }

        checkmate::assert_numeric(value, lower = 0)

        private$.retro_adjust <- value
        names(private$.retro_adjust) <- paste0(
          "Age",
          1:length(private$.retro_adjust)
        )
        withCallingHandlers(
          message = function(cnd) cli::cli_alert_info("retro_adjust: "),
          capture_output_as_message(private$.retro_adjust)
        )
      }
    },

    #' @field enable_retrospective_adjustments
    #' Logical field that flags if fields can be edited. This class will not
    #' accept new values to its fields or allow it to be exported to input file
    #' until this option flag is TRUE.
    enable_retrospective_adjustments = function(value) {
      if (isTRUE(missing(value))) {
        return(private$.retro_flag$op$enable_retrospective_adjustments)
      } else {
        private$set_enable_retrospective_adjustments(value)
      }
    },

    #' @field json_list_object
    #' Returns JSON list object of containing SCALE values
    json_list_object = function() {
      return(list(
        retro_adjust = self$retro_adjust
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
    .retro_adjust = NULL,

    .keyword_name = "retroadjust",
    .retro_flag = NULL,
    .name_options_flag = "enable_retrospective_adjustments",

    # Wrapper Function to toggle enable_retrospective_adjustments options_flag.
    set_enable_retrospective_adjustments = function(x) {
      checkmate::assert_logical(x, null.ok = TRUE)

      #Set value to options flags field reference "flag"
      private$.retro_flag$op$enable_retrospective_adjustments <- x

      cli::cli_alert_info(
        paste0(
          "{private$.name_options_flag} to ",
          "{.val ",
          "{private$.retro_flag$op$enable_retrospective_adjustments}}"
        )
      )
    },

    # Error message when setting retrospective_coefficient values while
    # enable_retrospective_adjustments is FALSE
    unenabled_options_flag_message = function() {
      return(invisible(
        paste0(
          private$.name_options_flag,
          " is FALSE. Set flag to TRUE to set value."
        )
      ))
    },

    # Convenience function to validate parameter `retro_flag_param` at
    # initialization
    validate_retro_flag = function(retro_flag_param) {
      # Check if parameter is a options_flag R6class w/ "op" field (or NULL)
      checkmate::assert_r6(
        retro_flag_param,
        classes = "options_flags",
        public = "op",
        null.ok = TRUE
      )

      # Check and warn if parameter has a non-null
      # enable_retrospective_adjustments value
      if (isFALSE(is.null(retro_flag_param$op$enable_scaling_factors))) {
        warning(paste0(
          "Initializing ",
          private$.keyword_name,
          " with a non-null ",
          private$.name_options_flag,
          " value"
        ))
      }
    }
  )
)
