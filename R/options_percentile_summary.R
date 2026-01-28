

#' @title
#' Percentile summary of the key results of AGEPRO projection output
#'
#' @description
#' Class Structure that includes user-defined options for setting
#' a specific percentile for the distributions of outputs.
#'
#' The logical flag `enable_percentile_summary` allows the user to
#' set values to this class field: report_percentile. The flag will also notify
#' `agepro_model` if this keyword parameter is allowed to be written to
#' input file.
#'
#' If percentile_summary is initialized with default values, it is
#' presumed that this keyword parameter is not used in the agepro_model.
#' Therefore, `enable_percentile_summary` is flagged as FALSE. Valid non-
#' default values will set this flag to TRUE.#'
#'
#' @details
#' The percentile_summary class (or `PERC`) is an optional keyword
#' parameter used in the AGEPRO input file format.
#'
#' @export
#'
percentile_summary <- R6Class(
  "percentile_summary",
  public = list(

    #' @description
    #' Initializes the class
    #'
    #' @param perc User-defined percentile of projected distributions
    #'
    #' @param perc_flag
    #' R6class containing option flags to allow percentile summary to be used
    #'
    initialize = function(perc = 0,
                          perc_flag = NULL){

      div_keyword_header(private$.keyword_name)

      # Validation checks in case percentile_summary is initialized w/ non-null
      # or invalid type for enable_percentile_summary
      private$validate_perc_flag(perc_flag)

      # Presume perc default is 0.
      # If all parameters are non-default values set the flag to FALSE.
      default_perc <- formals(self$initialize)[["perc"]]
      if(isTRUE(all.equal(perc,default_perc))){
        cli::cli_alert(paste0("All percentile_summary parameters are default:"))

        self$report_percentile <- perc

        private$set_enable_percentile_summary(FALSE)

        return()

      }

      cli::cli_alert("Setting percentile_summary values ... ")

      self$report_percentile <- perc
      private$set_enable_percentile_summary(TRUE)

    },


    #' @description
    #' Formatted to print out output_option values
    #'
    print = function(){

      cli::cli_alert_info(
        paste0("enable_percentile_summary ",
               "{.emph (Request Percentile Report)}: ",
               "{.val {self$enable_percentile_summary}}"))
      cli::cli_alert_info("report_percentile: {.val {self$report_percentile}}")
    },


    #' @description
    #' Reads in the values from the keyword parameter PERC from the
    #' AGEPRO Input file
    #'
    #' @template inp_con
    #' @template nline
    #'
    read_inp_lines = function(inp_con, nline) {

      if(isFALSE(self$enable_percentile_summary)){
        stop(private$unenabled_options_flag_message())
      }

      cli::cli_alert("Reading {.strong {private$.keyword_name}}")

      nline <- nline + 1
      inp_line <- read_inp_numeric_line(inp_con)

      cli::cli_alert("Line {nline} ... ")

      li_nested <-
        cli::cli_div(id = "report_perc_field",
                     theme = list(".alert-info" = list("margin-left" = 2)))
      self$report_percentile <- inp_line

      cli::cli_end("report_perc_field")


      return(nline)
    },

    #' @description
    #' Returns values from the class to the PERC AGEPRO keyword parameter
    #' formatted as AGEPRO input file lines.
    #'
    #' @template delimiter
    #'
    get_inp_lines = function(delimiter = " ") {

      # Re-check fields before formatting.
      # In this case, do not allow NULL values to be passed.
      checkmate::assert_numeric(self$report_percentile,
                                lower = 0, upper = 100)

      return(list(
        self$inp_keyword,
        self$report_percentile
      ))

    }

  ),
  active = list(

    #' @field report_percentile
    #' User-defined percentile for reporting the percentile of the projected
    #' distribution of the following quantities of interest by year:
    #' spawning stock biomass, stock biomass on January 1st, mean biomass,
    #' combined catch biomass, landings, fishing mortality, and stock
    #' numbers at age
    #'
    report_percentile = function(value) {
      if(missing(value)){
        if(is.null(private$.report_percentile)){
          warning("report_percentile is NULL", call. = FALSE)
        }
        return(private$.report_percentile)
      }else {

        checkmate::assert_numeric(value, null.ok = TRUE, len = 1,
                                  lower = 0, upper = 100)
        if(isFALSE(self$enable_percentile_summary)) {
          stop(paste0(private$.name_options_flag," flag is FALSE. ",
                        "Set flag to TRUE to set value.") )
        }

        private$.report_percentile <- value
        withCallingHandlers(
          message = function(cnd) {

          },
          cli::cli_alert_info(paste0("report_percentile: ",
                                     "{.val {private$.report_percentile}}"))
        )


      }

    },

    #' @field enable_percentile_summary
    #' Read-only logical field that flags if fields can be edited. To set
    #' the value use `set_enable_percentile_summary` or field
    enable_percentile_summary = function(value) {
      if(isTRUE(missing(value))){
        return(private$.perc_flag$op$enable_percentile_summary)
      }else{
        #Validate and set value via set_enable_percentile_summary
        private$set_enable_percentile_summary(value)
      }

    },


    #' @field json_list_object
    #' Returns JSON list object of containing options_output values
    json_list_object = function() {
      return(list(
        perc_report_value = self$report_percentile
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

    .keyword_name = "perc",

    .report_percentile = NULL,

    .perc_flag = NULL,
    .name_options_flag = "enable_percentile_summary",

    # Wrapper Function to toggle enable_percentile_summary options_flag.
    #
    #The percentile_summary class will not accept values until it is
    #enable_percentile_summary is TRUE.
    set_enable_percentile_summary = function(x) {

      checkmate::assert_logical(x)

      #Set value to options flags field reference "flag"
      private$.perc_flag$op$enable_percentile_summary <- x

      cli::cli_alert_info(
        paste0("{private$.name_options_flag} to ",
               "{.val ",
               "{private$.perc_flag$op$enable_percentile_summary}}"))


    },


    # Error message when setting values to reference_points while
    # enable_reference_points is FALSE
    unenabled_options_flag_message = function() {
      return(invisible(
        paste0(private$.name_options_flag, " is FALSE. ",
               "Set flag to TRUE to set value.")
      ))
    },

    # Convenience function to validate input perc_flag parameter at
    # percentile_summary initialization
    validate_perc_flag = function(perc_flag_param) {

      # Check perc_flag_param is a options_flag R6class w/ "op" field
      checkmate::assert_r6(perc_flag_param, classes = "options_flags",
                           public = "op", null.ok = TRUE)

      # Check and warn if input perc_flag_param has a non-null
      # enable_reference_points value
      if(isFALSE(is.null(perc_flag_param$op$enable_percentile_summary))){
        warning(paste0("Initializing percentile summary with a non-null ",
                       private$name_options_flag,
                       " value"))
      }

    }

  )
)
