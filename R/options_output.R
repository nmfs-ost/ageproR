
#' @title
#' AGEPRO projection output options.
#'
#' @description
#' Class Structure that includes user-defined options to enable auxiliary or
#' options to export AGEPRO output
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#'
#' @template inp_con
#' @template nline
#'
#' @export
#'
options_output <- R6Class(
  "options_output",
  public = list(

    #' @description
    #' Initializes the class
    #'
    #' @param summary_report
    #' [Numeric][base::numeric] flag to enable Stock Distribution Summary file
    #' and auxiliary data. The following options allow the user to select which
    #' output from the AGEPRO calculation engine is returned:
    #' \describe{
    #'  \item{0}{Do not output Stock of Age Distribution Summary File, but output
    #'  auxiliary data files EXCEPT the Stock Numbers of Age Auxiliary File.}
    #'  \item{1}{Output Stock of Age Distribution Summary File and
    #'  all auxiliary data files
    #'  files.}
    #'  \item{2}{Do not output Stock of Age Distribution Summary and Auxiliary
    #'  files.}
    #'  \item{3}{Output Stock of Age Distribution Summary, but do not output
    #'  any auxiliary files.}
    #'  \item{4}{Output Stock of Age Distribution Summary and Auxiliary files
    #'  EXCEPT the Stock Numbers of Age Auxiliary File
    #'  }
    #' }
    #'
    #' @param output_process_error_aux_files
    #' [Logical][base::logical] flag to enable output of process_error
    #' auxiliary files
    #'
    #' @param export_r_data_frame
    #' [Logical][base::logical] flag to enable AGEPRO output to data.frame
    #'
    initialize = function(summary_report = 0,
                          output_process_error_aux_files = FALSE,
                          export_r_data_frame = TRUE) {

      div_keyword_header(private$.keyword_name)
      cli_alert("Setting AGEPRO projection output options ...")

      self$output_stock_summary <- summary_report
      self$output_process_error_aux_files <- output_process_error_aux_files
      self$output_data_frame <- export_r_data_frame
    },

    #' @description
    #' Formatted to print out output_option values
    #'
    print = function() {
      cli::cli_alert_info(
        paste0(
          "output_stock_summary: ",
          "{.val {private$.output_stock_summary}} ",
          "{.emph ({private$aux_flag_string(private$.output_stock_summary)})}"))
      cli::cli_alert_info(
        paste0(
          "output_process_error_aux_files: ",
          "{.val {private$.output_process_error_aux_files}} ",
          "{.emph ({as.logical(private$.output_process_error_aux_files)})}"))
      cli::cli_alert_info(
        paste0(
          "output_data_frame {.emph (export output as data.frame)}: ",
          "{.val {private$.output_data_frame}} ",
          "{.emph ({as.logical(private$.output_data_frame)})}"))


    },

    #' @description
    #' Reads in the values from the keyword parameter OPTIONS from the
    #' AGEPRO Input file
    #'
    read_inp_lines = function(inp_con, nline) {

      cli::cli_alert("Reading {.strong {private$.keyword_name}}")

      nline <- nline + 1
      inp_line <- read_inp_numeric_line(inp_con)

      suppressMessages(self$output_stock_summary <- inp_line[1])
      suppressMessages(self$output_process_error_aux_files <- inp_line[2])
      suppressMessages(self$output_data_frame <- inp_line[3])

      cli::cli_alert(paste0("Line {nline} : ",
                            "Reading AGEPRO projection output options ..."))

      cli::cli_div(id = "options_fields",
                   theme = list(".alert-info" = list("margin-left" = 2)))
      self$print()
      cli::cli_end("options_fields")

      return(nline)
    },

    #' @description
    #' Returns values from the options_output (OPTIONS)
    #' AGEPRO keyword parameter formatted as AGEPRO input file lines.
    #'
    #' @template delimiter
    #'
    get_inp_lines = function(delimiter = " "){
      return(list(
        self$inp_keyword,
        paste(self$output_stock_summary,
              self$output_process_error_aux_files,
              self$output_data_frame,
              sep = delimiter)
      ))
    }

  ),
  active = list(

    #' @field output_stock_summary
    #' [Logical][base::logical] flag to output stock summary information
    output_stock_summary = function(value) {
      if(missing(value)) {
        return(private$.output_stock_summary)
      }else{

        # Calling Handler to wrap field name w/ validate_logical_parameter
        # message
        withCallingHandlers(
          message = function (cnd) {
            cli::cli_alert_info(
              paste0("output_summary_report: ",
                     "{sub('\u2192 ', '', conditionMessage(cnd))}"))

            rlang::cnd_muffle(cnd)
          },

          private$.output_stock_summary <- validate_logical_parameter(value)
        )
      }
    },

    #' @field output_process_error_aux_files
    #' [Logical][base::logical] flag to output population and fishery processes
    #' simulated with lognormal process error (process_error parameters) to
    #' auxiliary output files
    output_process_error_aux_files = function(value) {
      if(missing(value)) {
        return(private$.output_process_error_aux_files)
      }else {

        # Calling Handler to wrap field name w/ validate_logical_parameter
        # message
        withCallingHandlers(
          message = function(cnd) {
            cli::cli_alert_info(
              paste0("output_process_error_aux_files ",
                     "{.emph (Auxillary output files)}: ",
                     "{sub('\u2192 ', '', conditionMessage(cnd))}"))

            rlang::cnd_muffle(cnd)
          },
          private$.output_process_error_aux_files <-
            validate_logical_parameter(value)
        )
      }
    },

    #' @field output_data_frame
    #' [Logical][base::logical] flag to output AGEPRO calculation engine
    #' projection results to R [data.frame][base::data.frame]. Default is
    #' `1` (or TRUE) at initialization.
    #'
    output_data_frame = function(value) {
      if(missing(value)){
        return(private$.output_data_frame)
      }else{

        # Calling Handler to wrap field name w/ validate_logical_parameter
        # message
        withCallingHandlers(
          message = function(cnd) {
            cli::cli_alert_info(
              paste0("output_data_frame ",
                     "{.emph (AGEPRO output as data.frame)}: ",
                     "{sub('\u2192 ', '', conditionMessage(cnd))}"))
            rlang::cnd_muffle(cnd)
          },

          private$.output_data_frame <- validate_logical_parameter(value)
        )

      }
    },

    #' @field valid_aux_output_flags
    #' Returns a list of valid numerical flags to enable _Stock of Age _
    #' _Distribution Summary and Auxiliary Files_.
    valid_aux_output_flags = function () {
      return(private$.valid_aux_output_flags)
    },

    #' @field json_list_object
    #' Returns JSON list object of containing options_output values
    json_list_object = function() {
      return(list(
        stock_summary_flag = self$output_stock_summary,
        process_error_aux_data_flag = self$output_process_error_aux_files,
        export_R_flag = self$output_data_frame
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

    .keyword_name = "options",

    .output_stock_summary = NULL,
    .output_process_error_aux_files = NULL,
    .output_data_frame = NULL,
    .valid_aux_output_flags = c(0,1,2,3,4),

    aux_flag_string = function(value) {

      #Validation
      checkmate::assert_choice(value, choices = private$.valid_aux_output_flags)

      list_aux_flag <- list("No Stock of Age Distribution Summary, Output Auxiliary Files EXCEPT Stock Numbers of Age",
                            "Output Stock of Age Distribution Summary and All Auxiliary Files",
                            "No Stock Of Age Distribution Summary and Auxiliary Files",
                            "Output Stock Distribution Summary, but NO Auxiliary Files",
                            "Output Stock Distribution Summary and Auxiliary Files EXCEPT Auxiliary Stock File")

      #Add 1 to value to match up with list_aux_flag indexing
      return(list_aux_flag[[value+1]])

    }


  )

)
