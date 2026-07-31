#' @title
#' AGEPRO Case ID
#'
#' @description
#' Input title identifying model attributes
#'
#' @template inp_con
#' @template nline
#'
#' @export
#' @importFrom R6 R6Class
case_id <- R6Class(
  "case_id",
  public = list(
    #' @description
    #' Initialize Class
    #'
    #' @param model_name Character string that describes the projection model
    #'
    initialize = function(model_name = NULL) {
      self$model_name <- model_name
    },

    #' @description
    #' Prints out Model case id
    #'
    print = function() {
      if (is.null(self$model_name)) {
        warning("model_name is NULL", call. = FALSE)
      }
      cli::cli_text("{symbol$info} model_name: {.val {self$model_name}}")
    },

    #' @description
    #' Read AGEPRO Case ID from input data file
    #'
    read_inp_lines = function(inp_con, nline) {
      nline <- nline + 1
      self$model_name <- readLines(inp_con, n = 1, warn = FALSE)

      cli::cli_alert(
        "Line {nline}: Reading to {.strong {self$keyword_name}}:"
      )
      div_model_name <-
        cli::cli_div(
          class = "input_field",
          theme = list(.input_field = list("margin-left" = 2))
        )
      self$print()
      cli::cli_end(div_model_name)
      return(nline)
    },

    #' @description
    #' Returns the values for the CASEID keyword parameter formatted
    #' to the AGEPRO input file format.description
    #'
    get_inp_lines = function() {
      return(list(
        self$inp_keyword,
        self$model_name
      ))
    },

    #' @description
    #' Cleans up CASE_ID's model name for any invalid
    #' filename characters. Return 'untitled` for blank or empty
    #' character strings.
    #'
    #' @param replace_blanks \cr
    #' Option to clean up blank characters and replace them
    #' as a underscore symbol
    #'
    sanitize_case_id_fschar = function(replace_blanks = TRUE) {
      case_id_fschar <- self$model_name
      # Use "untitled" for blank case_id
      if (
        checkmate::test_character(
          case_id_fschar,
          pattern = "^$|^[:blank:]]+$",
          null.ok = FALSE
        )
      ) {
        return("untitled")
      }

      # Check CASE_ID model name for invalid characters for filenames.
      regex_invalid_file_char <- '[[:cntrl:]\\\\/:*?\"<>|-]'
      if (
        checkmate::test_character(
          case_id_fschar,
          pattern = regex_invalid_file_char
        )
      ) {
        gsub(regex_invalid_file_char, "", case_id_fschar)
      }

      if (replace_blanks) {
        # Collapse multiple blank spaces to 1 character then
        # Replace as Underscore
        gsub("[[:blank:]]{1,}", " ", case_id_fschar)
      }

      return(case_id_fschar)
    }
  ),
  active = list(
    #' @field model_name
    #' String that describes the projection model run
    model_name = function(val) {
      if (missing(val)) {
        return(private$.model_name)
      } else {
        private$.model_name <- val
      }
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
    .keyword_name = "case_id",

    .model_name = NULL
  )
)
