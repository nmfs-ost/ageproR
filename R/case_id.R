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
        "Line {nline}: Reading to {.strong {self$keyword_name}}: {self$model_name}"
      )
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
