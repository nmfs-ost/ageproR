

#' @title Scale Factors
#'
#' @description
#' Input information on scaling factors for biomass, recruitment, and stock size
#'
#' The logical flag `enable_scaling_factors` allows the user to
#' set values to this class active binding fields: `biomass_scale`,
#' `recruitment_scale`, and `stock_size_scale`. The flag
#' will also notify `agepro_model` that this keyword parameter is allowed
#' to be written to input file.
#'
#' If this class is initialized with default values, it is presumed that this
#' keyword parameter is not used in the agepro_model. Therefore,
#' `enable_scaling_factors` is flagged as FALSE. Valid non-default values will
#' set this flag to TRUE.
#'
#' @details
#' The scaling_factors class (or SCALE) is recognized as a keyword
#' parameter used in the AGEPRO input file format, but it is optional.
#'
#' @include options_flags.R
#'
#' @export
#' @importFrom R6 R6Class
#'
scaling_factors <-R6Class(
  "scaling_factors",
  public = list(

    #' @description
    #' Initializes the class
    #'
    #' @param scale_bio
    #' Output units of biomass express in thousand metric units
    #'
    #' @param scale_recruit
    #' Output Units of Recruitment Numbers
    #'
    #' @param scale_stock_size
    #' Output Units of Stock Size Numbers
    #'
    #' @param scale_flag
    #' R6class containing option flags to allow scaling factors to be used
    #'
    initialize = function(scale_bio = 0,
                          scale_recruit = 0,
                          scale_stock_size = 0,
                          scale_flag = NULL) {

      div_keyword_header(private$.keyword_name)

      # Validation checks in case scaling_factors is initialized w/ non-null
      # or invalid enable_scaling_factors
      private$validate_scale_flag(scale_flag)

      #Check for defaults
      default_scale_bio <- formals(self$initialize)[["scale_bio"]]
      default_scale_recruit <- formals(self$initialize)[["scale_recruit"]]
      default_scale_stock_size <- formals(self$initialize)[["scale_stock_size"]]

      if(all(c(all.equal(scale_bio, default_scale_bio),
               all.equal(scale_recruit, default_scale_recruit),
               all.equal(scale_stock_size, default_scale_stock_size)))) {
        cli::cli_alert(paste0("All scaling_factor parameters are default: "))

        self$biomass_scale <- scale_bio
        self$recruitment_scale <- scale_recruit
        self$stock_size_scale <- scale_stock_size

        private$set_enable_scaling_factors(FALSE)

        return()
      }

      cli::cli_alert("Setting scaling_factor values: ")

      self$biomass_scale <- scale_bio
      self$recruitment_scale <- scale_recruit
      self$stock_size_scale <- scale_stock_size

      private$set_enable_scaling_factors(TRUE)


    },

    #' @description
    #' Formatted to print out scaling_factors values
    #'
    print = function(){

      cli::cli_alert_info(
        paste0("enable_scaling_factors ",
               "{.emph (Specify Scaling Factors for Output Report File)}: ",
               "{.val {self$enable_scaling_factors}}"))

      cli::cli_alert_info("biomass_scale: {.val {self$biomass_scale}}")
      cli::cli_alert_info("recruitment_scale: {.val {self$recruitment_scale}}")
      cli::cli_alert_info(paste0("stock_size_scale: ",
                         "{.val {self$stock_size_scale}}"))



    },

    #' @description
    #' Reads in the values from the keyword parameter SCALE from the
    #' AGEPRO Input file
    #'
    #' Note: enable_scaling_factors must be set to TRUE.
    #'
    #' @template inp_con
    #' @template nline
    #'
    read_inp_lines = function(inp_con, nline) {

      if(isFALSE(self$enable_scaling_factors)){
        stop(private$unenabled_options_flag_message())
      }

      cli::cli_alert("Reading {.strong {private$.keyword_name}}")

      nline <- nline + 1
      inp_line <- read_inp_numeric_line(inp_con)

      cli::cli_alert("Line {nline}: Scaling Factor values")

      li_nested <-
        cli::cli_div(id = "scale_inp_fields",
                     theme = list(".alert-info" = list("margin-left" = 2)))

      self$biomass_scale <- inp_line[1]
      self$recruitment_scale <- inp_line[2]
      self$stock_size_scale <- inp_line[3]

      cli::cli_end("scale_inp_fields")

      return(nline)

    },

    #' @description
    #' Returns values from the class to the SCALE AGEPRO keyword parameter
    #' formatted as AGEPRO input file lines.
    #'
    #' @template delimiter
    #'
    get_inp_lines = function(delimiter = " ") {

      # Re-check fields before formatting.
      # In this case, do not allow NULL values to be passed.
      validation_error <- checkmate::makeAssertCollection()
      checkmate::assert_numeric(self$biomass_scale,
                                lower = 0, len = 1,
                                add = validation_error)
      checkmate::assert_numeric(self$recruitment_scale,
                                lower = 0, len = 1,
                                add = validation_error)
      checkmate::assert_numeric(self$stock_size_scale,
                                lower = 0, len = 1,
                                add = validation_error)

      return(list(
        self$inp_keyword,
        paste(self$biomass_scale,
              self$recruitment_scale,
              self$stock_size_scale,
              sep = delimiter)
      ))

    }



  ),
  active = list(

    #' @field biomass_scale
    #' Output units of biomass expressed in thousand metric tons
    biomass_scale = function(value){
      if(missing(value)){
        if(is.null(private$.biomass_scale)){
          warning("biomass_scale is NULL", call. = FALSE)
        }
        return(private$.biomass_scale)
      }else{
        if(isFALSE(self$enable_scaling_factors)) {
          stop(private$unenabled_options_flag_message(), call. = FALSE)
        }

        checkmate::assert_numeric(value, lower = 0, len = 1)

        private$.biomass_scale <- value
        withCallingHandlers(
          message = function(cnd) {

          },
          cli::cli_alert_info(paste0("biomass_scale: ",
                                     "{.val {private$.biomass_scale}}"))
        )


      }
    },

    #' @field recruitment_scale
    #' Output units of recruitment numbers
    recruitment_scale = function(value){
      if(missing(value)){
        if(is.null(private$.recruitment_scale)){
          warning("recruitment_scale is NULL", call. = FALSE)
        }
        return(private$.recruitment_scale)
      }else{
        if(isFALSE(self$enable_scaling_factors)) {
          stop(private$unenabled_options_flag_message(), call. = FALSE)
        }

        checkmate::assert_numeric(value, lower = 0, len = 1)

        private$.recruitment_scale <- value
        withCallingHandlers(
          message = function(cnd) {

          },
          cli::cli_alert_info(paste0("recruitment_scale: ",
                                     "{.val {private$.recruitment_scale}}"))
        )
      }
    },

    #' @field stock_size_scale
    #' Output Units of stock size numbers
    stock_size_scale = function(value){
      if(missing(value)){
        if(is.null(private$.stock_size_scale)){
          warning("stock_size_scale is NULL", call. = FALSE)
        }
        return(private$.stock_size_scale)
      }else{
        if(isFALSE(self$enable_scaling_factors)) {
          stop(private$unenabled_options_flag_message(), call. = FALSE)
        }

        checkmate::assert_numeric(value, lower = 0, len = 1)

        private$.stock_size_scale <- value
        withCallingHandlers(
          message = function(cnd) {

          },
          cli::cli_alert_info(paste0("stock_size_scale: ",
                                     "{.val {private$.stock_size_scale}}"))
        )
      }
    },

    #' @field enable_scaling_factors
    #' Logical field that flags if fields can be edited. This class will not
    #' accept new values to its fields or allow it to be exported to input file
    #' until this option flag is TRUE.
    enable_scaling_factors = function(value) {
      if(isTRUE(missing(value))){
        return(private$.scale_flag$op$enable_scaling_factors)
      } else {
        private$set_enable_scaling_factors(value)
      }

    },

    #' @field json_list_object
    #' Returns JSON list object of containing SCALE values
    json_list_object = function() {
      return(list(
        scale_biomass = self$biomass_scale,
        scale_recruitment = self$recruitment_scale,
        scale_stock_size = self$stock_size_scale
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

    .keyword_name = "scale",

    .biomass_scale = NULL,
    .recruitment_scale = NULL,
    .stock_size_scale = NULL,

    .scale_flag = NULL,
    .name_options_flag = "enable_scaling_factors",

    # Wrapper Function to toggle enable_reference_points options_flag.
    set_enable_scaling_factors = function(x) {

      checkmate::assert_logical(x, null.ok = TRUE)

      #Set value to options flags field reference "flag"
      private$.scale_flag$op$enable_scaling_factors <- x

      cli::cli_alert_info(
        paste0("{private$.name_options_flag} to ",
               "{.val ",
               "{private$.scale_flag$op$enable_scaling_factors}}"))

    },

    # Error message when setting values to reference_points while
    # enable_reference_points is FALSE
    unenabled_options_flag_message = function() {
      return(invisible(
        paste0(private$.name_options_flag," is FALSE. ",
               "Set flag to TRUE to set value.")
      ))
    },

    # Convenience function to validate parameter `scale_flag_param` at
    # initialization
    validate_scale_flag = function(scale_flag_param) {

      # Check if parameter is a options_flag R6class w/ "op" field (or NULL)
      checkmate::assert_r6(scale_flag_param, classes = "options_flags",
                           public = "op", null.ok = TRUE)

      # Check and warn if parameter has a non-null
      # enable_scaling_factors value
      if(isFALSE(is.null(scale_flag_param$op$enable_scaling_factors))){
        warning(paste0("Initializing ",
                       private$.keyword_name ," with a non-null ",
                       private$.name_options_flag, " value"))
      }

    }


  )
)
