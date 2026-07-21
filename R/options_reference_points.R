#' @title
#' Values to output optional agepro model's reference point threshold report.
#'
#' @description
#' Class Structure that defines biomass thresholds for spawaning biomass
#' \eqn{(B_{S,\ THRESHOLD})}, mean biomass \eqn{(\bar B_{S,\ THRESHOLD})},
#' and total stock biomass\eqn{(B_{T,\ THRESHOLD})}, including the threshold
#' rate for fishing mortality \eqn{(F_{THRESHOLD})}.
#'
#' The logical flag `enable_reference_points` allows the user to
#' set values to this class fields. The flag will also notify `agepro_model`
#' if this keyword parameter is allowed to be written to input file.
#'
#' If this class is initialized with default values, it is presumed that this
#' keyword parameter is not used in the agepro_model. Therefore,
#' `enable_reference_points` is flagged as FALSE. Valid non-default values will
#' set this flag to TRUE.
#'
#' @details
#' The reference_points class (`REFPOINTS`) is recognized as a keyword
#' parameter used in the AGEPRO input file format, but it is optional.
#'
#' @include options_flags.R
#'
#' @export
#' @importFrom R6 R6Class
#'
reference_points <- R6Class(
  "reference_points",
  public = list(
    #' @description
    #' Initializes the class
    #'
    #' @param ssb_threshold
    #' Spawning Biomass threshold express in biomass output units (MT).
    #'
    #' @param stockbio_threshold
    #' Stock biomass threshold expressed in biomass output units(MT).
    #'
    #' @param meanbio_threshold
    #' Mean biomass threshold expressed in biomass output units (MT)
    #'
    #' @param fmort_threshold
    #' Fishing mortality threshold
    #'
    #' @param refpoint_flag
    #' R6class containing option flags to allow reference points to be used
    #'
    initialize = function(
      ssb_threshold = 0,
      stockbio_threshold = 0,
      meanbio_threshold = 0,
      fmort_threshold = 0,
      refpoint_flag = NULL
    ) {
      div_keyword_header(private$.keyword_name)

      # Check refpoint_flag is a options_flag R6class w/ "op" field
      checkmate::assert_r6(
        refpoint_flag,
        classes = "options_flags",
        public = "op",
        null.ok = TRUE
      )

      # Check and warn if input refpoint_flag param has a non-null
      # enable_reference_points value
      if (isFALSE(is.null(refpoint_flag$op$enable_reference_points))) {
        warning(paste0(
          "Initializing reference points with a non-null ",
          "options flag value"
        ))
      }

      # If all parameters are non-default values set the flag to FALSE.
      default_ssb_threshold <-
        formals(self$initialize)[["ssb_threshold"]]
      default_stockbio_threshold <-
        formals(self$initialize)[["stockbio_threshold"]]
      default_meanbio_threshold <-
        formals(self$initialize)[["meanbio_threshold"]]
      default_fmort_threshold <-
        formals(self$initialize)[["fmort_threshold"]]

      if (
        all(c(
          all.equal(ssb_threshold, default_ssb_threshold),
          all.equal(stockbio_threshold, default_stockbio_threshold),
          all.equal(meanbio_threshold, default_meanbio_threshold),
          all.equal(fmort_threshold, default_fmort_threshold)
        ))
      ) {
        cli::cli_alert(paste0("All reference_points parameters are default:"))

        self$ssb_threshold <- ssb_threshold
        self$stock_biomass_threshold <- stockbio_threshold
        self$mean_biomass_threshold <- meanbio_threshold
        self$fishing_mortality_threshold <- fmort_threshold

        private$set_enable_reference_points(FALSE)

        return()
      }

      cli::cli_alert("Setting reference_points values ... ")

      self$ssb_threshold <- ssb_threshold
      self$stock_biomass_threshold <- stockbio_threshold
      self$mean_biomass_threshold <- meanbio_threshold
      self$fishing_mortality_threshold <- fmort_threshold

      private$set_enable_reference_points(TRUE)
    },

    #' @description
    #' Formatted to print out reference_points values
    #'
    print = function() {
      cli::cli_alert_info(
        paste0(
          "enable_reference_points ",
          "{.emph (Enable Reference Point Threshold Report)}: ",
          "{.val {self$enable_reference_points}}"
        )
      )
      cli::cli_alert_info("ssb_threshold: {.val {self$ssb_threshold}}")
      cli::cli_alert_info(paste0(
        "stock_biomass_threshold: ",
        "{.val {self$stock_biomass_threshold}}"
      ))
      cli::cli_alert_info(paste0(
        "mean_biomass_threshold: ",
        "{.val {self$mean_biomass_threshold}}"
      ))
      cli::cli_alert_info(paste0(
        "fishing_mortality_threshold: ",
        "{.val {self$fishing_mortality_threshold}}"
      ))
    },

    #' @description
    #' Reads in the values from the keyword parameter REFPOINT from the
    #' AGEPRO Input file
    #'
    #' Note: enable_reference_points must be set to TRUE.
    #'
    #' @template inp_con
    #' @template nline
    #'
    read_inp_lines = function(inp_con, nline) {
      if (isFALSE(self$enable_reference_points)) {
        stop(private$unenabled_options_flag_message())
      }

      cli::cli_alert("Reading to {.strong {private$.keyword_name}}")

      nline <- nline + 1
      inp_line <- read_inp_numeric_line(inp_con)

      cli::cli_alert(paste0(
        "Line {nline}: ",
        "Values for Reference Point Thereshold Report"
      ))
      li_nested <-
        cli::cli_div(
          id = "bounds_inp_fields",
          theme = list(".alert-info" = list("margin-left" = 2))
        )

      self$ssb_threshold <- inp_line[1]
      self$stock_biomass_threshold <- inp_line[2]
      self$mean_biomass_threshold <- inp_line[3]
      self$fishing_mortality_threshold <- inp_line[4]

      cli::cli_end("bounds_inp_fields")

      return(nline)
    },

    #' @description
    #' Returns values from the class to the REFOPINTS AGEPRO keyword parameter
    #' formatted as AGEPRO input file lines.
    #'
    #' @template delimiter
    #'
    get_inp_lines = function(delimiter = " ") {
      # Re-check fields before formatting.
      # In this case, do not allow NULL values to be passed.
      validation_error <- checkmate::makeAssertCollection()
      checkmate::assert_numeric(
        self$ssb_threshold,
        lower = 0,
        len = 1,
        add = validation_error
      )
      checkmate::assert_numeric(
        self$stock_biomass_threshold,
        lower = 0,
        len = 1,
        add = validation_error
      )
      checkmate::assert_numeric(
        self$mean_biomass_threshold,
        lower = 0,
        len = 1,
        add = validation_error
      )
      checkmate::assert_numeric(
        self$fishing_mortality_threshold,
        lower = 0,
        len = 1,
        add = validation_error
      )

      return(list(
        self$inp_keyword,
        paste(
          self$ssb_threshold,
          self$stock_biomass_threshold,
          self$mean_biomass_threshold,
          self$fishing_mortality_threshold,
          sep = delimiter
        )
      ))
    }
  ),
  active = list(
    #' @field ssb_threshold
    #' Threshold of spawning biomass
    ssb_threshold = function(value) {
      if (missing(value)) {
        if (is.null(private$.ssb_threshold)) {
          warning("ssb_threshold is NULL", call. = FALSE)
        }
        return(private$.ssb_threshold)
      } else {
        if (isFALSE(self$enable_reference_points)) {
          stop(private$unenabled_options_flag_message(), call. = FALSE)
        }

        checkmate::assert_numeric(value, lower = 0, len = 1)

        private$.ssb_threshold <- value
        withCallingHandlers(
          message = function(cnd) {},
          cli::cli_alert_info("ssb_threshold: {.val {private$.ssb_threshold}}")
        )
      }
    },

    #' @field stock_biomass_threshold
    #' Threshold of total stock biomass
    stock_biomass_threshold = function(value) {
      if (missing(value)) {
        if (is.null(private$.stock_biomass_threshold)) {
          warning("stock_biomass_threshold is NULL", call. = FALSE)
        }
        return(private$.stock_biomass_threshold)
      } else {
        if (isFALSE(self$enable_reference_points)) {
          stop(private$unenabled_options_flag_message(), call. = FALSE)
        }

        checkmate::assert_numeric(value, lower = 0, len = 1)

        private$.stock_biomass_threshold <- value
        withCallingHandlers(
          message = function(cnd) {},
          cli::cli_alert_info(
            paste0(
              "stock_biomass_threshold: ",
              "{.val {private$.stock_biomass_threshold}}"
            )
          )
        )
      }
    },

    #' @field mean_biomass_threshold
    #' Threshold for mean biomass
    mean_biomass_threshold = function(value) {
      if (missing(value)) {
        if (is.null(private$.mean_biomass_threshold)) {
          warning("mean_biomass_threshold is NULL", call. = FALSE)
        }
        return(private$.mean_biomass_threshold)
      } else {
        if (isFALSE(self$enable_reference_points)) {
          stop(private$unenabled_options_flag_message(), call. = FALSE)
        }

        checkmate::assert_numeric(value, lower = 0, len = 1)

        private$.mean_biomass_threshold <- value
        withCallingHandlers(
          message = function(cnd) {},
          cli::cli_alert_info(
            paste0(
              "mean_biomass_threshold: ",
              "{.val {private$.mean_biomass_threshold}}"
            )
          )
        )
      }
    },

    #' @field fishing_mortality_threshold
    #' Threshold for the fishing mortality rate for annual total fishing
    #' mortality calculated across all fleets.
    fishing_mortality_threshold = function(value) {
      if (missing(value)) {
        if (is.null(private$.fishing_mortality_threshold)) {
          warning("fishing_mortality_threshold is NULL", call. = FALSE)
        }
        return(private$.fishing_mortality_threshold)
      } else {
        if (isFALSE(self$enable_reference_points)) {
          stop(private$unenabled_options_flag_message(), call. = FALSE)
        }

        checkmate::assert_numeric(value, lower = 0, len = 1)

        private$.fishing_mortality_threshold <- value
        withCallingHandlers(
          message = function(cnd) {},
          cli::cli_alert_info(
            paste0(
              "fishing_mortality_threshold: ",
              "{.val {private$.fishing_mortality_threshold}}"
            )
          )
        )
      }
    },

    #' @field json_list_object
    #' Returns JSON list object of containing REFPOINT values
    json_list_object = function() {
      return(list(
        ssb_threshold = self$ssb_threshold,
        stock_biomass_threshold = self$stock_biomass_threshold,
        mean_biomass_threshold = self$mean_biomass_threshold,
        fishing_mortality_threshold = self$fishing_mortality_threshold
      ))
    },

    #' @field enable_reference_points
    #' Logical field that flags if fields can be edited. This class will not
    #' accept new values to its fields or allow it to be exported to input file
    #' until this option flag is TRUE.
    enable_reference_points = function(value) {
      if (isTRUE(missing(value))) {
        return(private$.refpoint_flag$op$enable_reference_points)
      } else {
        private$set_enable_reference_points(value)
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
    .keyword_name = "refpoint",

    .ssb_threshold = NULL,
    .stock_biomass_threshold = NULL,
    .mean_biomass_threshold = NULL,
    .fishing_mortality_threshold = NULL,

    .refpoint_flag = NULL, #R6class containing option_flags
    .name_options_flag = "enable_reference_points",

    # Wrapper Function to toggle enable_reference_points options_flag.
    set_enable_reference_points = function(x) {
      checkmate::assert_logical(x, null.ok = TRUE)

      #Set value to options flags field reference "flag"
      private$.refpoint_flag$op$enable_reference_points <- x

      cli::cli_alert_info(
        paste0(
          "{private$.name_options_flag} to ",
          "{.val ",
          "{private$.refpoint_flag$op$enable_reference_points}}"
        )
      )
    },

    # Error message when setting values to reference_points while
    # enable_reference_points is FALSE
    unenabled_options_flag_message = function() {
      return(invisible(
        paste0(
          private$.name_options_flag,
          " is FALSE. ",
          "Set flag to TRUE to set value."
        )
      ))
    }
  )
)
