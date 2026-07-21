#' @title
#' Generalized structure of projection analyses keyword parameters
#'
#' @description
#' Generalized or common data structure for Standard (Harvest Table only),
#' Rebuilding, and PStar projection analyses based AGEPRO Keyword parameters.
#'
#' @template elipses
#' @template inp_con
#' @template nline
#' @template delimiter
#' @template enable_cat_print
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#'
#' @keywords projection_analyses
#'
projection_analyses <- R6Class(
  "projection_analyses",
  public = list(
    #' @description
    #' Initializes class
    #'
    #' @param proj_years May be a single numeric value: the number of years in
    #' the time projection; a vector of sequential values: Sequence of years
    #' in from first to last year of the time projection; or an instance of
    #' [Projection years][ageproR::projection_years]:
    #' @param target_year Harvest Scenario Target Year for Pstar or
    #' Rebuild projections. The NULL default triggers the default to be
    #' assigned to the last projection year from `proj_years`
    #'
    initialize = function(proj_years, target_year = NULL) {
      # Handle `proj_years` that may be a single int or vector of sequential
      # values or an instance of ageproR::projection_years
      private$.projection_years <- private$validate_proj_years_parameter(
        proj_years
      )

      if (is.null(target_year)) {
        # Set default target_year to last projection year
        self$target_year <-
          private$.projection_years$sequence[private$.projection_years$count]
      } else {
        self$target_year <- target_year
      }
    }
  ),
  active = list(
    #' @field target_year
    #' User-Selected target year for rebuilder and pstar projection analyses
    target_year = function(value) {
      if (missing(value)) {
        private$.target_year
      } else {
        #Check target_year is within projection years
        first_projection_year <- private$.projection_years$sequence[1]
        last_projection_year <-
          private$.projection_years$sequence[private$.projection_years$count]

        checkmate::assert_numeric(
          value,
          len = 1,
          lower = first_projection_year,
          upper = last_projection_year,
          .var.name = "target_year"
        )

        private$.target_year <- value
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
    .target_year = NULL,

    .keyword_name = NULL,

    #setup variables at initialization
    .projection_years = NULL
  ),
)

#' @title
#' Standard Projection Analyses
#'
#' @description
#' Class Structure containing Standard Projection Analyses
#'
#' @param proj_years May be a single numeric value: the number of years in the
#' time projection; a vector of sequential values: Sequence of years in from
#' first to last year of the time projection; or an instance of
#' [Projection years][ageproR::projection_years]
#'
#' @importFrom R6 R6Class
#'
#' @keywords projection_analyses
#'
standard_projection <- R6Class(
  "standard_projection",
  inherit = projection_analyses,
  public = list(
    #' @description
    #' Initializes class
    #'
    initialize = function(proj_years) {
      super$initialize(proj_years)
    }
  )
)

#' @title
#' Projection analyses that shows the probability of exceeding overfishing
#' threshold of the target year
#'
#' @description
#' Input information for calculating Total Allowable Catch (\eqn{TAC_{pstar}})
#' to produce \eqn{P*}, which is the probability of overfishing in the target
#' projection year
#'
#' @details
#' Creating or importing `pstar_projection` object will overwrite the
#' any existing rebuild and pstar projection objects.
#'
#' @template inp_con
#' @template nline
#' @template elipses
#'
#' @param proj_years May be a single numeric value: the number of years in the
#' time projection; a vector of sequential values: Sequence of years in from
#' first to last year of the time projection; or an instance of
#' [Projection years][ageproR::projection_years]
#'
#' @importFrom R6 R6Class
#'
#' @keywords projection_analyses
#'
#' @export
#'
pstar_projection <- R6Class(
  "pstar_projection",
  inherit = projection_analyses,
  public = list(
    #' @description
    #' Initializes class
    #'
    #' @param num_pstar_levels Number of Pstar levels. Default is `1`
    #' @param pstar_f Fishing mortality rate \eqn{f}. Default is `0.0`
    #' @param ... Other parameters to pass to
    #' [`projection_analyses`][ageproR::projection_analyses]
    #'
    initialize = function(
      proj_years,
      num_pstar_levels = 1,
      pstar_f = 0.0,
      ...
    ) {
      super$initialize(proj_years)

      self$num_pstar_levels <- num_pstar_levels
      self$pstar_overfishing_f <- pstar_f

      self$pstar_levels_table <-
        self$create_blank_pstar_levels_table(self$num_pstar_levels)

      #TODO: implement enable_cat_print argument
    },

    #' @description
    #' Creates a blank table-like matrix of probabilities of overfishing,
    #' or PStar values, to be used.
    #'
    #' @param num_pstar_levels Number of pstar values
    #'
    create_blank_pstar_levels_table = function(num_pstar_levels = 1) {
      dimnames_pstar_levels_table <-
        list(NULL, paste("Level", 1:num_pstar_levels))

      return(create_blank_parameter_table(
        num_rows = 1,
        num_cols = num_pstar_levels,
        dimnames = dimnames_pstar_levels_table
      ))
    },

    #' @description
    #' Reads in the values from the keyword parameter PSTAR from the
    #' AGEPRO Input file
    #'
    read_inp_lines = function(inp_con, nline) {
      cli::cli_alert_info("Reading to {.strong {private$.keyword_name}}")

      nline <- nline + 1

      #Read an additional line from the file connection, and assign it to
      #num_pstar_levels
      inp_line <- read_inp_numeric_line(inp_con)

      self$num_pstar_levels <- inp_line

      cli::cli_alert(paste0(
        "Line {nline}: ",
        "num_pstar_levels ",
        "(Number of Pstar Levels, {.emph KPStar}): ",
        "{.val {self$num_pstar_levels}}"
      ))

      # Create new pstar_level_table matrix based on num_pstar_levels
      self$pstar_levels_table <-
        self$create_blank_pstar_levels_table(self$num_pstar_levels)

      # Read an additional line from the file connection, delimit the line
      # by whitespace, and assign as pstar_levels_table
      nline <- nline + 1
      inp_line <- read_inp_numeric_line(inp_con)

      self$pstar_levels_table[1, ] <- inp_line

      cli::cli_alert(c(
        "Line {nline}: ",
        "pstar_levels_table ",
        "({.emph PStar}): ",
        "{.val {self$pstar_levels_table[1,]}} "
      ))

      # Read an additional line from the file connection, and assign it to
      # pstar_overfishing_f(PStarF)
      nline <- nline + 1
      inp_line <- read_inp_numeric_line(inp_con)

      self$pstar_overfishing_f <- inp_line
      cli::cli_alert(paste0(
        "Line {nline}: ",
        "pstar_overfsihing_f ",
        "(Overfishing Rate, {.emph PStarF}): ",
        "{.val {self$pstar_overfishing_f}}"
      ))

      # Read an additional line from the file connection, and assign it to
      # target_year(TargetYear)
      nline <- nline + 1
      inp_line <- read_inp_numeric_line(inp_con)

      self$target_year <- inp_line
      cli::cli_alert(paste0(
        "Line {nline}: ",
        "target_year: ",
        "{.val {self$target_year}}"
      ))

      return(nline)
    },

    #' @description
    #' Returns PStar projection Values formatted as AGEPRO input file lines.
    #'
    #' @template delimiter
    #'
    get_inp_lines = function(delimiter = " ") {
      return(c(
        self$inp_keyword,
        self$num_pstar_levels,
        paste(self$pstar_levels_table, collapse = delimiter),
        self$pstar_overfishing_f,
        self$target_year
      ))
    },

    #' @description
    #' Formatted to print out PStar values on Rconsole
    #'
    #' @template enable_cat_print
    #'
    print = function(enable_cat_print = TRUE) {
      cli::cli_ul()
      cli::cli_li(paste0(
        "num_pstar_levels ({.emph KPStar}): ",
        "{.val {self$num_pstar_levels}}"
      ))
      cli::cli_li(paste0(
        "pstar_overfsihing_f ({.emph PStarF}): ",
        "{.val {self$pstar_overfishing_f}}"
      ))
      cli::cli_li("target_year: {.val {self$target_year}}")

      ifelse(
        enable_cat_print,
        private$cat_print_pstar_levels_table(),
        #suppresses cli::cat_print
        capture.output(x <- private$cat_print_pstar_levels_table())
      )
      cli::cli_end()
    }
  ),
  active = list(
    #' @field num_pstar_levels
    #' Number of pstar values to be evaluated
    #'
    num_pstar_levels = function(value) {
      if (missing(value)) {
        private$.num_pstar_levels
      } else {
        checkmate::assert_numeric(
          value,
          len = 1,
          lower = 1,
          .var.name = "num_pstar_levels"
        )

        private$.num_pstar_levels <- value
      }
    },

    #' @field pstar_levels_table
    #' The vector of probabilities of overfishing or PStar values to be used
    #'
    pstar_levels_table = function(value) {
      if (missing(value)) {
        private$.pstar_levels_table
      } else {
        checkmate::assert_matrix(
          value,
          mode = "numeric",
          nrows = 1,
          min.cols = 1,
          .var.name = "pstar_levels_table"
        )

        private$.pstar_levels_table <- value
      }
    },

    #' @field pstar_overfishing_f
    #' Fishing mortality rate that defines the overfishing level
    pstar_overfishing_f = function(value) {
      if (missing(value)) {
        private$.pstar_overfishing_f
      } else {
        checkmate::assert_numeric(
          value,
          len = 1,
          lower = 0,
          .var.name = "pstar_overfishing_f"
        )

        private$.pstar_overfishing_f <- value
      }
    },

    #' @field json_list_object
    #' Returns JSON list object of PStar Projection values
    json_list_object = function() {
      return(list(
        k_pstar = self$num_pstar_levels,
        pstar_values = self$pstar_levels_table,
        pstar_f = self$pstar_overfishing_f,
        target_year = self$target_year
      ))
    }
  ),
  private = list(
    .keyword_name = "pstar",

    .num_pstar_levels = NULL,
    .pstar_levels_table = NULL,
    .pstar_overfishing_f = NULL,

    #Helper Function to printout num_pstar_levels to Rconsole
    cat_print_pstar_levels_table = function() {
      cli::cli_text("{symbol$bullet} pstar_levels_table ({.emph PStar}):")
      checkmate::assert_matrix(self$pstar_levels_table)
      cli::cat_print(self$pstar_levels_table)
    }
  )
)


#' @title
#' Input information for calculating F to rebuild spawning biomass
#'
#' @description
#' Rebuilding projections is focused on the calculation of the constant
#' total fishing mortality calculated across all fleets that will rebuild the
#' population, denoted as \eqn{F_{REBUILD}}.
#'
#' @details
#' Creating or importing `rebuild_projection` object will overwrite the
#' any existing rebuild and pstar projection objects.
#'
#' @template inp_con
#' @template nline
#' @template elipses
#'
#' @importFrom R6 R6Class
#'
#' @keywords projection_analyses
#'
#' @export
rebuild_projection <- R6Class(
  "rebuild_projection",
  inherit = projection_analyses,
  public = list(
    #' @description
    #' Initializes class
    #'
    #' @param proj_years May be a single numeric value: the number of years in the
    #' time projection; a vector of sequential values: Sequence of years in from
    #' first to last year of the time projection; or an instance of
    #' [Projection years][ageproR::projection_years]
    #' @param target_biomass Target biomass value in units of thousands
    #' of metric tons (MT). Default set to 0.
    #' @param target_type Target population biomass:
    #' \describe{
    #'   \item{0}{Spawning Stock Biomass. Set as Default}
    #'   \item{1}{January 1st Stock Biomass}
    #'   \item{2}{Mid-Year (Mean) Biomass}
    #' }
    #' @param target_percent The percent frequency that `target_year` reaches
    #' `target_biomass` from 0 to 100. Default set to 0.
    #' @param ... Other parameters to pass to
    #' [`projection_analyses`][ageproR::projection_analyses]
    #'
    initialize = function(
      proj_years,
      target_biomass = 0,
      target_type = 0,
      target_percent = 0,
      ...
    ) {
      super$initialize(proj_years, ...)

      self$target_biomass_value <- target_biomass
      self$target_biomass_type <- target_type
      self$target_percent <- target_percent
    },

    #' @description
    #' Reads in the values from the keyword parameter REBUILD from the
    #' AGEPRO Input file
    #'
    read_inp_lines = function(inp_con, nline) {
      cli::cli_alert_info("Reading to {.strong {private$.keyword_name}}")

      nline <- nline + 1

      #Read an additional line from the file connection, and split into
      #substrings by whitespace
      inp_line <- read_inp_numeric_line(inp_con)
      cli::cli_alert("Line {nline}: Reading Rebuilding Projection fields: ")

      self$target_year <- inp_line[1]
      self$target_biomass_value <- inp_line[2]
      self$target_biomass_type <- inp_line[3]
      self$target_percent <- inp_line[4]

      self$print()

      return(nline)
    },

    #' @description
    #' Returns rebuild projection Values formatted as AGEPRO input file lines.
    #'
    #' @template delimiter
    #'
    get_inp_lines = function(delimiter) {
      return(paste(
        c(
          self$target_year,
          self$target_biomass_value,
          self$target_biomass_type,
          self$target_percent
        ),
        collapse = delimiter
      ))
    },

    #' @description
    #' Prints out the rebuild projections fields to console
    #'
    print = function(...) {
      cli::cli_ul()
      cli::cli_li("target_year: {.val {self$target_year}}")
      cli::cli_li(paste0(
        "target_biomass_value: ",
        "{.val {self$target_biomass_value}} MT"
      ))
      cli::cli_li(paste0(
        "target_biomass_type: ",
        "{.val {self$target_biomass_type}}"
      ))
      private$print_name_target_biomass_type()
      cli::cli_li("target_percent: {.val {self$target_percent}} %")
      cli::cli_end()
    }
  ),
  active = list(
    #' @field json_list_object
    #' Returns JSON list object of rebuilder Projection values
    json_list_object = function() {
      return(list(
        target_year = self$target_year,
        target_value = self$target_biomass_value,
        target_type = self$target_biomass_type,
        target_percent = self$target_percent
      ))
    },

    #' @field target_biomass_value
    #' Rebuilding projection's target biomass value in units of thousands
    #' of metric tons (MT)
    target_biomass_value = function(value) {
      if (missing(value)) {
        private$.target_biomass_value
      } else {
        checkmate::assert_numeric(
          value,
          lower = 0,
          len = 1,
          .var.name = "target_biomass_value"
        )

        private$.target_biomass_value <- value
      }
    },

    #' @field target_biomass_type
    #' Index for the type of population biomass as the target:
    #' \describe{
    #'   \item{0}{Spawning Stock Biomass}
    #'   \item{1}{January 1st Stock Biomass}
    #'   \item{2}{Mid-Year (Mean) Biomass}
    #' }
    target_biomass_type = function(value) {
      if (missing(value)) {
        private$.target_biomass_type
      } else {
        checkmate::assert_numeric(
          value,
          len = 1,
          .var.name = "target_biomass_type"
        )
        checkmate::assert_choice(
          value,
          choices = c(0, 1, 2),
          .var.name = "target_biomass_type"
        )
        private$.target_biomass_type <- value
      }
    },

    #' @field target_percent
    #' The percent frequency of achieving the target value by the target
    #' year. The percent frequency is a value between 0 (a zero
    #' chance of achieving target) and 100 (indicating a 100 percent chance
    #' of achieving target).
    target_percent = function(value) {
      if (missing(value)) {
        private$.target_percent
      } else {
        checkmate::assert_numeric(
          value,
          len = 1,
          lower = 0,
          upper = 100,
          .var.name = "target_percent"
        )

        private$.target_percent <- value
      }
    }
  ),
  private = list(
    .keyword_name = "rebuild",

    .target_biomass_value = NULL,
    .target_biomass_type = NULL,
    .target_percent = NULL,

    .names_target_biomass_type = list(
      "0" = "Spawning Stock Biomass",
      "1" = "January 1st Stock Biomass",
      "2" = "Mid-Year (Mean) Biomass"
    ),

    print_name_target_biomass_type = function() {
      target_biomass_type_name <-
        private$.names_target_biomass_type[[
          as.character(self$target_biomass_type)
        ]]

      li_nested <-
        cli::cli_div(
          class = "target_type",
          theme = list(.target_type = list("margin-left" = 2))
        )
      cli::cli_text("{.emph {.field {target_biomass_type_name}}}")
      cli::cli_end(li_nested)
    },

    # Validates the usage of the 'projection years' parameter.
    #
    # If `proj_years` parameter is a projection_years class, then it will return
    # that value. Otherwise, it will create a new `projection_years` class based
    # on the param value passed.
    validate_proj_years_parameter = function(proj_years) {
      #Validate parameters
      if (checkmate::test_r6(proj_years, public = c("count", "sequence"))) {
        proj_years_class <- proj_years
      } else {
        proj_years_class <- ageproR::projection_years$new(proj_years)
      }

      return(proj_years_class)
    }
  )
)
