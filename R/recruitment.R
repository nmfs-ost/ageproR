#' @title AGEPRO Recruitment Parameter
#'
#' @description
#' Recruitment is the primary stochastic element in the AGEPRO model (Brodziak,
#' 2022). AGEPRO handles 21 Recruitment stochastic recruitment models for
#' projections. Please refer to the AGEPRO reference Manual for more detail on
#' these models.
#'
#' @template model_num
#' @template seq_years
#' @template proj_years_vector
#' @template elipses
#' @template inp_con
#' @template nline
#' @template delimiter
#'
#' @export
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#' @importFrom checkmate test_int assert_numeric assert_list assert_r6
#' @importFrom checkmate assert_int
#' @importFrom collections dict
#' @importFrom rlang expr eval_tidy
#' @importFrom purrr map
#'
recruitment <- R6Class(
  # nolint: cyclocomp_linter
  "recruitment",
  public = list(
    #' @description
    #' Initializes the Recruitment Class
    #'
    #' @param num_recruit_models
    #' Number of Recruitment Models in AGEPRO model. Default is 1.
    #'
    #' @param recruit_scaling_factor
    #' Recruit model's multiplier to convert units to absolute numbers of fish.
    #'
    #' @param ssb_scaling_factor
    #' Recruit model's multiplier to convert SSB to absolute
    #' spawning weight of fish in kilograms (kg)
    #'
    #' @param max_recruit_obs
    #' Max limit of recruitment observations. Default is 10000.
    #'
    #' @param enable_cat_print
    #' Flag to print out `cat` based cli messages printed on console. Default
    #' is TRUE.
    #'
    #'
    initialize = function(
      model_num,
      seq_years,
      num_recruit_models = 1,
      recruit_scaling_factor = 1000,
      ssb_scaling_factor = 1,
      max_recruit_obs = 10000,
      enable_cat_print = TRUE
    ) {
      #Handle seq_years as a single int or a vector of sequential values
      #This is used to set parameters for some recruitment models
      private$set_projection_years(seq_years)

      ## Validation
      # Check if input model number matches the number of observed years
      if (isFALSE(length(model_num) == num_recruit_models)) {
        stop(
          paste0(
            "Recruitment Model vector (model_num) object count ",
            "does not match number of recruits. ",
            "(count: ",
            length(model_num),
            ", number of recruits: ",
            num_recruit_models,
            ")"
          ),
          call. = FALSE
        )
      }

      # Setup number of recruits based on the vector length of the recruitment
      # models field sent to the function.
      private$.number_recruit_models <- length(model_num)

      # 'recruit' cli messages at initialization
      div_keyword_header(self$keyword_name)
      cli::cli_alert("Creating Default Recruitment Model")

      # Setup Recruitment Model data
      suppressMessages(private$set_recruit_scaling_factor(
        recruit_scaling_factor
      ))
      suppressMessages(private$set_ssb_scaling_factor(ssb_scaling_factor))
      suppressMessages(private$set_max_recruit_obs(max_recruit_obs))

      private$setup_recruit_model_num_list(model_num)

      private$setup_recruitment_probability()
      private$setup_recruit_data()

      self$print(enable_cat_print)
    },

    #' @description
    #' Prints out Recruitment
    #'
    #' @param enable_cat_print Flag to allow `cat` based cli messages printed on
    #' console. Default is TRUE
    print = function(enable_cat_print = TRUE, ...) {
      #verify private fields are numeric
      checkmate::assert_numeric(private$.number_recruit_models)
      checkmate::assert_numeric(private$.number_projection_years)
      #verify recruit_prob list
      checkmate::assert_list(private$.recruit_probability)

      cli::cli_alert(c(
        "{private$.number_recruit_models} recruitment model{?s}",
        " for {private$.number_projection_years} year{?s}."
      ))

      cli::cli_alert_info(
        "recruit_scaling_factor: {.val {self$recruit_scaling_factor}}"
      )
      cli::cli_alert_info(
        "ssb_scaling_factor: {.val {self$ssb_scaling_factor}}"
      )
      cli::cli_alert_info("max_recruit_obs: {.val {self$max_recruit_obs}}")

      #Module to printout Recruitment Probability
      #Verbose flag check
      if (enable_cat_print) {
        #Allow Recruitment Probability 'cat' cli message
        private$cli_recruit_probability()
      } else {
        #Suppress Recruitment Probability 'cat' cli message
        capture.output(x <- private$cli_recruit_probability())
      }

      cli::cli_alert_info("recruit_data:")
      for (recruit in 1:private$.number_recruit_models) {
        #par_recruit <- cli::cli_par()
        cli::cli_text("[[{recruit}]]")

        #Verify class inherits from "recruit_model"
        assert_r6(self$recruit_data[[recruit]], "recruit_model")
        self$recruit_data[[recruit]]$print(enable_cat_print = enable_cat_print)

        #cli::cli_end(par_recruit)
      }
    },

    #' @description
    #' Reads in Recruitment AGEPRO parameters from AGEPRO INP Input File
    #'
    #' @param num_recruit_models
    #' Number of Recruitment Models. Default is 1
    read_inp_lines = function(
      inp_con,
      nline,
      seq_years = 1,
      num_recruit_models = 1
    ) {
      #Setup .number_projection_years and .sequence_projection_years
      private$set_projection_years(seq_years)

      # Read an additional line from the file connection and split the string
      # into substrings by whitespace
      inp_line <- read_inp_numeric_line(inp_con)

      nline <- nline + 1
      cli_alert(
        "Line {nline} : Scaling Factors & Max Recruit Observations ..."
      )

      # Assign substrings (Console Output)
      cli::cli_div(
        id = "recruit_inp_fields",
        theme = list(".alert-info" = list("margin-left" = 2))
      )
      private$set_recruit_scaling_factor(inp_line[1])
      private$set_ssb_scaling_factor(inp_line[2])
      private$set_max_recruit_obs(inp_line[3])

      cli::cli_end("recruit_inp_fields")

      # Read an additional line from the file connection, and parse the
      # substring(s) for Recruitment Model(s) for recruit_data
      inp_line <- read_inp_numeric_line(inp_con)

      nline <- nline + 1
      #TODO: Refactor recruit_model_num_list to a function
      cli_alert(
        "Line {nline}: List of {num_recruit_models} recruitment model{?s} ..."
      )

      # Validate length Recruitment's recruit_model_num_list matches
      # Check if input model number matches the number of observed years
      if (isFALSE(length(inp_line) == num_recruit_models)) {
        stop(paste0(
          "Length of Recruitment number vector does not match ",
          "AGEPRO model's number of recruits: ",
          length(inp_line),
          " (Number of Recruits: ",
          num_recruit_models,
          ")"
        ))
      }

      # Setup number of recruits based on the vector length of the recruitment
      # models field sent to the function.
      private$.number_recruit_models <- length(inp_line)

      ## Setup for recruitment list vectors:
      private$setup_recruit_model_num_list(inp_line)
      private$setup_recruit_data()
      private$setup_recruitment_probability()

      # Assign "recruit type" inp_line values to recruit_model_num_list.
      div_model_num <-
        cli::cli_div(
          class = "input_field",
          theme = list(.input_field = list("margin-left" = 2))
        )
      for (recruit in 1:private$.number_recruit_models) {
        #Model Num
        private$set_recruit_model_num_list_item(inp_line[recruit], recruit)
        cli::cli_text(
          paste0(
            "{symbol$info} recruit_model_num_list[[",
            recruit,
            "]]: ",
            "{.val {private$.recruit_model_num_list[[recruit]]}}"
          )
        )
      }
      cli::cli_end(div_model_num)

      cli::cli_alert("Reading to {.strong recruit_probability} ... ")
      # Set Input File Recruitment Probability values over default values.
      # For each year in AGEPRO Model's observation years ...
      div_recruit_prob <-
        cli::cli_div(
          class = "input_field",
          theme = list(.input_field = list("margin-left" = 2))
        )
      for (year in private$.sequence_projection_years) {
        # Read an additional line from the file connection ...
        inp_line <- read_inp_numeric_line(inp_con)

        nline <- nline + 1

        # Verify recruit probability value ...
        assert_numeric(inp_line, lower = 0, upper = 1)

        # And then append line to the recruitment probability (list) ...
        for (j in seq_along(inp_line)) {
          private$set_recruit_probability_by_inp_line(
            j,
            year,
            inp_line[[j]],
            verbose = FALSE
          )
        }

        # Output Results when recruitment probabilities at year is done
        # Extract recruit_probability value per "Year"
        nline_year_rec_prob <- sapply(
          self$recruit_probability[seq(num_recruit_models)],
          "[[",
          as.character(year)
        )

        # Build String for Rconsole
        output_string <- paste0(
          "recruit_probability[[",
          seq(num_recruit_models),
          "]][[\"",
          as.character(year),
          "\"]]: ",
          nline_year_rec_prob,
          collapse = ",  "
        )

        # Output
        cli::cli_alert(c("Line {nline}: {.emph (year {year})} ", output_string))
      }
      cli::cli_end(div_recruit_prob)

      # For each recruit model in recruit_model_collection
      for (recruit in 1:private$.number_recruit_models) {
        #Setup Recruitment Model w/ default values
        private$.recruit_data[[recruit]] <-
          private$initialize_recruit_model(self$recruit_model_num_list[[
            recruit
          ]])

        cli::cli_alert_info("{.strong recruit_data[[{recruit}]]}:")

        #Nest Recruitment model read_inp_lines Output per model
        div_recruit_model <-
          cli::cli_div(
            class = "input_field",
            theme = list(.input_field = list("margin-left" = 2))
          )

        cli::cli_alert(
          "For {.strong recruit_data[[{recruit}]]}: ..."
        )
        #Read in inp lines to set recruitment model data values
        nline <-
          self$recruit_data[[recruit]]$read_inp_lines(inp_con, nline)

        cli::cli_end(div_recruit_model)
      }

      return(nline)
    },

    #' @description
    #' Returns the values for the RECRUIT keyword parameter formatted
    #' to the AGEPRO input file format.
    get_inp_lines = function(delimiter = " ") {
      # Set recruitment probability as a matrix and then separate matrix by
      # (year) rows as assign rows as separate list objects representing
      # AGEPRO input data file line. "unname" to remove data.frame labeling.
      # Collapse multi-recruit prob values as a single inpline list string.
      list_recruit_probability <-
        lapply(
          unname(as.list(
            data.frame(t(sapply(self$recruit_probability, matrix)))
          )),
          paste,
          collapse = delimiter
        )

      # Unlist each recruitment model's "inp_lines_recruit_data", via rapply:
      # returning a flat string vector of input data lines. Relist vector,
      # via "as.list", to convert to list of input data line elements.

      list_recruit_data <-
        as.list(unlist(rapply(
          self$recruit_data,
          f = function(X) {
            X$inp_lines_recruit_data(delimiter)
          },
          how = "list"
        )))

      return(c(
        list(
          self$inp_keyword,
          paste(
            self$recruit_scaling_factor,
            self$ssb_scaling_factor,
            self$max_recruit_obs,
            sep = delimiter
          ),
          # Append recruit list of recruit models as a single line
          paste(self$recruit_model_num_list, collapse = delimiter)
        ),
        # Append Recruit probability
        list_recruit_probability,
        # Append recruit data
        list_recruit_data
      ))
    }
  ),
  active <- list(
    #' @field recruit_scaling_factor
    #' The multiplier to convert recruitment submodel's recruitment units to
    #' absolute numbers of fish
    recruit_scaling_factor = function(value) {
      if (missing(value)) {
        return(private$.recruit_scaling_factor)
      }
      private$set_recruit_scaling_factor(value)
    },

    #' @field ssb_scaling_factor
    #' The multiplier to convert recruitment submodel's SSB to absolute
    #' spawning weight of fish in kilograms (kg)
    ssb_scaling_factor = function(value) {
      if (missing(value)) {
        return(private$.ssb_scaling_factor)
      }
      private$set_ssb_scaling_factor(value)
    },

    #' @field max_recruit_obs
    #' Recruitment submodel's maximum number of observations
    max_recruit_obs = function(value) {
      if (missing(value)) {
        return(private$.max_recruit_obs)
      }
      private$set_max_recruit_obs(value)
    },

    #' @field recruit_model_num_list
    #' Returns a list of recruitment model numbers representing the agepro model's
    #' recruitment model instance. This "read-only" field is informative of the current
    #' instance of the model recrutment model order. Use recruitment R6class field
    #' `recruit_data` to access recruitment model data.
    recruit_model_num_list = function(value) {
      if (isFALSE(missing(value))) {
        stop(
          "Values for recruit_model_num_list (recruit_data) are read only",
          call. = FALSE
        )
      }
      return(private$.recruit_model_num_list)
    },

    #' @field number_recruit_models
    #' Returns number of recruitment models. This "read-only" field is set when the
    #' recruitment model is initialized.
    number_recruit_models = function(value) {
      if (isFALSE(missing(value))) {
        stop(
          "Values for number_recruit_models are set when initializing recruitment model. ",
          call. = FALSE
        )
      }
      return(private$.number_recruit_models)
    },

    #' @field recruit_probability
    #' The Recruitment Probabilities.
    recruit_probability = function(value) {
      if (missing(value)) {
        return(private$.recruit_probability)
      }

      checkmate::assert_list(
        value,
        types = c("numeric"),
        len = private$.number_recruit_models
      )

      # Ensure inputs to recruit_probability are valid.
      # Check that input vector:
      # #1. Is uniquely named numeric vector that has values between 0 and 1
      # #2. Length of Input Vector matches the number of projection years
      value |>
        purrr::map(\(value) {
          checkmate::assert_numeric(
            value,
            len = private$.number_projection_years,
            upper = 1,
            lower = 0,
            names = "unique"
          )
        })

      # #3. Check names of numeric vector match the projection year sequence
      value |>
        purrr::map(\(value) {
          checkmate::assert_names(
            names(value),
            permutation.of = as.character(private$.sequence_projection_years)
          )
        })

      private$.recruit_probability <- value
    },

    #' @field recruit_data
    #' List containing data for each recruitment model in the recruitment
    #' model collection list. Use this field to access a specific recruitment models field.
    recruit_data = function(value) {
      if (missing(value)) {
        return(private$.recruit_data)
      } else {
        checkmate::assert_list(
          value,
          types = c("recruit_model", "R6"),
          len = length(private$.recruit_model_num_list),
          .var.name = "recruit_data"
        )

        # Copy the new "model_num" values from the recruit_data and
        # set it to recruit_model_num_list.
        model_num_values <- purrr::map(value, "model_num")
        if (
          isFALSE(all(
            model_num_values %in%
              private$.valid_recruit_model_num
          ))
        ) {
          stop("Invalid AGEPRO Recruitment Model Number(s) found.")
        }
        private$.recruit_model_num_list <- model_num_values

        private$.recruit_data <- value
      }
    },

    #' @field json_list_object
    #' List of RECRUIT keyword fields values, exportable to JSON.
    #'
    json_list_object = function() {
      recruit_json_list_object <- list(
        recFac = self$recruit_scaling_factor,
        ssbFac = self$ssb_scaling_factor,
        maxRecObs = private$.max_recruit_obs,
        #list of model numbers
        type = unlist(self$recruit_model_num_list),
        prob = self$recruit_probability
      )

      #recruitData

      tryCatch(
        {
          #Gather Recruit Model Data
          recruit_model_data_list <-
            vector("list", length(self$recruit_model_num_list))

          for (recruit in seq_along(self$recruit_model_num_list)) {
            recruit_model_data_list[[recruit]] <-
              self$recruit_data[[recruit]][["json_recruit_data"]]
          }

          return(c(
            recruit_json_list_object,
            recruitData = recruit_model_data_list
          ))
        },
        error = function(cond) {
          message(paste0(
            "Invalid Recruitment Model or model data Found, ",
            "returning NULL recruitData."
          ))
          #Nullify for JSON output
          return(c(
            recruit_json_list_object,
            recruitData = list(NULL)
          ))
        }
      )
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

    #' @field valid_recruit_models
    #' Returns vector of valid Recruitment Model Numbers
    valid_recruit_models = function() {
      return(private$.valid_recruit_model_num)
    }
  ),
  private = list(
    .number_projection_years = NULL,
    .number_recruit_models = NULL,
    .sequence_projection_years = NULL,
    .max_recruit_obs = NULL,

    .keyword_name = "recruit",
    .valid_recruit_model_num = c(0, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 14, 15),

    .recruit_scaling_factor = NULL,
    .ssb_scaling_factor = NULL,
    .recruit_data = NULL,

    .recruit_probability = NULL,
    .recruit_model_num_list = NULL,

    #private setters for recruitment fields
    set_recruit_scaling_factor = function(value) {
      checkmate::assert_numeric(
        value,
        len = 1,
        .var.name = "recruit_scaling_factor"
      )
      private$.recruit_scaling_factor <- value
      cli::cli_alert_info(paste0(
        "recruit_scaling_factor: ",
        "{.val {private$.recruit_scaling_factor}}"
      ))
    },

    set_ssb_scaling_factor = function(value) {
      checkmate::assert_numeric(
        value,
        len = 1,
        .var.name = "ssb_scaling_factor"
      )
      private$.ssb_scaling_factor <- value
      cli::cli_alert_info(paste0(
        "ssb_scaling_factor: ",
        "{.val {private$.ssb_scaling_factor}}"
      ))
    },

    set_max_recruit_obs = function(value) {
      checkmate::assert_int(value, .var.name = "max_recruit_obs")
      private$.max_recruit_obs <- value
      cli::cli_alert_info(paste0(
        "max_recruit_obs: ",
        "{.val {private$.max_recruit_obs}}"
      ))
    },

    # Validate and set input Recruitment Model Number to
    # recruit model number list
    set_recruit_model_num_list_item = function(value, index) {
      checkmate::assert_choice(value, choice = private$.valid_recruit_model_num)
      checkmate::assert_number(
        index,
        lower = 1,
        upper = length(private$.recruit_model_num_list)
      )
      private$.recruit_model_num_list[[index]] <- value
    },

    set_projection_years = function(obs_years) {
      checkmate::assert_numeric(obs_years, lower = 0)

      #Handle observed_years as single int or a vector of sequential values
      if (test_int(obs_years)) {
        #single
        private$.number_projection_years <- obs_years
        private$.sequence_projection_years <- 1:obs_years
      } else {
        private$.number_projection_years <- length(obs_years)
        private$.sequence_projection_years <- obs_years
      }
    },

    #Module to printout Recruitment probability to Rconsole
    cli_recruit_probability = function() {
      cli::cli_alert_info("recruitment_probability:")
      capture_output_as_message(cli::cat_print(private$.recruit_probability))
    },

    # Helper function to help setup recruit_model_num_list vector
    setup_recruit_model_num_list = function(model_num_vector) {
      # Validate number_recruit_models
      checkmate::assert_count(private$.number_recruit_models)

      #Setup Recruitment Model Number list
      private$.recruit_model_num_list <-
        vector("list", private$.number_recruit_models)

      # Set Recruitment Model Number for each number_recruit_model vector
      for (recruit in 1:private$.number_recruit_models) {
        private$set_recruit_model_num_list_item(
          model_num_vector[[recruit]],
          recruit
        )
      }
    },

    # Creates Recruitment Model Data.
    # Helper function to help setup .number_recruit_models,
    # recruit_model_num_list, & recruit_data vectors.
    setup_recruit_data = function() {
      # Validate number_recruit_models
      checkmate::assert_count(private$.number_recruit_models)

      #Setup Recruitment Model Data List
      private$.recruit_data <-
        vector("list", private$.number_recruit_models)

      #Set Model data for each recruitment model.
      for (recruit in 1:private$.number_recruit_models) {
        #Add Recruitment Data with recruitment model number
        current_recruit_num <- self$recruit_model_num_list[[recruit]]
        private$.recruit_data[[recruit]] <-
          private$initialize_recruit_model(current_recruit_num)
      }
    },

    #Setup recruitment probability for each recruitment model.
    setup_recruitment_probability = function() {
      assert_numeric(private$.number_recruit_models, lower = 1)
      assert_numeric(private$.number_projection_years, lower = 1)
      assert_numeric(private$.sequence_projection_years)

      #Setup Recruitment Probability list
      private$.recruit_probability <-
        vector("list", private$.number_recruit_models)

      for (recruit in 1:private$.number_recruit_models) {
        # Recruitment Probability: Fill the time series with a recruitment
        # probability sums equal to unity
        private$.recruit_probability[[recruit]] <-
          as.numeric(format(
            round(
              rep(1, private$.number_projection_years) /
                private$.number_recruit_models,
              digits = 4
            ),
            nsmall = 4
          ))

        names(private$.recruit_probability[[recruit]]) <-
          private$.sequence_projection_years
      }
    },

    # Sets the recruitment probability
    #
    set_recruit_probability_by_inp_line = function(
      j,
      year,
      value,
      verbose = TRUE
    ) {
      assert_int(j, lower = 1, upper = private$.number_recruit_models)
      assert_numeric(year, max.len = length(self$recruit_probability[[j]]))
      assert_numeric(value, lower = 0, upper = 1, max.len = length(year))

      if (!all(year %in% private$.sequence_projection_years)) {
        stop(paste0(
          "Year ",
          subset(year, !(year %in% private$.sequence_projection_years)),
          " is not within model projected year time horizon.\n  "
        ))
      }

      private$.recruit_probability[[j]][as.character(year)] <- value

      if (verbose) private$cli_recruit_probability()
    },

    # Initializes Recruit Model Data.
    #
    initialize_recruit_model = function(model_num) {
      checkmate::assert_choice(
        model_num,
        choice = private$.valid_recruit_model_num
      )

      # `.number_projection_years` is used for recruitment
      # models that use the model projection year time horizon for setup.
      #
      # Uses a pyhton-like data structure (collections::dict) to store
      # expressions and evaluate via tidy evaluation to initialize recruitment
      # models
      model_dict <- dict(list(
        "0" = rlang::expr(null_recruit_model$new()),
        "2" = rlang::expr(empirical_ssb$new(
          private$.number_projection_years
        )),
        "3" = rlang::expr(empirical_distribution_model$new(
          private$.number_projection_years
        )),
        "4" = rlang::expr(two_stage_empirical_ssb$new()),
        "5" = rlang::expr(beverton_holt_curve_model$new()),
        "6" = rlang::expr(ricker_curve_model$new()),
        "7" = rlang::expr(shepherd_curve_model$new()),
        "9" = rlang::expr(deprecated_recruit_model_9$new()),
        "10" = rlang::expr(beverton_holt_autocorrelated_error$new()),
        "11" = rlang::expr(ricker_curve_autocorrelated_error$new()),
        "12" = rlang::expr(shepherd_curve_autocorrelated_error$new()),
        "14" = rlang::expr(empirical_cdf_model$new()),
        "15" = rlang::expr(two_stage_empirical_cdf$new())
      ))

      rlang::eval_tidy(
        model_dict$get(
          as.character(model_num),
          default = stop(
            paste0(
              "Recruitment Model Number ",
              model_num,
              " does not match list ",
              "of known recruitment models ",
              " to initialize."
            ),
            call. = FALSE
          )
        )
      )
    }
  )
)
