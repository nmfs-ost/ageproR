
#' @title R6 class representing AGEPRO model
#'
#' @description
#' AGEPRO model contains the projection time horizon, age class range, number
#' of fleets, recruitment, and uncertainties
#'
#' @details
#' AGEPRO performs stochastic projections on exploited fisheries stock to
#' determine age-structured population over a time period. Brodziak, 2022
#'
#' @template model_num
#' @template elipses
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom checkmate test_logical assert_number assert_file_exists
#'
#' @examples
#' \dontrun{
#' # General parameters for 2019-2026 Uku Projections Base (Example 4)
#' test <- agepro_model$new(2019,2026,1,32,1000,4,3,0,300)
#' }
#'
agepro_model <- R6Class(
  classname = "agepro_model",
  public = list(

    #' @description
    #' Initializes the instance of the AGEPRO Model
    #'
    #' @template model_general_params
    #' @param show_general_params
    #' Logical flag to show AGEPRO model's general parameters on R console.
    #' TRUE, by default.
    #' @param enable_cat_print
    #' Logical flag to show target function's **cli**
    #' [`cat_print`][cli::cat_print] messages to be seen on console. This is
    #' also used to print verbose information about the the initialized object.
    #' By default, this is set to TRUE.
    #'
    initialize = function(yr_start,
                           yr_end,
                           age_begin,
                           age_end,
                           num_pop_sims,
                           num_fleets,
                           num_rec_models,
                           discards_present = FALSE,
                           seed = sample.int(1e8, 1),
                          show_general_params = TRUE,
                          enable_cat_print = TRUE,
                          ...) {

      #Current Input File Version
      private$.ver_inpfile_string = private$.currentver_inpfile_string
      private$.ver_jsonfile_format = 0
      private$setup_ver_rpackage()

      #Set GENERAL
      self$general <- general_params$new(yr_start,
                                         yr_end,
                                         age_begin,
                                         age_end,
                                         num_pop_sims,
                                         num_fleets,
                                         num_rec_models,
                                         discards_present,
                                         seed,
                                         enable_cat_print = show_general_params)

      ## Helper function to create a new instance of agepro_model
      if(enable_cat_print){

        self$default_agepro_keyword_params(self$general,
                                           enable_cat_print = enable_cat_print,
                                           ...)
      }else{

        suppressMessages(
          self$default_agepro_keyword_params(self$general,
                                           enable_cat_print = enable_cat_print,
                                           ...))
      }


      cli::cli_alert_success("Agepro Model Initialized")
      invisible(self)
    },

    #' @description
    #' This will create default values for each primary AGEPRO keyword
    #' parameter based on the values passed by the `general_params` class.
    #' Optional keyowrd parameters, such as as discard fraction or discard
    #' weight may be created, if enabled.
    #'
    #' @param x [General Params][ageproR::general_params] class object.
    #' @param projection_analyses_type Type of projection analyses type.
    #' Default is `"standard"`.
    #'
    #' @template enable_cat_print
    #'
    default_agepro_keyword_params = function (x, projection_analyses_type =
                                                "standard",
                                              enable_cat_print = TRUE,
                                              ...) {

      #Verify general param
      checkmate::assert_r6(x, public = c("yr_start",
                                         "yr_end",
                                         "age_begin",
                                         "age_end",
                                         "num_pop_sims",
                                         "num_fleets",
                                         "num_rec_models",
                                         "discards_present",
                                         "seed"),
                           .var.name = "general")

      #Assign and verify projection_analyses_type
      self$setup_projection_analyses_values(projection_analyses_type)

      private$.discards_present <- x$discards_present

      self$case_id <- case_id$new("Unnamed AGEPRO model")

      #TODO: rename cat_verbose to enable_cat_print
      self$recruit <- recruitment$new(rep(0, x$num_rec_models),
                                      x$seq_years,
                                      num_recruit_models = x$num_rec_models,
                                      enable_cat_print = enable_cat_print,
                                      ...)

      self$bootstrap <- bootstrap$new()

      self$natmort <-
        natural_mortality$new(x$seq_years,
                              x$num_ages,
                              enable_cat_print = enable_cat_print)

      self$maturity <-
        maturity_fraction$new(x$seq_years,
                              x$num_ages,
                              enable_cat_print = enable_cat_print)

      self$fishery <-
        fishery_selectivity$new(x$seq_years,
                                x$num_ages,
                                x$num_fleets,
                                enable_cat_print = enable_cat_print)

      self$stock_weight <-
        jan_stock_weight_age$new(x$seq_years,
                                 x$num_ages,
                                 enable_cat_print = enable_cat_print)

      self$ssb_weight <-
        spawning_stock_weight_age$new(x$seq_years,
                                      x$num_ages,
                                      enable_cat_print = enable_cat_print)

      self$mean_weight <-
        mean_population_weight_age$new(x$seq_years,
                                       x$num_ages,
                                       enable_cat_print = enable_cat_print)

      self$catch_weight <-
        landed_catch_weight_age$new(x$seq_years,
                                    x$num_ages,
                                    x$num_fleets,
                                    enable_cat_print = enable_cat_print)

      if(as.logical(x$discards_present)) {

        self$discard <-
          discard_fraction$new(x$seq_years,
                               x$num_ages,
                               x$num_fleets,
                               enable_cat_print = enable_cat_print)

        self$disc_weight <-
          discard_weight_age$new(x$seq_years,
                                 x$num_ages,
                                 x$num_fleets,
                                 enable_cat_print = enable_cat_print)
      }

      self$harvest <-
        harvest_scenario$new(x$seq_years,
                             x$num_fleets,
                             enable_cat_print = enable_cat_print)

      self$biological <-
        mortality_fraction_prior_spawn$new(x$seq_years,
                                           enable_cat_print = enable_cat_print)

      self$options <- options_output$new()


      if(self$projection_analyses_type == "pstar") {
        self$pstar <-
          pstar_projection$new(x$seq_years,
                               enable_cat_print = enable_cat_print)
      }

      if(self$projection_analyses_type == "rebuild") {
        self$rebuild <-
          rebuild_projection$new(x$seq_years)
      }

      self$perc <- percentile_summary$new(perc_flag = options_flags$new())

      self$bounds <- max_bounds$new(bounds_flag = options_flags$new())

      self$refpoint <- reference_points$new(refpoint_flag = options_flags$new())

      self$scale <- scaling_factors$new(scale_flag = options_flags$new())

      self$retroadjust <- retrospective_adjustments$new(
        enable_cat_print = enable_cat_print,
        retro_flag = options_flags$new())


    },

    #' @description
    #' Setup recruitment with a recruitment model collection list with default
    #' data using current AGEPRO model's number of recruits and sequence of
    #' projection years.
    #'
    #' To establish multiple recruit models, pass multiple valid AGERPRO
    #' Recruitment Model numbers as vector to the `model_num` parameter. If
    #' the vector length of `model_num` doesn't match current AGEPRO
    #' [general parameter's][ageproR::general_params] `num_rec_models`
    #' value, it will throw an error.
    #'
    #'
    set_recruit_model = function(..., enable_cat_print = TRUE) {

      validation_error <- checkmate::makeAssertCollection()
      # Custom Validation check to input format
      assert_model_num_vector_format(list(...), add = validation_error,
                                   .var.name = "model_num")

      # TODO: Check for Multiple Recruitment Model number "1"

      list_is_numeric <- checkmate::check_list(list(...), types = "numeric")
      if(isTRUE(list_is_numeric)) {

        #Combines lists elements as a vector
        model_num <- purrr::list_c(list(...))
        # Throw informative error if model_num count doesn't match
        # num_recruit_models
        assert_model_num_vector_count(model_num, self$general$num_rec_models,
                                      add = validation_error)


        sapply(model_num, function(.X) {
          checkmate::assert_choice(.X,
                                   choices = self$recruit$valid_recruit_models,
                                   add = validation_error,
                                   .var.name = deparse(.X))
        })
      } else{
        #Throw the error message to the validation_error assertion
        validation_error$push(list_is_numeric)
        sapply(list(...),function(.X){
          checkmate::assert_numeric(.X, .var.name=deparse(.X),
                                    add = validation_error )})
      }

      checkmate::reportAssertions(validation_error)


      cli_alert("{.emph Recruitment Data Setup}")
      cli_alert("{.emph Using Model Number {.field {model_num}}}")

      self$recruit <- recruitment$new(model_num,
                      seq_years = self$general$seq_years,
                      num_recruit_models = self$general$num_rec_models,
                      enable_cat_print = enable_cat_print)



    },

    #' @description
    #' Wrapper function to call bootstrap's set_bootstrap_filename
    #'
    #' @param bsnfile bootstrap filename
    set_bootstrap_filename = function(bsnfile) {

      self$bootstrap$set_bootstrap_filename(bsnfile)
    },


    #' @description
    #' Helper Function to setup agepro model's projection analyses type. agepro
    #' models use standard projection analyses by default, and do not require
    #' additional keyword parameter setup. "pstar" and  "rebuild" projection
    #' analyses types require their own keyword parameter classes to setup.
    #' However, they are not created during model initialization.
    #'
    #' AGEPRO models must can not have both pstar and rebuilder projection
    #' analyses,
    #'
    #' @param type projection_analyses_type
    #'
    #'
    #'
    setup_projection_analyses_values = function(type,
                                            enable_cat_print = FALSE) {

      previous_projection_type <- self$projection_analyses_type

      cli::cli_alert(paste0("AGEPRO Model Projection analyses type set ",
                                 "to {.field {type}}"))
      tryCatch({

          #Clean PSTAR and REBULD
          if(isFALSE(is.null(self$pstar)))  {
            self$pstar <- NULL
          }
          if(isFALSE(is.null(self$rebuild))) {
            self$rebuild <- NULL
          }

          #if(self$projection_analyses_type == "pstar") {
          if(type == "pstar"){

            self$pstar <- pstar_projection$new(self$general$seq_years)
            cli::cli_alert("Default values for PStar projection created.")

          #}else if(self$projection_analyses_type == "rebuild") {
          }else if(type =="rebuild") {

            self$rebuild <- rebuild_projection$new(self$general$seq_years)
            cli::cli_alert("Default values for Rebuild Projection created.")

          }

          # projection_analyses_type active binding will verify input value
          # `type` is "standard", "pstar", or "rebuild" before being assiged
          self$projection_analyses_type <- type

        },
        error = function(cond) {

          message(paste0("Revert to previous value: ",
                         previous_projection_type))

        }
      )

    }

  ),
  active = list(

    #' @field ver_inpfile_string
    #' Version string on AGEPRO input files (*.inp) for version compatibility
    #' with Jon Brodiak's AGEPRO calculation engine.
    ver_inpfile_string = function(value){
      if(missing(value)){
        return(private$.ver_inpfile_string)
      } else {
        checkmate::assert_character(value,
                                    pattern="AGEPRO VERSION")
        private$.ver_inpfile_string <- value
      }
    },

    #' @field ver_json_format
    #' JSON Input File Format version.
    ver_json_format = function(value) {
      if(missing(value)) {
        return(private$.ver_json_format)
      }else{
        checkamate::as.numeric(value)
        cli::cli_alert("JSON Version:")
      }
    },

    #' @field ver_rpackage
    #' Returns ageproR r package version.
    ver_rpackage = function(value){
      if(missing(value)){
        return(private$.ver_rpackage)
      }else{
        #use as.numeric_version to validate
        cli::cli_alert_info("Version: {as.numeric_version(value)}")
        private$.ver_rpackage <- value
      }
    },

    #' @field projection_analyses_type
    #' Type of projection analyses: standard, rebuilding, pstar.
    projection_analyses_type = function(value) {
      if(missing(value)){
        return(private$.projection_analyses_type)
      } else{
        checkmate::assert_choice(value,
                                 choices = c("standard","rebuild", "pstar"),
                                 .var.name = "projection_analyses_type")
        private$.projection_analyses_type <- value
      }
    },

    #' @field case_id
    #' Title identifying AGEPRO model attributes
    case_id = function(value) {
      if(missing(value)){
        return(private$.case_id)
      }else {
        tryCatch({

          # Validate value as case_id R6class if value includes the "model_name"
          # (active binding) public field
          assert_case_id_active_binding(value, .var.name = "case_id")

          private$.case_id <- value

        },
        error = function(err) {

          message(paste0("Error: \n", gsub("\\.$","",conditionMessage(err)) ))
        })

      }
    },

    #' @field general
    #' General Options
    general = function(value) {
      if(missing(value)){
        return(private$.general_options)
      }else {
        checkmate::assert_r6(value)
        private$.general_options <- value
      }
    },

    #' @field bootstrap
    #' Bootstrapping
    bootstrap = function(value) {
      if(missing(value)){
        return(private$.bootstrap)
      }else {
        checkmate::assert_r6(value, .var.name = "bootstrap")
        private$.bootstrap <- value
      }
    },


    #' @field natmort
    #' Natural Mortality
    natmort = function(value){
      if(missing(value)){
        return(private$.natural_mortality)
      }else {
        checkmate::assert_r6(value, classes = "process_error")
        private$.natural_mortality <- value
      }
    },

    #' @field maturity
    #' Maturity Fraction
    maturity = function(value){
      if(missing(value)){
        return(private$.maturity_fraction)
      }else {
        checkmate::assert_r6(value, classes= "process_error")
        private$.maturity_fraction <- value
      }
    },

    #' @field fishery \cr
    #' Fishery Selectivity
    fishery = function(value) {
      if(missing(value)) {
        return(private$.fishery_selectivity)
      }else {
        checkmate::assert_r6(value, classes = "process_error")
        private$.fishery_selectivity <- value
      }
    },

    #' @field discard \cr
    #' Discard Fraction
    discard = function(value) {
      if(missing(value)) {
        return(private$.discard_fraction)
      }else {
        checkmate::assert_r6(value,
                             classes = c("process_error",
                                         "discard_fraction"),
                             null.ok = TRUE,
                             .var.name = "discard")
        private$.discard_fraction <- value
      }
    },

    #' @field stock_weight
    #' Stock weight on January 1st at age
    stock_weight = function(value) {
      if(missing(value)){
        return(private$.jan_stock_weight_age)
      }else {
        checkmate::assert_r6(value, classes = "process_error")
        private$.jan_stock_weight_age <- value
      }
    },

    #' @field ssb_weight
    #' Spawning Stock Weight of Age
    ssb_weight = function(value) {
      if(missing(value)){
        return(private$.spawning_stock_weight_age)
      }else {
        checkmate::assert_r6(value, classes = "process_error")
        private$.spawning_stock_weight_age <- value
      }
    },

    #' @field mean_weight
    #' Midyear mean population weight at age
    mean_weight = function(value) {
      if(missing(value)){
        return(private$.mean_population_weight_age)
      } else{
        checkmate::assert_r6(value, classes = "process_error")
        private$.mean_population_weight_age <- value
      }
    },

    #' @field catch_weight
    #' Landed catch weight at age by fleet
    catch_weight = function(value) {
      if(missing(value)){
        return(private$.landed_catch_weight_age)
      } else{
        checkmate::assert_r6(value, classes = "process_error")
        private$.landed_catch_weight_age <- value
      }
    },

    #' @field disc_weight
    #' Discard weight of age by fleet
    disc_weight = function(value) {
      if(missing(value)) {
        return(private$.discard_weight_age)
      } else {
        checkmate::assert_r6(value,
                             classes = c("process_error",
                                         "discard_weight_age"),
                             null.ok = TRUE,
                             .var.name = "disc_weight")
        private$.discard_weight_age <- value
      }
    },

    #' @field harvest
    #' Harvest intensity (of fishing mortality or landings quota) by fleet
    harvest = function(value) {
      if(missing(value)){
        return(private$.harvest_scenario)
      }else{
        checkmate::assert_r6(value,
                             public = c("harvest_specifications",
                                        "harvest_value"),
                             .var.name = "harvest")

        private$.harvest_scenario <- value
      }
    },


    #' @field recruit
    #' AGEPRO Recruitment Model information
    recruit = function(value) {
     if(missing(value)) {
       return(private$.recruitment)
     }else {
       #Check
       checkmate::assert_r6(value, public = c("recruit_scaling_factor",
                                              "ssb_scaling_factor",
                                              "recruit_probability"),
                            .var.name = "recruit")
       private$.recruitment <- value
     }
    },


    #' @field perc
    #' User-selected percentile summary of the key results in the output file.
    #'
    perc = function(value) {
      if(missing(value)){
        return(private$.percentile_summary)
      }else{
        tryCatch({

          #Validate value as percentile_summary R6class
          assert_perc_active_binding(value, .var.name = "perc")

          private$.percentile_summary <- value

        },
        error = function(err) {

          message(paste0("Error: \n", gsub("\\.$","",conditionMessage(err)) ))
        })
      }
    },


    #' @field bounds
    #' Bounds on simulated fish weights and natural mortality rates
    #'
    bounds = function(value) {
      if(missing(value)){
        return(private$.max_bounds)
      }else{
        tryCatch({

          #Validate value as percentile_summary R6class
          assert_bounds_active_binding(value, .var.name = "bounds")

          private$.max_bounds <- value

        },
        error = function(err) {

          message(paste0("Error: \n", gsub("\\.$","",conditionMessage(err)) ))
        })
      }
    },

    #' @field refpoint
    #' Reference points for optional AGEPRO output threshold report.
    #'
    refpoint = function(value) {
      if(missing(value)) {
        return(private$.reference_points)
      }else{

        #validate if input value is reference_points class
        refpoint_fields <- c("ssb_threshold",
                             "stock_biomass_threshold",
                             "mean_biomass_threshold",
                             "fishing_mortality_threshold")
        checkmate::assert_r6(value, public = refpoint_fields,
                             .var.name = "refpoint")

        private$.reference_points <- value

      }
    },

    #' @field scale
    #' Scaling factors for biomass, recruitment, and stock size
    #'
    scale = function(value) {
      if(missing(value)) {
        return(private$.scaling_factors)
      }else {
        scale_fields <- c("biomass_scale",
                          "recruitment_scale",
                          "stock_size_scale")
        checkmate::assert_r6(value, public = scale_fields,
                             .var.name = "scale")

        private$.scaling_factors <- value
      }
    },

    #' @field retroadjust
    #' Retrospective bias Adjustment
    retroadjust = function(value) {
      if(missing(value)) {
        return(private$.retrospective_adjustments)
      }else {
        checkmate::assert_r6(value, public = c("retro_adjust"),
                             .var.name = "retroadjust")

        private$.retrospective_adjustments <- value
      }
    },

    #' @field pstar
    #' Calculating Total Allowable Catch \eqn{TAC} to produce \eqn{P*}, the
    #' probability of overfishing in the target year.
    pstar = function(value) {
      if(missing(value)){
        return(private$.pstar_projection)
      }else {
        #Check if current projection_analyses_type is not REBUILD
        if(self$projection_analyses_type == "rebuild"){
          stop(paste0("Assigning value PSTAR projection object but ",
                    "current projection_analyses_type is REBUILD"))
        }
        checkmate::assert_r6(value, public = c("target_year",
                                               "num_pstar_levels",
                                               "pstar_levels_table",
                                               "pstar_overfishing_f"),
                             null.ok = TRUE,
                             .var.name = "pstar")
        private$.pstar_projection <- value
      }
    },


    #' @field rebuild
    #' calculation of the constant total Fishing Mortality \eqn{F}
    #' calculated across all fleets with the rebuild spawning biomass.
    rebuild = function(value) {
      if(missing(value)){
        return(private$.rebuild_projection)
      }else {
        #Check if current active projection_analyses_type is not PSTAR
        if(self$projection_analyses_type == "pstar"){
          stop(paste0("Assigning value to REBUILD projection object but ",
                      "current projection_analyses_type is PSTAR"))
        }
        checkmate::assert_r6(value,
                             public = c("target_year",
                                        "target_biomass_value",
                                        "target_biomass_type",
                                        "target_percent"),
                             null.ok = TRUE,
                             .var.name = "rebuild")

        private$.rebuild_projection <- value

      }

    },

    #' @field biological
    #' Seasonal spawning timing for fishing mortality (\eqn{F}) and natural
    #' mortality (\eqn{M})
    #'
    biological = function(value) {
      if(missing(value)) {
        return(private$.mortality_fraction_prior_spawn)
      }else{
        checkmate::check_r6(value,
                            public = c("time_varying",
                                       "proportion_total_mortality"))

        private$.mortality_fraction_prior_spawn <- value
      }

    },


    #' @field options
    #' Options for AGEPRO projection output
    #'
    options = function(value) {
      if(missing(value)){
        return(private$.options_output)
      }else {
        checkmate::check_r6(value,
                            public = c("auxfile_output_flag",
                                       "process_error_datafiles",
                                       "export_df"))
        private$.options_output <- value
      }
    },

    #' @field supported_inpfile_versions
    #' Supported AGEPRO Input File formats
    #'
    supported_inpfile_versions = function(){
      return(c(private$.currentver_inpfile_string,
               "AGEPRO VERSION 4.0",
               "AGEPRO VERSION 4.25"))
    }


  ),
  private = list(

    #Version
    .ver_inpfile_string = NULL,
    .ver_jsonfile_format = NULL,
    .ver_rpackage = NULL,

    #AGEPRO Input File version
    .currentver_inpfile_string = "AGEPRO VERSION 4.25",
    .agepro40_inpfile_string = "AGEPRO VERSION 4.0",

    # AGEPRO keyword parameters
    .case_id = NULL,
    .general_options = NULL,
    .recruitment = NULL,
    .bootstrap = NULL,
    .natural_mortality = NULL,
    .maturity_fraction = NULL,
    .fishery_selectivity = NULL,
    .discard_fraction = NULL,
    .jan_stock_weight_age = NULL,
    .spawning_stock_weight_age = NULL,
    .mean_population_weight_age = NULL,
    .landed_catch_weight_age = NULL,
    .discard_weight_age = NULL,
    .harvest_scenario = NULL,
    .pstar_projection = NULL,
    .rebuild_projection = NULL,
    .mortality_fraction_prior_spawn = NULL,
    .options_output = NULL,
    .percentile_summary = NULL,
    .max_bounds = NULL,
    .reference_points = NULL,
    .scaling_factors = NULL,
    .retrospective_adjustments = NULL,

    .discards_present = NULL,
    .projection_analyses_type = NULL,

    setup_ver_rpackage = function() {
      private$.ver_rpackage = utils::packageVersion("ageproR")
    }

  )

)


#' AGEPRO Input File Model
#'
#' File Functionality is based on AGEPRO-CoreLib implementation
#'
#' @template inp_line
#' @template inp_con
#' @template delimiter
#' @template elipses
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom checkmate assert_character
#' @importFrom collections dict
#'
agepro_inp_model <- R6Class(
  "agepro_inp_model",
  inherit = agepro_model,
  public = list(

    #' @description
    #' Initializes an instance of the AGEPRO model with AGEPRO input file
    #' format functions. A default model can be initialized without setting
    #' [general_params][ageproR::general_params] parameter values. The default
    #' values for the default model is:
    #'
    #' \itemize{
    #'  \item Projection years: From `yr_start` 0 to `yr_end` 2
    #'  \item Ages: From `age_begin` 1 to `age_end` 6
    #'  \item 1000 Population Simulations (`num_pop_sims`)
    #'  \item 1 Fleet (`num_fleets`)
    #'  \item 1 Recruit Model (`num_rec_models`)
    #'  \item Discards Present (`discards_present`): `FALSE` (or 0)
    #'  \item Pseudo-Randomly generated `seed`
    #' }
    #'
    #' @template model_general_params
    #' @param enable_cat_print
    #' Logical flag to show target function's **cli** [`cat_print`][cli::cat_print]
    #' messages to be seen on console. In this instance, this is set to TRUE
    #' @param show_general_params
    #' Logical flag to show AGEPRO model's general parameters on R console.
    #' TRUE, by default.
    #'
    initialize = function(yr_start = 0,
                          yr_end = 2,
                          age_begin = 1,
                          age_end = 6,
                          num_pop_sims = 1000,
                          num_fleets = 1,
                          num_rec_models = 1,
                          discards_present = 0,
                          seed = sample.int(1e8, 1),
                          enable_cat_print = TRUE,
                          show_general_params = TRUE,
                          ...) {


      if(all(isTRUE(c(
        missing(yr_start),
        missing(yr_end),
        missing(age_begin),
        missing(age_end),
        missing(num_pop_sims),
        missing(num_fleets),
        missing(num_rec_models)
      )))){
        cli::cli_alert("Setting up defualt AGEPRO model w/ default values")
      }

      super$initialize(yr_start,
                       yr_end,
                       age_begin,
                       age_end,
                       num_pop_sims,
                       num_fleets,
                       num_rec_models,
                       discards_present,
                       seed,
                       enable_cat_print = enable_cat_print,
                       show_general_params = show_general_params,
                       ...)

    },

    #' @description
    #' Read AGEPRO INP Input Files
    #'
    #' @param inpfile input file name
    read_inp = function(inpfile) {


      if (missing(inpfile)) {

        inpfile <- open_file_dialog(c("AGEPRO input File", ".inp"))
        #Exit Function if user cancels out of file dialog
        if (!test_file_exists(inpfile, access = "r", extension = "inp")) {
          return(invisible(NULL))
        }
      }

      ##Verify that input file location is valid
      checkmate::assert_file_exists(inpfile, access = "r", extension = "inp")

      tryCatch(
        {
          #(Re)Set File connection to input file
          inp_con <- file(file.path(inpfile), "r")
          #Hacky way to get input filename from file connection
          private$set_inp_filepath(summary(inp_con)$description)

          self$setup_projection_analyses_values("standard")
          self$read_inpfile_values(inp_con)

          #Cleanup and close file connections
          cli::cli_alert("Input File Read")

        },
        error = function(cond) {
          cli::cli_alert_danger("There was an error reading this file. \n  {cond}")

          #Reset projection_analyses_type
          self$projection_analyses_type <- "standard"
          suppressMessages(self$perc$enable_percentile_summary <- FALSE)
          return(invisible())
        },
        finally = {
          cli::cli_alert("Closing connection to file.")
          close(inp_con)
        }
      )
    },

    #' @description
    #' Read Input file Values
    #'
    #' @export
    #'
    read_inpfile_values = function(inp_con) {

      ## TODO: Refactor to read inpfile version

      # Assume line 1 is version string
      self$nline <- 1

      div_line1_alert = function() {
        cli::cli_div(class = "tmp",
                     theme = list(.tmp = list(color="darkorange",
                                              "font-weight" = "bold")))
        cli::cli_alert("line {self$nline}:", class = "tmp")
      }
      div_line1_alert()

      # Read line 1 from file connection
      inp_line1 <- readLines(inp_con, n = 1, warn = FALSE)
      cli::cli_alert_info("Version: '{inp_line1}'")

      # Validate Line 1 for Input File VERSION format.
      # - Non-valid/Non-Supported Strings returns Error
      # - Warn if AGEPRO VERSION 4.0 was found
      private$check_inpfile_version(inp_line1)
      self$ver_inpfile_string <- inp_line1


      #loop through inpfile to read in value fore each parameter keyword
      while (TRUE) {
        inp_line <- readLines(inp_con, n = 1, warn = FALSE)
        if (length(inp_line) == 0) {
          break
        }

        self$nline <- self$nline + 1
        self$match_keyword(inp_line, inp_con)

      }

    },

    #' @description
    #' Match Keyword
    #'
    match_keyword = function(inp_line, inp_con) {

      #Tidy evaluation evaluate wrapper functions
      keyword_dict <- dict(list(
        "[CASEID]" = {
          rlang::expr(private$read_case_id(inp_con, self$nline))
        },
        "[GENERAL]" = {
            rlang::expr(private$read_general_params(inp_con, self$nline))
        },
        "[RECRUIT]" = {
            rlang::expr(private$read_recruit(inp_con, self$nline))
        },
        "[BOOTSTRAP]" = {
          rlang::expr(private$read_bootstrap(inp_con, self$nline))
        },
        "[NATMORT]" = {
            rlang::expr(private$read_natural_mortality(inp_con, self$nline))
        },
        "[MATURITY]" = {
          rlang::expr(private$read_maturity_fraction(inp_con, self$nline))
        },
        "[BIOLOGICAL]" = {
          rlang::expr(
            private$read_mortality_fraction_prior_spawn(inp_con, self$nline))
        },
        "[FISHERY]" = {
          rlang::expr(private$read_fishery_selectivity(inp_con, self$nline))
        },
        "[DISCARD]" = {
          rlang::expr(private$read_discard_fraction(inp_con, self$nline))
        },
        "[STOCK_WEIGHT]" = {
          rlang::expr(private$read_jan_stock_weight_age(inp_con, self$nline))
        },
        "[SSB_WEIGHT]" = {
          rlang::expr(private$read_spawning_stock_weight_age(inp_con,
                                                               self$nline))
        },
        "[MEAN_WEIGHT]" = {
          rlang::expr(private$read_mean_population_weight_age(inp_con,
                                                              self$nline))
        },
        "[CATCH_WEIGHT]" = {
          rlang::expr(private$read_landed_catch_weight_age(inp_con, self$nline))
        },
        "[DISC_WEIGHT]" = {
          rlang::expr(private$read_discard_weight_age(inp_con, self$nline))
        },
        "[HARVEST]" = {
          rlang::expr(private$read_harvest_scenario(inp_con, self$nline))
        },
        "[PSTAR]" = {
          rlang::expr(private$read_pstar_projection(inp_con, self$nline))
        },
        "[REBUILD]" = {
          rlang::expr(private$read_rebuild_projection(inp_con, self$nline))
        },
        "[OPTIONS]" = {
          rlang::expr(private$read_options_output(inp_con, self$nline))
        },
        "[PERC]" = {
          rlang::expr(private$read_percentile_summary(inp_con, self$nline))
        },
        "[BOUNDS]" = {
          rlang::expr(private$read_max_bounds(inp_con, self$nline))
        },
        "[REFPOINT]"= {
          rlang::expr(private$read_reference_points(inp_con, self$nline))
        },
        "[SCALE]" = {
          rlang::expr(private$read_scaling_factors(inp_con, self$nline))
        },
        "[RETROADJUST]" = {
          rlang::expr(private$read_retrospective_adjustments(inp_con,
                                                             self$nline))
        }

      ))

      div_keyword_line_alert <- function() {
        cli::cli_div(class = "tmp",
                     theme = list(.tmp = list(color="darkorange",
                                              "font-weight" = "bold")))
        cli::cli_alert("line {self$nline}: {inp_line}", class = "tmp")
      }
      div_keyword_line_alert()


      if (rlang::eval_tidy(!keyword_dict$has(inp_line))) {
        message(c("Input line ", self$nline,
                  " does not match AGEPRO keyword parameter"))
        invisible() #next
      }else {
        #If there is a match w/ keyword_dict then use the keyword's own
        #readLine function
        data <- list(inp_con = inp_con)
        rlang::eval_tidy(keyword_dict$get(inp_line), data)

      }

    },

    #' @description
    #' Throws a Not Implemented exception message. Placeholder function.
    #'
    #' @param keyword keyword
    not_implemented = function(keyword = "") {
      message(keyword, "Not Implemented")
    },

    #' @description
    #' Writes AGEPRO keyword parameter data as a AGEPRO input file (*.inp)
    #'
    #' @param inpfile input file path
    #' @param overwrite_as_currentver As default, overwrites version value to
    #' to the current version of the AGEPRO input file format.
    #'
    write_inp = function(inpfile, delimiter = "  ",
                         overwrite_as_currentver = TRUE) {

      if (missing(inpfile)) {

        inpfile <- save_file_dialog()
        # Exit Function if user cancels out of file dialog
        # User cancelled dialogs return NULL values
        if (is.null(inpfile)) {
          return(invisible(NULL))
        }
      }

      private$write_inpfile_version(overwrite_as_currentver)

      withCallingHandlers(
        {
          list_inp_lines <- c(
            self$ver_inpfile_string,
            self$case_id$get_inp_lines(),
            self$general$get_inp_lines(delimiter),
            self$bootstrap$get_inp_lines(delimiter),
            self$stock_weight$get_inp_lines(delimiter),
            self$ssb_weight$get_inp_lines(delimiter),
            self$mean_weight$get_inp_lines(delimiter),
            self$catch_weight$get_inp_lines(delimiter),
            if(as.logical(self$general$discards_present)){
              self$disc_weight$get_inp_lines(delimiter)
            },
            self$natmort$get_inp_lines(delimiter),
            self$maturity$get_inp_lines(delimiter),
            self$biological$get_inp_lines(delimiter),
            self$fishery$get_inp_lines(delimiter),
            if(as.logical(self$general$discards_present)){
              self$discard$get_inp_lines(delimiter)
            },
            self$recruit$get_inp_lines(delimiter),
            self$harvest$get_inp_lines(delimiter),
            if(self$projection_analyses_type == "pstar"){
              self$pstar$get_inp_lines(delimiter)
            },
            if(self$projection_analyses_type == "rebuild"){
              self$rebuild$get_inp_lines(delimiter)
            },
            self$options$get_inp_lines(delimiter),
            if(self$retroadjust$flag$op$enable_retrospective_adjustments){
              self$retroadjust$get_inp_lines(delimiter)
            },
            if(self$bounds$flag$op$enable_max_bounds){
              self$bounds$get_inp_lines(delimiter)
            },
            if(self$scale$flag$op$enable_scaling_factors){
              self$scale$get_inp_lines(delimiter)
            },
            if(self$perc$flag$op$enable_percentile_summary){
              self$perc$get_inp_lines(delimiter)
            }

          )

        },
        error = function(cond) {

        }

      )

      #Write list_inp_lines to inpfile
      sink(inpfile)
      cat(unlist(list_inp_lines), sep = "\n")
      sink()
      cli::cli_alert_info("Saved to {.file {inpfile}}")

    }


  ),
  active = list(

    #' @field nline nlines
    nline = function(val) {
      if (missing(val)) {
        return(private$.nline)
      }else {
        private$.nline <- val
      }
    },

    #' @field inp_filepath Filepath of AGEPRO input file
    inp_filepath = function(val) {
      if(isFALSE(missing(val))){
        stop("active binding is read only", call. = FALSE)
      }
      private$.inp_filepath
    }

  ),
  private = list(

    .nline = NULL,
    .inp_filepath = NULL,

    set_inp_filepath = function(value) {
      if(isFALSE(checkmate::test_file_exists(value))) {
        warning(paste0(
          "Input file '", value, "' is an invalid path or doesn't exist in ",
          "current working directory. \n",
        ))
        return()
      }
      private$.inp_filepath <- value
    },

    read_case_id = function(con, nline) {
      self$nline <- self$case_id$read_inp_lines(con, nline)
    },

    read_general_params = function(con, nline) {
      self$nline <- self$general$read_inp_lines(con, nline)
      # Set .discards_present to Input file's "discards_present" value
      private$.discards_present <- as.logical(self$general$discards_present)
    },

    read_recruit = function(con, nline) {
      # Set Recruitment's observation year sequence array using GENERAL's
      # year names from the projection time period
      cli::cli_alert(paste0("Reading {.strong recruit}: ",
                            "Recruitment for time period from ",
                            "{.val {self$general$yr_start}}"," to ",
                            "{.val {self$general$yr_end}} ..."))

      self$nline <- self$recruit$read_inp_lines(con, nline,
                                                self$general$seq_years,
                                                self$general$num_rec_models)
    },

    read_bootstrap = function(con, nline) {
      self$nline <- self$bootstrap$read_inp_lines(con, nline)
    },

    read_natural_mortality = function(con, nline) {
      self$nline <- self$natmort$read_inp_lines(con,
                                                nline,
                                                self$general$seq_years,
                                                self$general$num_ages)
    },

    read_maturity_fraction = function(con, nline) {
      self$nline <- self$maturity$read_inp_lines(con,
                                                 nline,
                                                 self$general$seq_years,
                                                 self$general$num_ages)
    },

    read_mortality_fraction_prior_spawn = function(con, nline){
      self$nline <- self$biological$read_inp_lines(con,
                                                   nline,
                                                   self$general$seq_years)
    },

    read_fishery_selectivity = function(con, nline) {
      self$nline <-
        self$fishery$read_inp_lines(con,
                                    nline,
                                    self$general$seq_years,
                                    self$general$num_ages,
                                    self$general$num_fleets)
    },

    read_discard_fraction = function(con, nline) {

      if(!(as.logical(self$general$discards_present))){
        stop(paste0("Reading Discard Fraction data but ",
                    "'Discards are present' option is FALSE"))
      }
      self$nline <- self$discard$read_inp_lines(con,
                                                nline,
                                                self$general$seq_years,
                                                self$general$num_ages,
                                                self$general$num_fleets)

    },

    read_jan_stock_weight_age = function(con, nline) {
      self$nline <- self$stock_weight$read_inp_lines(con,
                                                     nline,
                                                     self$general$seq_years,
                                                     self$general$num_ages)
    },

    read_spawning_stock_weight_age = function(con, nline) {
      self$nline <- self$ssb_weight$read_inp_lines(con,
                                                   nline,
                                                   self$general$seq_years,
                                                   self$general$num_ages)
    },

    read_mean_population_weight_age = function(con, nline) {
      self$nline <- self$mean_weight$read_inp_lines(con,
                                                    nline,
                                                    self$general$seq_years,
                                                    self$general$num_ages)
    },

    read_landed_catch_weight_age = function(con, nline) {
      self$nline <- self$catch_weight$read_inp_lines(con,
                                                     nline,
                                                     self$general$seq_years,
                                                     self$general$num_ages,
                                                     self$general$num_fleets)
    },

    read_discard_weight_age = function(con, nline) {

      if(!as.logical(self$general$discards_present)){
        stop(paste0("Reading Discard Fraction data but ",
                    "'Discards are present' option is FALSE"))
      }
      self$nline <- self$disc_weight$read_inp_lines(con,
                                                    nline,
                                                    self$general$seq_years,
                                                    self$general$num_ages,
                                                    self$general$num_fleets)

    },

    read_harvest_scenario = function(con,nline) {

      self$nline <- self$harvest$read_inp_lines(con,
                                                nline,
                                                self$general$seq_years,
                                                self$general$num_fleets)

    },

    read_pstar_projection = function(con, nline) {

      if(self$projection_analyses_type == "rebuild"){
        stop(paste0("Read REBUILD projection data prevents ",
                    "PSTAR projections to be read from file. ",
                    "(current projection_analyses_type is 'rebuild')"),
             call. = FALSE)
      }

      self$setup_projection_analyses_values("pstar")

      self$nline <- self$pstar$read_inp_lines(con, nline)
    },

    read_rebuild_projection = function(con, nline) {

      if(self$projection_analyses_type == "pstar"){
        stop(paste0("Read PSTAR projection data prevents ",
                    "REBUILD projection data to be read from file. ",
                    "(current projection_analyses_type is 'pstar')"),
             call. = FALSE)
      }

      self$setup_projection_analyses_values("rebuild")

      self$nline <- self$rebuild$read_inp_lines(con, nline)
    },

    read_options_output = function(con, nline) {

      # Is Input File Version String matches agepro40 Input File Format string:
      # "AGEPRO VERSION 4.0"
      self$options$enable_agepro40_format <-
        private$check_agepro40_inpfile_string(self$ver_inpfile_string)

      self$nline <- self$options$read_inp_lines(con, nline)
    },

    read_percentile_summary = function(con, nline) {

      self$perc$enable_percentile_summary <- TRUE
      self$nline <- self$perc$read_inp_lines(con, nline)
    },

    read_max_bounds = function(con, nline) {
      self$bounds$enable_max_bounds <- TRUE
      self$nline <- self$bounds$read_inp_lines(con, nline)
    },

    read_reference_points = function(con, nline) {
      self$refpoint$enable_reference_points <- TRUE
      self$nline <- self$refpoint$read_inp_lines(con, nline)
    },

    read_scaling_factors = function(con, nline) {
      self$scale$enable_scaling_factors <- TRUE
      self$nline <- self$scale$read_inp_lines(con, nline)
    },
    read_retrospective_adjustments = function(con, nline) {
      self$retroadjust$enable_retrospective_adjustments <- TRUE
      self$nline <- self$retroadjust$read_inp_lines(con,
                                                    nline,
                                                    self$general$num_ages)
    },



    # Helper function to validate AGEPRO Input File Version format is
    # supported_inpfile_version
    check_inpfile_version = function(inp_line,
                                     check_currentver = TRUE) {

      checkmate::assert_character(inp_line, len = 1)

      # Throw Error if VERSION string doesn't match supported
      # AGEPRO Input File Version string formats
      if(isFALSE(inp_line %in% self$supported_inpfile_versions)){
        stop(paste0(
          "This version of this input file is not supported: ",inp_line,
          "\n - Supported verion(s): ",
          paste(self$supported_inpfile_versions,collapse=", ")),
          call.= FALSE)

      }

      if(check_currentver) {
        return(invisible(private$check_currentver_inpfile_string(inp_line)))
      }
      return(TRUE)

    },


    # Helper function to check if the string matches AGEPRO VERSION 4.0
    # input file version format
    check_agepro40_inpfile_string = function(inp_line) {

      return(identical(inp_line, private$.agepro40_inpfile_string))
    },

    # Throw Warning if (supported) VERSION string doesn't match
    # "current version" AGEPRO Input file format
    check_currentver_inpfile_string = function (inp_line, line1_msg = TRUE) {

      # In case this is used outside checking/reading line1 of input files
      msg_wasfound <- ifelse(line1_msg,
                    " (Line 1 read from input file) found,",
                    " was found,")

      if(isFALSE(identical(inp_line,
                           private$.currentver_inpfile_string))){
        warning(paste0(inp_line,
                       msg_wasfound,
                       " but does not match current version of the AGEPRO",
                       " Input File format: ",
                       private$.currentver_inpfile_string),
                call. = FALSE)

        return(FALSE)
      }
      return(TRUE)
    },


    # Set Input File String based on preference on current AGEPRO input file
    # version. Warn for agepro model's version string doesn't match current
    # version
    write_inpfile_version = function(overwrite_as_currentver = TRUE){

      # Check agepro_model VERSION string matches "current version"
      # of input file version format
      is_model_currentver <- identical(self$ver_inpfile_string,
                                     private$.currentver_inpfile_string)

      if(isFALSE(is_model_currentver)) {


        cli::cli_alert_warning(paste0(
          "Model's input file format ",
          "{.strong {self$ver_inpfile_string}} does not match ",
          "{.emph current version} ",
          "{.strong {private$.currentver_inpfile_string}} "))


        if(isFALSE(overwrite_as_currentver)){
          #Return/Escape function and send warning
          warning(paste0("AGEPRO input file version does not match",
                         "current input file version ",
                         private$.currentver_inpfile_string,".")    )
          return()
        }

        #Otherwise, set model's input file string to "current version"
        cli::cli_alert_info(paste0(
          "Overwiting model's input file format (ver_inpfile_string) to ",
          "{.emph current version} ",
          " {.val {private$.currentver_inpfile_string}}"))
        self$ver_inpfile_string <- private$.currentver_inpfile_string

      }

      cli::cli_alert_info(paste0("Input file format (ver_inpfile_string): ",
                                   "{.val {self$ver_inpfile_string}}"))

      return()

    }


  )
)

#' @title
#' AGEPRO model w/ JSON input file bindings
#'
#' @description
#' File Functionality on experimental JSON input file
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom checkmate assert_number
#' @importFrom jsonlite toJSON
#' @importFrom utils browseURL
agepro_json_model <- R6Class(
  "agepro_json_model",
  inherit = agepro_model,
  public = list(

    #' @description
    #' Initializes the instances of the AGEPRO Model
    #'
    #' @param ... Parameters to initialize the parent
    #' [`agepro_model`][ageproR::agepro_model] class
    #'
    initialize = function(...) {
      super$initialize(...)
    },


    #' @description
    #' Return a json formatted object.
    #'
    #' @details
    #' See [jsonlite::toJSON] for more details.`NA` values in a list or
    #' a multi-length vector will converted to JSON style NULL value, but
    #' wrapped in a JSON array (`[null, null]`). Using the defaults in
    #' [jsonlite::fromJSON], the JSON null array can be is converted back to
    #' `NA`. Single `NA` values will be reconverted to `NULL`.
    #'
    get_json = function() {

      version_json <- list(

        inpfile_string = self$ver_inpfile_string,
        jsonfile_format = self$ver_jsonfile_format
      )

      agepro_json <- list(
        "version" = version_json,
        "case_id" = self$case_id$model_name,
        "general" = self$general$json_list_object,
        "bootstrap" = self$bootstrap$json_list_object,
        "natmort" = self$natmort$json_list_object,
        "maturity" = self$maturity$json_list_object,
        "biological" = self$biological$json_list_object,
        "fishery" = self$fishery$json_list_object,
        "discard" =
            ifelse(!is.null(self$discard),
                  self$discard$json_list_object,
                  NA),
        "stock_weight" = self$stock_weight$json_list_object,
        "ssb_weight" = self$ssb_weight$json_list_object,
        "mean_weight" = self$mean_weight$json_list_object,
        "catch_weight" = self$catch_weight$json_list_object,
        "disc_weight" =
            ifelse(!is.null(self$disc_weight),
                  self$disc_weight$json_list_object,
                  NA),
        "recruit" = self$recruit$json_list_object,
        "harvest" = self$harvest$json_list_object,
        "pstar" = self$pstar$json_list_object,
        "rebuild" = self$rebuild$json_list_object,
        "options" = self$options$json_list_object,
        "perc" = {
          if(self$perc$enable_percentile_summary){
            self$perc$json_list_object
          }else{
            NA
          }
        },
        "bounds" = {
          if(self$bounds$enable_max_bounds){
            self$bounds$json_list_object
          }else{
            NA
          }
        },
        "retroadjust" = {
          if(self$retroadjust$enable_retrospective_adjustments){
            self$retroadjust$json_list_object
           }else{
             NA
           }
         },
         "refpoint" = {
           if(self$refpoint$enable_reference_points){
             self$refpoint$json_list_object
           }else{
             NA
           }
         },
         "scale" = {
           if(self$scale$enable_scaling_factors){
             self$scale$json_list_object
           }else{
             NA
           }
         }
      )



      toJSON(agepro_json,
             pretty = TRUE,
             auto_unbox = TRUE)

    },

    #' @description
    #' Write JSON file
    #'
    #' @param file input file path
    #' @param show_dir Option to show directory after JSON file is written.
    #'
    write_json = function(file, show_dir = FALSE) {

      if (missing(file)) {

        file <- save_file_dialog()
        # Exit Function if user cancels out of file dialog
        # User cancelled dialogs return NULL values
        if (is.null(file)) {
          return(invisible(NULL))
        }
      }


      write(self$get_json(), file)

      message("JSON input file saved at:\n", file)
      if (show_dir) {
        browseURL(dirname(file))
      }
    },

    #' @description
    #' Reads AGEPRO json experimental input file format.
    #'
    #' @param file input file path
    #'
    read_json = function(file) {
      warning("AGEPRO JSON input is in development, and format may change.")
      return(jsonlite::read_json(file, simplifyVector = TRUE))
    },

    #' @description
    #' Imports AGEPRO model data formatted for AGEPRO input files
    #' (`agepro_inp_model`).
    #'
    #' @param inp_model AGEPRO model with AGEPRO Input File (*.INP) functions
    import_agepro_inp_model = function(inp_model){

      #Validate agepro_inp_model
      checkmate::assert_r6(inp_model,
                           classes = c("agepro_inp_model","agepro_model"),
                           public = c("case_id",
                                      "general",
                                      "bootstrap",
                                      "natmort",
                                      "maturity",
                                      "biological",
                                      "fishery",
                                      "discard",
                                      "stock_weight",
                                      "ssb_weight",
                                      "mean_weight",
                                      "catch_weight",
                                      "disc_weight",
                                      "recruit",
                                      "harvest",
                                      "pstar",
                                      "options",
                                      "refpoint",
                                      "bounds",
                                      "retroadjust",
                                      "refpoint",
                                      "scale"))

      cli::cli_alert_info("Importing from agepro_inp_model ...")
      self$projection_analyses_type <-
        inp_model$projection_analyses_type

      tryCatch(
        {
          if(as.logical(inp_model$general$discards_present)){
            cli::cli_alert("Discard and Discard Weights ...")
            self$discard <- inp_mode$discard
            self$disc_weight <- inp_model$disc_weight
          }else {
            self$discard <- NULL
            self$disc_weight <- NULL
          }

          self$case_id <- inp_model$case_id
          cli::cli_alert_success("Case ID")

          self$general <- inp_model$general
          cli::cli_alert_success("General AGEPRO Model options")

          self$bootstrap <- inp_model$bootstrap
          cli::cli_alert_success("Bootstrap")

          self$natmort <- inp_model$natmort
          self$maturity <- inp_model$maturity
          self$biological <- inp_model$biological
          self$fishery <- inp_model$fishery
          self$stock_weight <- inp_model$stock_weight
          self$ssb_weight <- inp_model$ssb_weight
          self$mean_weight <- inp_model$mean_weight
          self$catch_weight <- inp_model$catch_weight
          cli::cli_alert_success("Process Error")

          self$recruit <- inp_model$recruit
          cli::cli_alert_success("Recruitment")

          self$harvest <- inp_model$harvest
          cli::cli_alert_success("Harvest Scenario")
          if(self$projection_analyses_type == "pstar"){

            self$setup_projection_analyses_values("pstar")
            cli::cli_alert_info(paste0("Importing PStar Projection values ",
                                       "from AGEPRO Input Data format ..."))
            self$pstar <- inp_model$pstar

          }
          if(self$projection_analyses_type == "rebuild"){

            self$setup_projection_analyses_values("rebuild")
            cli::cli_alert_info(paste0("Importing Rebuling Projection values ",
                                       "from AGEPRO Input Data format ..."))
            self$rebuild <- inp_model$rebuild

          }

          self$options <- inp_model$options
          self$perc <- inp_model$perc
          self$bounds <- inp_model$bounds
          self$retroadjust <- inp_model$retroadjust
          self$refpoint <- inp_model$refpoint
          self$scale <- inp_model$scale
          cli::cli_alert_success("AGEPRO model and output options")

        },
        error = function(err){
          message(conditionMessage(err))

        },
        finally ={
          cli::cli_alert("Done")
        }
      )



      invisible(inp_model)

    }

  )
)
