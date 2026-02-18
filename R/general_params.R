
#' @title Input general model parameters
#'
#' @description Stores overall AGERPRO model project parameters including
#' the projection time horizon, the target model's age class range, the
#' quantity of recruitment models used, and fishery processes that affect
#' projections.
#'
#' @template elipses
#' @template inp_line
#' @template inp_con
#' @template nline
#' @template delimiter
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom checkmate test_logical assert_number
general_params <- R6Class(
  classname = "general_params",
  public = list(

    #' @description
    #' Starts an instances of the AGEPRO Model
    #'
    #' @param yr_start First Year of Projection
    #' @param yr_end Last Year of Projection
    #' @param age_begin age begin
    #' @param age_end age end
    #' @param num_pop_sims Number of population sims
    #' @param num_fleets Number of fleets
    #' @param num_rec_models Number of Recruit Modeles
    #' @param discards_present discards_present
    #' @param seed Random Number seed
    #' @param enable_cat_print
    #' Logical flag to show target function's messages on console.
    #' By Default, set to TRUE.
    #'
    initialize = function(yr_start = 0,
                          yr_end = 2,
                          age_begin = 1,
                          age_end = 6,
                          num_pop_sims = 1000,
                          num_fleets = 1,
                          num_rec_models = 1,
                          discards_present = FALSE,
                          seed = sample.int(1e8, 1),
                          enable_cat_print = TRUE) {


      if(enable_cat_print){
        div_keyword_header(self$keyword_name)
      }
      # Discards: Assert numeric format
      if (test_logical(discards_present)) {
        discards_present <- as.numeric(discards_present)
      }

      private$set_yr_start(as.numeric(yr_start))
      private$set_yr_end(as.numeric(yr_end))
      private$set_age_begin(as.numeric(age_begin))
      private$set_age_end(as.numeric(age_end))
      private$set_num_pop_sims(as.numeric(num_pop_sims))
      private$set_num_fleets(as.numeric(num_fleets))
      private$set_num_rec_models(as.numeric(num_rec_models))
      private$set_discards_present(discards_present)
      private$set_seed(as.numeric(seed))

      if(enable_cat_print){
        self$print()
      }

    },

    #' @description
    #' Prints out General Parameters
    #'
    print = function(...) {

      cli::cli_alert_info(
        paste0("yr_start {.emph (First Year in Projection)}: ",
               "{.val {self$yr_start}}"))
      cli::cli_alert_info(
        paste0("yr_end {.emph (Last Year in Projection)}: ",
               "{.val {self$yr_end}}"))
      cli::cli_alert_info(
        paste0("age_begin {.emph (First Age Class)}: ",
               "{.val {self$age_begin}}"))
      cli::cli_alert_info(
        paste0("age_end {.emph (Last Age Class)}: ",
               "{.val {self$age_end}}"))
      cli::cli_alert_info(
        paste0("num_pop_sims {.emph (Number of Population Simulations)}: ",
               "{.val {self$num_pop_sims}}"))
      cli::cli_alert_info(
        paste0("num_fleets {.emph (Number of Fleets)}: ",
               "{.val {self$num_fleets}}"))
      cli::cli_alert_info(
        paste0("num_rec_models {.emph (Number of Recruitment Model(s))}: ",
               "{.val {self$num_rec_models}}"))
      cli::cli_alert_info(
        paste0("discards_present {.emph (Discards are present)}: ",
               "{.val {as.logical(self$discards_present)}} "))
      cli::cli_alert_info(
        paste0("seed {.emph (Calculation Engine Random Number Seed)}: ",
               "{.val {self$seed}}"))
      invisible(self)
    },

    #' @description
    #' Reads General AGEPRO parameters from AGEPRO INP Input File
    read_inp_lines = function(inp_con, nline) {

      # Read an additional line from the file connection and split the string
      # into substrings by whitespace
      nine <- nline + 1
      cli_alert("Line {nline} : Reading AGEPRO model GENERAL options ...")

      inp_line <- read_inp_numeric_line(inp_con)

      private$set_yr_start(inp_line[1])
      private$set_yr_end(inp_line[2])
      private$set_age_begin(inp_line[3])
      private$set_age_end(inp_line[4])
      private$set_num_pop_sims(inp_line[5])
      private$set_num_fleets(inp_line[6])
      private$set_num_rec_models(inp_line[7])
      private$set_discards_present(inp_line[8])
      private$set_seed(inp_line[9])

      div_general_fields <-
        cli::cli_div(class = "input_field",
                     theme = list(.input_field = list("margin-left" = 2)))
      self$print()

      cli::cli_end(div_general_fields)

      return(nline)
    },

    #' @description
    #' Returns the values for the GENERAL keyword parameter formatted
    #' to the AGEPRO input file format.
    #'
    get_inp_lines = function(delimiter = "  ") {
      return(list(
        self$inp_keyword,
        paste(self$yr_start,
              self$yr_end,
              self$age_begin,
              self$age_end,
              self$num_pop_sims,
              self$num_fleets,
              self$num_rec_models,
              as.numeric(self$discards_present),
              self$seed,
              sep = delimiter)
      ))
    }

  ),
  active = list(

    #' @field yr_start
    #' First Year in Projection
    yr_start = function(value) {
      if(isFALSE(missing(value))){
        stop("active binding is read only", call. = FALSE)
      }
      private$.yr_start
    },

    #' @field yr_end
    #' Last Year in Projection
    yr_end = function(value) {
      if(isFALSE(missing(value))){
        stop("active binding is read only", call. = FALSE)
      }
      private$.yr_end
    },

    #' @field age_begin
    #' First Age Class
    age_begin = function(value){
      if(isFALSE(missing(value))){
        stop("active binding is read only", call. = FALSE)
      }
      private$.age_begin
    },

    #' @field age_end
    #' Last Age Class
    age_end = function(value){
      if(isFALSE(missing(value))){
        stop("active binding is read only", call. = FALSE)
      }
      private$.age_end
    },

    #' @field num_pop_sims
    #' Number of Population Simulations
    num_pop_sims = function(value){
      if(isFALSE(missing(value))){
        stop("active binding is read only", call. = FALSE)
      }
      private$.num_pop_sims
    },

    #' @field num_fleets
    #' Number of Fleets
    num_fleets = function(value){
      if(isFALSE(missing(value))){
        stop("active binding is read only", call. = FALSE)
      }
      private$.num_fleets
    },

    #' @field num_rec_models
    #' Number of Recruitment Models
    num_rec_models = function(value) {
      if(isFALSE(missing(value))){
        stop("active binding is read only", call. = FALSE)
      }
      private$.num_rec_models
    },

    #' @field discards_present
    #' Are discards present?
    discards_present = function(value) {
      if(isFALSE(missing(value))){
        stop("active binding is read only", call. = FALSE)
      }
      private$.discards_present
    },

    #' @field seed
    #' Pseudo Random Number seed
    seed = function(value){
      if(isFALSE(missing(value))){
        stop("active binding is read only", call. = FALSE)
      }
      private$.seed
    },


    #' @field num_years Determines the number of years in projection by the
    #' (absolute) difference of the last and first year of projection.
    num_years = function() {
      abs(self$yr_end - self$yr_start) + 1
    },

    #' @field num_ages Determines number of ages by the (absolute) difference
    #' of the first and last age class.
    num_ages = function() {
      abs(self$age_begin - self$age_end) + 1
    },

    #' @field seq_years Returns a sequence of years from First year of
    #' projection
    seq_years = function() {
      seq(self$yr_start, self$yr_end)
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
    },

    #' @field json_list_object
    #' List of GENERAL keyword fields values, exportable to JSON.
    json_list_object = function() {

      #If discard Flag is numeric check if it is 0 or 1
      if (!test_logical(self$discards_present)) {
        #Assert for 0 and 1
        assert_number(self$discards_present, lower = 0, upper = 1)
      }

      return(list(
        nFYear = self$yr_start,
        nXYear = self$yr_end,
        nFAge = self$age_begin,
        nXAge = self$age_end,
        nSims = self$num_pop_sims,
        nFleet = self$num_fleets,
        nRecModel = self$num_rec_models,
        discFlag = as.numeric(self$discards_present),
        seed = self$seed
      ))
    }

  ),
  private = list(

    .yr_start = NULL,
    .yr_end = NULL,
    .age_begin = NULL,
    .age_end = NULL,
    .num_pop_sims = NULL,
    .num_fleets = NULL,
    .num_rec_models = NULL,
    .discards_present = NULL,
    .seed = NULL,

    .keyword_name = "general",

    ## Private setter functions w/ validation

    #yr_start
    set_yr_start = function (value) {
      checkmate::assert_numeric(value, lower = 0, len = 1,
                                .var.name = "yr_start")
      private$.yr_start <- value
    },

    #yr_end
    set_yr_end = function(value){
      checkmate::assert_numeric(value, len = 1,
                                lower = self$yr_start + 1,
                                .var.name = "yr_end")
      private$.yr_end <- value
    },

    #age_begin
    set_age_begin = function(value){
      checkmate::assert_choice(value, choices = c(0, 1),
                               .var.name = "age_begin")
      private$.age_begin <- value
    },

    #age_end
    set_age_end = function(value){
      checkmate::assert_numeric(value, len = 1,
                                lower = self$age_begin + 1,
                                .var.name = "age_end")
      private$.age_end <- value
    },

    #num_pop_sims
    set_num_pop_sims = function(value){
      checkmate::assert_numeric(value, lower = 0, len = 1,
                                .var.name = "num_pop_sims")
      private$.num_pop_sims <- value
    },

    #num_fleets
    set_num_fleets = function(value){
      checkmate::assert_numeric(value, lower = 1, len = 1,
                                .var.name = "num_fleets")
      private$.num_fleets <- value
    },

    #num_rec_models
    set_num_rec_models = function(value){
      checkmate::assert_numeric(value, lower = 1, len = 1,
                                .var.name = "num_rec_models")
      private$.num_rec_models <- value
    },

    #discards_present
    set_discards_present = function(value){
      #set discard_present values as int/numeric. 0=FALSE 1=TRUE
      checkmate::assert_choice(value, choices = c(0, 1),
                               .var.name = "discards_present")
      private$.discards_present <- value
    },

    #seed
    set_seed = function(value){
      checkmate::assert_numeric(value, len = 1,
                                .var.name = "seed")
      private$assert_nonzero_seed(value)
      private$.seed <- value
    },

    #check random number seed is non-zero
    check_nonzero_seed = function(value) {
      if(isTRUE(all.equal(value,0))) {
        return(paste0("Random Seed value must be a non-zero"))
      }
      return(TRUE)
    },

    #Checkmate assertion wrapper for check_nonzero_seed
    assert_nonzero_seed = function(x, .var.name = checkmate::vname(x),
                                   add = NULL){
      result = private$check_nonzero_seed(x)
      checkmate::makeAssertion(x, result, .var.name, add)
    }

  )


)
