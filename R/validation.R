

#' Recruitment Model Number Parameter validation
#'
#' @description
#' Custom validation to check
#' \href{../../ageproR/html/agepro_model.html#method-set_recruit_model}{\code{agepro_model$set_recruit_model()}}
#' arguments to see if multiple recruit numbers is passed as a single vector
#' or seen as a list of multiple arguments.
#'
#' If the input value is passed as a list of multiple arguments,
#' this function will "throw" a message of the issue and possible resolution.
#'
#' [ageproR::assert_model_num_vector_format] wraps
#' [ageproR::check_model_num_vector_format] as a custom checkmate assertion via [checkmate::makeAssertion]
#'
#' @param x object to check
#'
check_model_num_vector_format <- function(x) {

  # Catch "Empty" argument
  if(isTRUE(all.equal(length(x), 0))){
    return(paste0("No recruitment model numbers passed"))
  }

  # Catch Multiple parameters and return validation message
  if(!isTRUE(all.equal(length(x),1))){
    return(paste0("Multiple parameters detected, ",
                  "please pass multiple recruitment models as a single vector"))
  }

  return(TRUE)

}


#' @rdname check_model_num_vector_format
#'
#' @template assert
assert_model_num_vector_format <- function(x,
                                      .var.name = checkmate::vname(x),
                                      add = NULL) {

  res = check_model_num_vector_format(x)
  checkmate::makeAssertion(x, res, .var.name, add)

}


#' Recruitment model number vector count validation
#'
#' @description
#' Checks if input model number matches the number of recruitment models of
#' the model.
#'
#' @param x
#' Object to check
#'
#' @param num_recruit_models
#' Number of recruitment models AGEPRO model at initialization
#'
check_model_num_vector_count <- function(x, num_recruit_models){

  #Throw Error if vector length doesn't match num_recruit_models
  if(!isTRUE(all.equal(length(x), num_recruit_models))){
    return(paste0("Recruitment Model vector (model_num) object count ",
                "does not match number of recruits. ",
                "(count: ", length(x), ", number of recruits: ",
                num_recruit_models, ")"))
  }

  return(TRUE)

}

#' @rdname check_model_num_vector_count
#'
#' @template assert
#'
assert_model_num_vector_count <- function(x, num_recruit_models,
                                          .var.name = checkmate::vname(x),
                                          add = NULL) {

  res = check_model_num_vector_count(x, num_recruit_models)
  checkmate::makeAssertion(x, res, .var.name, add)
}


#' Projection year sequence vector validation
#'
#' @description
#' Custom validation to check projection_years sequence vector is
#' properly incremented by 1, Can be Wrapped as a custom checkmate assertion.
#'
#' @param x
#' Object to check
#'
check_proj_years_sequence <- function(x) {
  if(isFALSE(all(diff(x) %in% 1))) {
    paste0("Invalid projection_years Sequence: ",
           "Sequence does not increment by 1 or ",
           "not a valid interaction (no colon) of two numeric ",
           "elements.")
  }else {
    TRUE
  }
}

#' @rdname check_proj_years_sequence
#'
#' @template assert
#'
assert_proj_years_sequence <- function(x, .var.name = checkmate::vname(x),
                                   add = NULL) {
  res = check_proj_years_sequence(x)
  checkmate::makeAssertion(x, res, .var.name, add)
}


#' Validation for PERC active binding in agepro_model
#'
#' @description
#' Custom validation procedure to check if input value matches the structure
#' of the percentile_summary R6Class. It will also catch single numeric
#' input values presuming that the active binder sets the report_percentile.
#'
#' @param x
#' Object to Check
#'
check_perc_active_binding <- function(x) {

  if(checkmate::test_numeric(x, len = 1)) {
    return(paste0(
      "Input value found as a numeric, not a percentile_summary class. ",
      "Did you mean to set report_percentile field?"
    ))
  }

  perc_fields <- c("report_percentile")

  return(checkmate::check_r6(x, public = perc_fields) )

}


#' @rdname check_perc_active_binding
#'
#' @template assert
#'
assert_perc_active_binding <- function(x, .var.name = checkmate::vname(x),
                                       add = NULL) {
  res = check_perc_active_binding(x)
  checkmate::makeAssertion(x, res, .var.name, add)

}


#' Validation for CASEID active binding in agepro_model
#'
#' @description
#' Custom validation procedure to check if input value matches the structure
#' of the case_id R6Class. It will also catch single character strings
#' presuming that the input value was intended to set the model_name field.
#'
#' @param x
#' Object to Check
#'
check_case_id_active_binding <- function(x) {

  if(checkmate::test_string(x)) {
    return(paste0(
      "Input value found as a string, not a case_id class. ",
      "Did you mean to set model_name field?"
    ))
  }

  return(checkmate::check_r6(x, public = "model_name") )

}


#' @rdname check_case_id_active_binding
#'
#' @template assert
#'
assert_case_id_active_binding <- function(x, .var.name = checkmate::vname(x),
                                       add = NULL) {
  res = check_case_id_active_binding(x)
  checkmate::makeAssertion(x, res, .var.name, add)

}



#' Validation for BOUNDS active binding in agepro_model
#'
#' @description
#' Custom validation procedure to check if input value has the public
#' methods and active bindings fields of the max_bounds R6Class.
#'
#' @param x
#' Object to Check
#'
check_bounds_active_binding <- function(x) {

  bounds_fields <- c("max_weight",
                     "max_natural_mortality")
  return(checkmate::check_r6(x, public = bounds_fields) )
}

#' @rdname check_bounds_active_binding
#'
#' @template assert
#'
assert_bounds_active_binding <- function(x, .var.name = checkmate::vname(x),
                                       add = NULL) {
  res = check_bounds_active_binding(x)
  checkmate::makeAssertion(x, res, .var.name, add)

}



#' @title
#' Custom mapping function for error handing
#'
#' @description
#' Custom mapping function used for error handling. This is based on the
#' rlang topic errors guide.
#'
#' @template elipses
#'
#' @param .xs List r Atomic Vector
#' @param .fn Function
#'
#' @export
#'
validate_map = function(.xs, .fn, ...) {

  # Capture the defused code supplied as `.fn`
  fn_code <- substitute(.fn)

  out <- rlang::new_list(length(.xs))

  for(i in seq_along(.xs)){
    rlang::try_fetch(
      out[[i]] <- .fn(.xs[[i]], ...),
      error = function(cnd) {
        # Inspect the 'call' field to detect `.fn` calls
        if(rlang::is_call(cnd$call, ".fn")) {
          # Replace ".fn" by the defused code
          # and Keep existing Arguemnts
          cnd$call[[1]] <- fn_code
        }
        rlang::abort(
          sprintf("Problem while mapping around element %d ", i),
          parent = cnd
        )
      }
    )
  }
  out
}

#' @title
#' Validates the usage of the 'projection years' parameter.
#'
#' @description
#' If `proj_years` parameter is a
#' [projection_years class][ageproR::projection_years], then it will return
#' that value. Otherwise, it will create a new `projection_years` class based
#' on the param value passed.
#'
#' @param proj_years Projection year parameter. May be a numeric vector or a
#' [`projection_years`][ageproR::projection_years]
#'
validate_proj_years_parameter <- function (proj_years) {

  #Validate parameters
  if (checkmate::test_r6(proj_years, public = c("count","sequence") )) {
    proj_years_class <- proj_years
  } else {
    proj_years_class <- ageproR::projection_years$new(proj_years)
  }

  return(proj_years_class)
}



#' Checks the validity of filetype key-value pair.
#'
#' Checks the filetype as a 2 length vector without missing values. If filetype
#' as not passed in the parameter, it will return the default `All files (*.*)`
#' string vector.
#'
#' @details
#' The filetype key-value pair is defined as _fileTypeName_ _extension_. This
#' is used to [specifying flie
#' patterns](https://www.tcl.tk/man/tcl8.0/TkCmd/getOpenFile.html#M11) for
#' Tcl/TK file modules.
#'
#' @param filetype filename extension.
#'
#' @keywords internal
#'
validate_filetype <- function(filetype) {

  if (missing(filetype)) {
    #Default "All Files (*)" file type
    filetype <- c("All Files", "*")
  }

  #Validate filetype string
  checkmate::assert_vector(filetype, all.missing = FALSE, len = 2,
                           null.ok = FALSE)
  return(filetype)

}



