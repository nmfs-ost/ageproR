

#' Command-line interface header for AGEPRO Keyword parameters
#'
#' Creates an custom header with double-lines (colored in cyan by default), via
#' cli library.
#'
#' @param keyword Text of the header
#' @param header_color Text Color, R color, or HTML hexidecimal color.
#'
#' @keywords internal
#'
div_keyword_header <- function(keyword, header_color = "cyan") {
  d <- cli_div(theme = list(rule = list(
    color = header_color,
    "line-type" = "double")))
  cli_rule(keyword)
  cli_end(d)
}


#' Use cli commands to display AGEPRO table-like values
#'
#' Helper function to print out AGEPRO keyword parameter's table-like matrix or
#' vector variables to console. Includes an option to show the first few rows
#' to the console.
#'
#' @param tbl AGEPRO Keyword Parameter matrix/vector variable
#' @param omit_rows Logical flag, if `TRUE`, will print the first six rows,
#' via [`head()`][utils::head], to R console. In addition, the total number
#' of rows and rows omitted will be displayed. If `FALSE`, by default,
#' the matrix or vector prints normally.
#'
#' @importFrom utils head
#' @export
#'
print_parameter_table = function (tbl, omit_rows=FALSE) {

  if(omit_rows) {

    omitted_num_rows <- pmax(0, nrow(tbl)-6)

    capture_output_as_message(cli::cat_print(head(tbl))) #first 6 rows
    cli::cli_text(
      paste0("{symbol$info} ","Total of {nrow(tbl)} row{?s}; ",
             "{no(omitted_num_rows)} row{?s} omitted"))
  }else{
    capture_output_as_message(cli::cat_print(tbl))
  }

}


#' Creates a table-like matrix with `NA` values
#'
#' Wrapper to [`Matrix`][base::matrix] function, that returns the object with
#' `NA` values. Uses the matrix dimnames argument to set row and column names.
#' See [`Matrix`][base::matrix] for more information.
#'
#' @param num_rows the desired number of rows
#' @param num_cols the desired number of columns
#' @param dimnames Matrix `dimnames`. See [`Matrix`][base::matrix] argument
#' for more detail.
#'
create_blank_parameter_table = function(num_rows, num_cols,
                                        dimnames = NULL) {

  return(matrix(rep(NA, (num_rows * num_cols) ) ,
                nrow = num_rows,
                ncol = num_cols,
                dimnames = dimnames))

}

#' Invalid Path Message
#'
#' Returns a reusable invalid Path Message
#'
#' @param x Filepath string
#'
invalid_path_message <- function(x) {
  paste0("'", x, "' is an invalid path or doesn't exist in ",
         "working directory. \n")
}


#' converts output as messages
#'
#' Helper function to format vector output, such as data.frames, matrices. or
#' lists to message format.
#'
#' @param x Vector output object
#'
#' @importFrom utils capture.output
#'
capture_output_as_message <- function(x) {

  paste(capture.output(x), collapse = "\n") |> message()

}

#' Import existing bootstrap file to create a new one that matches the
#' dimensions for a AGEPRO model.
#'
#' Note: This function is intended to test Bootstrap compatability with the
#' Agepro Input File model and is not intended for production use.
#'
#' @param bsn_file Input Bootstrap Filename
#' @param out_file Output Formatted Bootstrap Filename
#' @param target_ages Target Number of Ages (Columns) of formatted output
#' bootstrap file
#' @param target_runs Target Number of Bootstrap Runs (Rows) of formatted output
#' bootstrap file
#'
#' @importFrom utils read.table write.table
#'
#' @export
#'
write_bsn_from_existing_file <- function(bsn_file, out_file,
                                         target_ages, target_runs = 1000) {

  # Read the Example Bootstrap file
  # Assuming it has no headers and is space-delimited
  bsnfile_data <- read.table(bsn_file, header = FALSE)

  # Check current dimensions
  current_runs <- nrow(bsnfile_data)
  current_ages <- ncol(bsnfile_data)
  print(paste("Original File - Number of Runs:", current_runs, "Ages:", current_ages))

  # Adjust the Number of Ages (Columns)
  if (current_ages < target_ages) {
    # Add columns of zeros (or a tiny number like 0.001) for the missing older ages
    missing_ages <- target_ages - current_ages
    padding <- matrix(0.001, nrow = current_ages, ncol = missing_ages)
    bsnfile_data <- cbind(bsnfile_data, padding)

  } else if (current_ages > target_ages) {
    # Cut off the extra older ages
    bsnfile_data <- bsnfile_data[, 1:target_ages]
  }

  # Adjust the Number of Iterations (Rows)
  if (current_runs < target_runs) {
    # Randomly duplicate existing rows to reach the target number
    rows_to_add <- target_runs - current_runs
    sampled_rows <- bsnfile_data[sample(nrow(bsnfile_data), rows_to_add, replace = TRUE), ]
    new_data <- rbind(bsnfile_data, sampled_rows)

  } else if (current_runs > target_runs) {
    # Keep only the first 'n' iterations
    new_data <- bsnfile_data[1:target_runs, ]

  } else {
    new_data <- bsnfile_data # No change needed
  }

  # Write the newly sized file for AGEPRO
  write.table(new_data,
              file = out_file,
              row.names = FALSE,
              col.names = FALSE,
              sep = " ")

  print(paste("New File Created - Number of Runs:", nrow(new_data), "Ages:", ncol(new_data)))
}

