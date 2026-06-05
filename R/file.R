#' Choose a file to open or save a file interactively.
#'
#' This function allows the user to open a File Dialog interface to interatively choose to load or save a file.
#' Completion of this function will only return the filepath; it will not load nor save files by itself. TODO: RStudio/tcltk/vscode
#'
#' @param type Character option to request a "Open" or "Save file Dialog
#' @param ext Two item vector describing the filename description and filename extension respecively. By default it is assigned to `c("All Files", "*")`
#'
#' @importFrom checkmate assert_character
#'
#' @returns A Character vector representing the filepath of the chosen path frpm the file dialog.
#' @keywords internal
#'
file_dialog <- function(type = c("open", "save"), ext = c("All Files", ".*")) {
  #Validate Parameters
  type <- match.arg(type)
  ext <- validate_filetype(ext)

  #Dialog msg
  err_msg_dialog_cancelled <- "File choice cancelled"
  msg_caption_open <- "Open File"
  msg_caption_save <- "Save File As"
  pattern_ext <- paste0(ext[1], " (*", ext[2], ")")
  tcltk_pattern_ext <- paste0("{{", ext[1], "} {", ext[2], "}}")

  #Detect frontend
  r_frontend <- get_r_frontend()

  # RSTUDIO (rstudioapi) dialogs
  #
  if (
    r_frontend == "RStudio" && requireNamespace("rstudioapi", quietly = TRUE)
  ) {
    #Open Dialog
    if (type == "open") {
      path <- rstudioapi::selectFile(
        caption = msg_caption_open,
        existing = TRUE,
        filter = pattern_ext
      )
    } else {
      path <- rstudioapi::selectFile(
        caption = msg_caption_save,
        label = "Save",
        existing = FALSE,
        filter = pattern_ext
      )
    }
    #Check if user cancels file dialog window
    tryCatch(
      {
        checkmate::assert_character(path, len = 1)
      },
      error = function(cond) {
        message(err_msg_dialog_cancelled)
        return(invisible())
      }
    )
    return(path.expand(path))
  }

  # POSITRON, VSCODE, R for Windows (choose.files)
  # Windows native dialogs work with Postiron/Vs Code/Rgui
  # Using "All Files" Filter
  if (.Platform$OS.type == "windows") {
    if (type == "open") {
      path <- utils::choose.files(
        caption = msg_caption_open,
        multi = FALSE,
        filters = Filters["All", ]
      )
    } else {
      path <- utils::choose.files(
        default = "untitled.inp",
        caption = msg_caption_save,
        multi = FALSE,
        filters = Filters["All", ]
      )
    }
    #Check if user cancels file dialog window
    tryCatch(
      {
        checkmate::assert_character(path, len = 1)
      },
      error = function(cond) {
        message(err_msg_dialog_cancelled)
        return(invisible())
      }
    )
    return(path.expand(path))
  }

  # TCL/TK fallback (Mac/Linux): 'tcltk' package
  #
  # TODO: Need to Close Tcltk mangager after it is done. Refactor to own function.
  if (isFALSE(requireNamespace("tcltk", quietly = TRUE))) {
    stop(
      "Package 'tcltk' is required for file dialogs windows for Mac and Linux systems"
    )
  } else {
    if (type == "open") {
      path <- tcltk::tclvalue(
        tcltk::tkgetOpenFile(
          initialdir = here::here(),
          filetypes = tcltk_pattern_ext
        )
      )
    } else {
      path <- tcltk::tclvalue(
        tcltk::tkgetSaveFile(
          initialdir = here::here(),
          filetypes = tcltk_pattern_ext
        )
      )
    }
    # Validation
    tryCatch(
      {
        checkmate::assert_character(path, min.chars = 1, .var.name = "path")
      },
      error = function(cond) {
        message(err_msg_dialog_cancelled)
        return(invisible())
      }
    )
    return(path)
  }
}


#' Open file dialog or interface to interactively return file path.
#'
#' If Rconsole is currently running in Rstudio, it will use the rstudioapi
#' to show the file dialog window over the IDE.
#'
#' @keywords internal
#'
#' @param filetype filename extension
#'
#' @importFrom checkmate assert_character
#'
open_file_dialog <- function(filetype) {
  return(file_dialog(type = "open", ext = filetype))
}

#' Save file dialog or interface to interactively return file path.
#'
#' If Rconsole is currently running in Rstudio, it will use the rstudioapi
#' to show the file dialog window over the IDE.
#'
#' @keywords internal
#'
#' @importFrom checkmate assert_character
#'
save_file_dialog <- function() {
  # Defaults to "All Files"
  return(file_dialog(type = "save"))
}


#' Retruns GUI Frontend of current R process.
#'
#' Helper function to get the GUI frontend attached to the current Rconsole process.
#' This informtion will be used to determine the rstudioapi, windows native base R, or
#' muiltiplatform tcl/tk.
#'
get_r_frontend <- function() {
  # Positron : Check for POSITRON env first. Positron shares the same vscode terminal.
  if (Sys.getenv("POSITRON") == 1) {
    return("Positron")
  }

  # VS Code
  if (Sys.getenv("TERM_PROGRAM") == "vscode") {
    return("vscode")
  }

  # RStudio
  if (Sys.getenv("RSTUDIO") == 1) {
    return("RStudio")
  }

  # Fallback
  return(.Platform$GUI)
}


#' Reads a line of numeric strings from the AGEPRO input file connection.
#'
#' Reads in a line from the open file connection, splits the string
#' into substrings by whitespace, validates for numerical strings, and
#' then converts to numerical vector.
#'
#' Function uses regular expressions to validate if the string vector is
#' numeric. If non-digits were found, then function will throw an exception.
#'
#' @template inp_con
#'
#' @keywords internal
#'
read_inp_numeric_line <- function(inp_con) {
  if (!isOpen(inp_con)) {
    stop("No open file Connection to AGEPRO input file")
  }

  inp_line <-
    unlist(strsplit(readLines(inp_con, n = 1, warn = FALSE), " +"))

  ## Validate numeric substrings

  numeric_regex <- "^[-]?[[:digit:]]"

  if (!all(grepl(numeric_regex, inp_line))) {
    non_numerics <- inp_line[!grepl(numeric_regex, inp_line)]
    stop(
      "Line contains a Non Numeric Substring",
      paste(non_numerics, collapse = ", ")
    )
  }

  return(invisible(as.numeric(inp_line)))
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
  checkmate::assert_vector(
    filetype,
    all.missing = FALSE,
    len = 2,
    null.ok = FALSE
  )
  return(filetype)
}
