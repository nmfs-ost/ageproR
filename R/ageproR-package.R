#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom withr local_options
## usethis namespace: end
NULL

# ageproR package Startup
.onAttach <- function(libname, pkgname) {
  # Fetch 'AGEPRO_PATH' user's environmental variable. If not found, returns "".
  agepro_exepath <- Sys.getenv("AGEPRO_PATH")

  if (agepro_exepath == "") {
    packageStartupMessage(
      "Note: User Renviron variable 'AGEPRO_PATH' is not set."
    )
    return()
  }

  # Depencency-less checks
  # Check if AGEPRO_PATH exists
  if (isFALSE(file.exists(agepro_exepath))) {
    packageStartupMessage(sprintf(
      "Warning: Path in 'AGEPRO_PATH' does not exist (%s)",
      agepro_exepath
    ))
    return()
  }
  # Check if its a *.exe
  if (isFALSE(grepl("\\.exe$", agepro_exepath, ignore.case = TRUE))) {
    packageStartupMessage(sprintf(
      "Warning: 'AGEPRO_PATH' is not a *.exe binary path (%s)",
      agepro_exepath
    ))
    return()
  }

  # If all checks pass, print AGEPRO_PATH.
  packageStartupMessage("'AGEPRO_PATH' found at: ", agepro_exepath)
}
