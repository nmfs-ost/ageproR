test_that("write_inp writes numeric fields in decimal notation", {
  inpfile <- system.file("extdata", "Example1.INP", package = "ageproR")
  suppressMessages(capture.output(model <- import_agepro_model(inpfile)))
  expect_equal(model$recruit$recruit_scaling_factor, 1000000)

  bootstrap_file <- file.path(tempdir(), "bootstrap-1e+06.BSN")
  file.copy(
    system.file("extdata", "Example1.BSN", package = "ageproR"),
    bootstrap_file,
    overwrite = TRUE
  )
  suppressMessages(model$bootstrap$bootstrap_file <- bootstrap_file)
  model$case_id$model_name <- "Scenario 1e+06"
  model$bootstrap$pop_scale_factor <- 1e7
  suppressMessages(model$recruit$ssb_scaling_factor <- 1.25e-8)
  values <- model$stock_weight$parameter_table
  values[1, 1:2] <- c(1e-8, 1e6)
  model$stock_weight$parameter_table <- values
  observations <- model$recruit$recruit_data[[1]]$observations
  observations[1:2, 1] <- c(1e-8, 1e6)
  model$recruit$recruit_data[[1]]$observations <- observations

  withr::local_options(list(scipen = -9, OutDec = ","))
  expect_equal(as.character(1e6), "1e+06")
  outfile <- tempfile(fileext = ".INP")
  #Write to the temporary outfile
  suppressMessages(model$write_inp(outfile))
  expect_equal(getOption("scipen"), -9)
  expect_equal(getOption("OutDec"), ",")

  lines <- readLines(outfile)
  expect_equal(
    lines[match("[RECRUIT]", lines) + 1],
    "1000000  0.0000000125  75"
  )
  expect_equal(lines[match("[BOOTSTRAP]", lines) + 1], "1000  10000000")
  expect_equal(lines[match("[BOOTSTRAP]", lines) + 2], bootstrap_file)
  expect_equal(lines[match("[CASEID]", lines) + 1], "Scenario 1e+06")
  expect_match(
    lines[match("[STOCK_WEIGHT]", lines) + 2],
    "^0\\.00000001  1000000  "
  )
  expect_match(
    lines[match("[RECRUIT]", lines) + 10],
    "^0\\.00000001  1000000  "
  )

  #Roundtrip check
  options(scipen = 0, OutDec = ".")
  suppressMessages(capture.output(roundtrip <- import_agepro_model(outfile)))
  expect_equal(roundtrip$recruit$recruit_scaling_factor, 1e6)
  expect_equal(roundtrip$recruit$ssb_scaling_factor, 1.25e-8)
  expect_equal(
    unname(roundtrip$stock_weight$parameter_table[1, 1:2]),
    c(1e-8, 1e6)
  )
  expect_equal(
    roundtrip$recruit$recruit_data[[1]]$observations[1:2, 1],
    c(1e-8, 1e6)
  )
})

test_that("write_inp restores numeric options after an error", {
  inpfile <- system.file("extdata", "Example1.INP", package = "ageproR")
  suppressMessages(capture.output(model <- import_agepro_model(inpfile)))
  withr::local_options(list(scipen = -9, OutDec = ","))

  # Check that numeric options are restored after an error
  suppressMessages(expect_error(suppressWarnings(model$write_inp(file.path(
    tempfile(),
    "x.INP"
  )))))
  expect_equal(getOption("scipen"), -9)
  expect_equal(getOption("OutDec"), ",")
})
