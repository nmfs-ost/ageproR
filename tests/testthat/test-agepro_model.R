test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


# Test case: Create a agepro_model with:
# - yr_start = 2019
# - yr_end = 2026
# - age_begin = 1
# - age_end = 32
# - num_pop_sims = 1000
# - num_fleets = 4
# - num_recruits = 1
# - seed = 300
test_that("New agepro_model (Year: 2019-2026, Age: 1-32, num_pop_sims: 1000, num_fleets: 4, 1 NULL Recruitment, discards: 0, seed: 300), and NULL Bootstrap file", {
  expect_snapshot(
    ageproR::agepro_model$new(2019, 2026, 1, 32, 1000, 4, 1, 0, 300),
    cnd_class = TRUE
  )
})


test_that("extdata/Example1.INP exists", {
  inpfile_path <- system.file("extdata", "Example1.INP", package = "ageproR")
  expect_true(file.exists(inpfile_path))
})
test_that("extdata/Example1.BSN exists", {
  bsnfile_path <- system.file("extdata", "Example1.BSN", package = "ageproR")
  expect_true(file.exists(bsnfile_path))
})


# Opening Agepro Model with Example1.INP works
# Note: Using original agepro_inp_model$read_inp workflow.
test_that("Opening extdata/Example1.INP is imported to test agepro_inp_model", {
  # Create agepro_inp_model "test"
  expect_snapshot(
    test <- ageproR::agepro_inp_model$new(seed = 300),
    cnd_class = TRUE
  )
  # Create a input file and check if path works
  inpfile_path <- system.file("extdata", "Example1.INP", package = "ageproR")
  expect_true(file.exists(inpfile_path))
  # import AGEPRO input file data to "test"
  ## Note: The bootstrap_file output is masked to a fixed value to fix varying outputs
  ## using testthat and running tests with devtools::check()
  expect_snapshot(
    test$read_inp(inpfile_path),
    cnd_class = TRUE,
    transform = \(lines) {
      gsub(test$bootstrap$bootstrap_file, "<path>", lines, fixed = TRUE)
    }
  )
})

#Test that importing agepro_inp_model data from extdata/Example1.INP (with bootstrap_file)
#is exported to a new to agepro_json_model class instance.
test_that("Import agepro_inp_model (test_inp) class data to agepro_json_model (test_json) class", {
  # Create a input file and check if path works
  inpfile_path <- system.file("extdata", "Example1.INP", package = "ageproR")
  expect_true(file.exists(inpfile_path))
  # import_agepro_model from input file to "test_inp"
  # Note: The bootstrap_file output is masked to a fixed value to fix varying
  # outputs using testthat and running tests with devtools::check()
  expect_snapshot(
    test_inp <- import_agepro_model(inpfile_path),
    cnd_class = TRUE,
    transform = \(lines) {
      gsub(test_inp$bootstrap$bootstrap_file, "<path>", lines, fixed = TRUE)
    }
  )
  # Create a agepro_json_model as "test_json"
  expect_snapshot(
    test_json <- ageproR::agepro_json_model$new(0, 9, 0, 1, 1000, 4, 1, 0, 300),
    cnd_class = TRUE
  )
  # Set "test_json" wtih "test_inp" values
  expect_snapshot(test_json$import_agepro_inp_model(test_inp), cnd_class = TRUE)
})
