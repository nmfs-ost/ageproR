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
  expect_snapshot(ageproR::agepro_model$new(2019,2026,1,32,1000,4,1,0,300), cnd_class = TRUE)
})


#Note: The bootstrap_file output is masked to a fixed value to fix varying outputs using testthat and running tests
#with devtools::check()

#Test that Opening inst/test-example4,inp works
test_that("Opening inst/test-example4.inp is imported to test agepro_inp_model", {
  expect_snapshot(test <- ageproR::agepro_inp_model$new(seed=300), cnd_class = TRUE)
  expect_snapshot(test$read_inp(file.path(testthat::test_path(),"example/test-example4.inp")), cnd_class = TRUE,
                  transform = \(lines) gsub(test$bootstrap$bootstrap_file, "<path>", lines, fixed = TRUE))
})

#Test that Opening inst/test-example4.inp works, and Set to Bootstrap file
test_that("Setting inst/Example1.BSN to test agepro_inp_model works",{
  expect_snapshot(test <- ageproR::agepro_inp_model$new(seed=300), cnd_class = TRUE)
  expect_snapshot(test$read_inp(file.path(testthat::test_path(),"example/test-example4.inp")), cnd_class = TRUE,
                  transform = \(lines) gsub(test$bootstrap$bootstrap_file, "<path>", lines, fixed = TRUE))
  expect_snapshot(test$set_bootstrap_filename(file.path(testthat::test_path(),"example/Example1.BSN")), cnd_class = TRUE)
})

#Can that Opening inst/test-example4.inp works, and Set to Bootstrap file be exported to agepro_json_model class?
test_that("Import 'test' agepro_inp_model class data to 'json_test' agepro_json_model class",{
  expect_snapshot(test <- ageproR::agepro_inp_model$new(seed=300), cnd_class = TRUE)
  expect_snapshot(test$read_inp(file.path(testthat::test_path(),"example/test-example4.inp")), cnd_class = TRUE,
                  transform = \(lines) gsub(test$bootstrap$bootstrap_file, "<path>", lines, fixed = TRUE))
  expect_snapshot(test_json <- ageproR::agepro_json_model$new(0,9,0,1,1000,4,1,0,300), cnd_class = TRUE)
  expect_snapshot(test_json$import_agepro_inp_model(test), cnd_class = TRUE)
})


