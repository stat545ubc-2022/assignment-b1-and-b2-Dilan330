test_that("Function shows an error if non-numerical value is used as the height", {
  expect_error(Avg_height_diff("tall", male= FALSE))
})

test_that("Function shows an error if NA value is used as the height", {
  expect_error(Avg_height_diff(NA, male = TRUE))
})

test_that("Function produces a output with a charachter class type", {
  expect_true(is.character(Avg_height_diff(171, male = TRUE)))
})

# We can also use examples from datasets to test our function.
# We will comment out the needed code for these functions to work.
# We will be using datasets from the datateachr package

# library(datateachr)
# test_that("Function shows an error if non-numerical value is used as the height", {
#  expect_error(Avg_height_diff(cancer_sample$diagnosis, male= FALSE))
# })

# test_that("Function shows an error if NA value is used as the height", {
#   expect_error(Avg_height_diff(apt_buildings$no_barrier_free_accessible_units[6], male = TRUE))
# })

# test_that("Function produces a output with a charachter class type", {
#   expect_true(is.character(Avg_height_diff(cancer_sample$radius_mean[1], male = TRUE)))
# })
