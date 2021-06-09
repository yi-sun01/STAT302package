# within test-my_rf_cv.R

test_that("my_rf_cv function performs a random forest cross-validation", {
  expect_type(my_rf_cv(5), "double")
})
