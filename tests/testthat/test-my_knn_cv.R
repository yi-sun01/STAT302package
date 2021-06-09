# within test-my_knn_cv.R

# filter the data of my_penguins
penguins_test <- na.omit(my_penguins)
species_outcome_test <- dplyr::pull(penguins_test, var = "species")
penguins_test <- dplyr::select(penguins_test, 3:6)

test_that("my_knn_cv performs a k-nearest neighbor cross-validation", {
  expect_type(my_knn_cv(penguins_test, species_outcome_test, 5, 10), "list")

})
