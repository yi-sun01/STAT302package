#' K-nearest neighbors cross-validation function
#'
#' This function performs a k-neareast neighbors cross-validation.
#'   predicts "species" using four covariates bill_length_mm, bill_depth_mm,
#'   flipper_length_mm, and body_mass_g.
#'
#' @param train Input data frame.
#' @param cl True class value of the training data.
#' @param k_nn Integer representing the number of neighbors.
#' @param k_cv Integer representing the number of folds.
#' @keywords prediction
#'
#' @return A list of the predicted class and the CV misclassification rate.
#'
#' @examples
#' penguins_01 <- na.omit(my_penguins)
#' species_outcome <- dplyr::pull(penguins_01, var = "species")
#' penguins_01 <- dplyr::select(penguins_01, 3:6)
#' my_knn_cv(penguins_01, species_outcome, 5, 10)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {

  # initialize a vector to store the output of knn()
  my_class <- base::rep(NA, length(cl))

  # define the folds
  fold <- base::sample(base::rep(1:k_cv, length = nrow(train)))

  # initialize a vector to store the missclassification rate
  cv_error <- base::rep(NA, k_cv)

  for (i in 1:k_cv) {

    # split the input "train" into train and test set
    new_fold <- base::which(fold == i)
    data_train <- train[-new_fold,]
    data_test <- train[new_fold,]

    # split the input "cl" into train and test set
    cl_train <- cl[-new_fold]
    cl_test <- cl[new_fold]

    # compute the knn() function and store the results in my_class
    result <-  class::knn(train = data_train, test = data_test, cl = cl_train, k = k_nn)
    my_class[new_fold] <- as.character(result)

    # compute missclassification rate
    cv_error[i] <- mean(result != cl_test)

  }


  # compute the average of the miss-classification rates
  cv_err <- sum(cv_error) / k_cv

  # make predictions of the class by using knn() with the full data
  # as both the training and the test data
  class <- class::knn(train = train, test = train, cl = cl, k = k_nn)


  # store class and cv_error as a list
  final_output <- list("class" = class, "cv_error" = cv_err)

  # return the output of the function
  return(final_output)

}






