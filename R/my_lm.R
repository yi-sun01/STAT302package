#' Linear model function
#'
#' This function can fit a linear model in R.
#'
#' @param formula  a \code{formula} class object, similar to lm().
#' @param data input data frame.
#' @keywords inference
#'
#' @return A table with rows for each coefficient (including the (Intercept)!)
#'   and columns for the Estimate, Std. Error, t value, and Pr(>|t|).
#'
#' @examples
#' lifeExp <- my_gapminder$lifeExp
#' gdpPercap <- my_gapminder$gdpPercap
#' continent <- my_gapminder$continent
#' my_lm(lifeExp ~ gdpPercap + continent, my_gapminder)
#'
#' @importFrom stats model.frame model.matrix model.response
#'   predict pt sd na.omit
#'
#' @export
my_lm <- function(formula, data) {

  #using model.frame()to construct the data into frame
  #using model.matrix to extract the independent variable from the frame
  #using model.response to extract the dependent variable from the frame
  #turning y into matrix form for later calculation
  example <- model.frame(formula, data)
  x <- model.matrix(formula, data = example)
  y <- model.response(data = example)
  y <- as.matrix(y)

  #using beta_hat to store the slope coefficient
  #calculate the df
  #calculate the theta square in order to calculate standard error
  beta_hat <- round((solve(t(x) %*% x)) %*% t(x) %*% y, 5)
  df <- length(y)-  ncol(x)
  data1 <- (y - x %*% beta_hat) ^ 2
  data2 <- data1 / df
  theta <- sum(data2)

  #first use error to store the diagnal value of the matrix
  #then use std_error to store the standard error of value after square
  error <- diag(theta * solve(t(x) %*% x))
  std_error <- round(sqrt(error),5)

  #using t_value to store test statistics
  #p_value to store the p value of the hypothesis testing
  t_value <- beta_hat / std_error
  p_value <- 2 * pt(abs(t_value), df, lower.tail = FALSE)

  #forming a matrix to store the final statistics which we want to show
  #transform the matrix into data frame
  #and return the final data frame
  matrix1 <- matrix(nrow= ncol(x), ncol = 4)
  matrix1 <- cbind(beta_hat, std_error, t_value, p_value)
  colnames(matrix1) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  matrix1 <- as.data.frame(matrix1)
  return(matrix1)
}
