#' t test function
#'
#' This function can perform a one sample t test in R.
#'
#' @param x a numeric vector of data.
#' @param alternative a character string specifying the alternative hypothesis.
#'   This should only accept "two.sided", "less", or greater".
#' @param mu a number indicating the null hypothesis value of the mean.
#' @keywords inference
#'
#' @return A list with the numeric test statistic, the degrees of freedom,
#'   the value of the parameter \code{alternative}, the numeric p-value.
#'
#' @examples
#' lifeExp <- my_gapminder$lifeExp
#' my_t.test(lifeExp, "two.sided", 60)
#'
#' @export
my_t.test <- function(x, alternative, mu) {

  #mu.hat to store the mean of the sample
  #calculate the test statistic which we needed to test the hypothese
  #and round it into three digits
  #calculate the df which we also needed to get the p value
  mu.hat <- mean(x)
  test_stat <- (mu.hat - mu) / (sd(x)/ sqrt(length(x)))
  test_stat <- round(test_stat, 3)
  df <- length(x)-1

  #use if, else statement to calculate the p value
  #in different alternative hypothesis situations
  #if the alternative hypothesis is not match the three conditions below
  #it will stop and give hint to the user
  if(alternative == "greater") {
    my_pval <- pt(test_stat, df, lower.tail = FALSE)
  } else if (alternative == "less") {
    my_pval <- pt(test_stat, df, lower.tail = TRUE)
  } else if (alternative == "two.sided") {
    my_pval <- 2 * pt(abs(test_stat), df, lower.tail = FALSE)
  } else {
    stop("Alternative hypothesis must be 'two.sided', 'less' or 'greater' ")
  }

  #matrix to store the final statistics of our sample
  #and return the matrix
  output <- list(test_stat, df, my_pval, alternative)
  names(output) <- c("test stat", "df", "p-value", "alternative")
  return(output)
}
