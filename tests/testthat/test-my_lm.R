# within test-my_lm.R
matrix1 <- matrix(nrow= 3, ncol = 4)
estimate <- c(47.88852, 0.00045, 13.59272, 8.65779, 17.57234, 18.14604)
std_error <- c(0.33981, 0.00002, 0.60080, 0.55550,
               0.62576, 1.78747)
t_value <- c(140.927342   , 22.500000  , 22.624368  , 15.585581  , 28.081597  ,
             10.151801)
p_value <- c(0.000000e+00, 3.626474e-74, 2.849211e-99, 2.733496e-51,
             7.693794e-143, 1.506075e-23)

matrix1 <- cbind(estimate, std_error, t_value, p_value)
colnames(matrix1) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
rownames(matrix1) <- c("(Intercept)", "gdpPercap", "continentAmericas",
                       "continentAsia", "continentEurope",
                       "continentOceania")
#matrix1 <- as.data.frame(matrix1)



test_that("my_lm works correctly", {
  expect_equal(my_lm(lifeExp ~ gdpPercap + continent, my_gapminder), matrix1)
})
