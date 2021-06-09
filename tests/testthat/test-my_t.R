# within test-my_t.test.R

output1 <- list(-1.68, 1703, 0.95342964, "greater")
names(output1) <- c("test stat", "df", "p-value", "alternative")

output2 <- list(-1.68, 1703, 0.046570362, "less")
names(output2) <- c("test stat", "df", "p-value", "alternative")

output3 <- list(-1.68, 1703, 0.093140724, "two.sided")
names(output3) <- c("test stat", "df", "p-value", "alternative")


test_that("my_t.test work for testing a hypothesis", {
  expect_equal(my_t.test(my_gapminder$lifeExp, "greater", 60),output1)
  expect_equal(my_t.test(my_gapminder$lifeExp, "less", 60), output2)
  expect_equal(my_t.test(my_gapminder$lifeExp, "two.sided", 60), output3)


})
test_that("non-suitable input throws informative error", {
  expect_error(my_t.test(my_gapminder$lifeExp, "greater than", 60))
})
