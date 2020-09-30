context("osmObjects")

test_that("lenreg rejects errounous input", {
  expect_error(linreg_mod <- linreg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=iris))
  expect_error(linreg_mod <- linreg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis))
})
