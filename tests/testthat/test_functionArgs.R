library("utilizeR")
context("function.args")

test_that("function.args", {
  expect_identical(function.args(sin), c("x"))
  expect_identical(function.args(log), c("x", "base"))
  expect_identical(function.args(function(x, y) x+y), c("x", "y"))
})
