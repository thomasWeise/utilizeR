library("utilizeR")
context("functionToString")

test_that("functionToString", {
  expect_identical(functionToString(function(x) sin(x)), "sin(x)")
  expect_identical(functionToString(function(x) {sin(x)}), "sin(x)")
  expect_identical(functionToString(function(x) {x<-5*x; sin(x)}), "x <- 5 * x; sin(x)")
})
