library("utilizeR")
context("function.toString")

test_that("function.toString", {
  expect_identical(function.toString(function(x) sin(x)), "sin(x)")
  expect_identical(function.toString(function(x) {sin(x)}), "sin(x)")
  expect_identical(function.toString(function(x) {x<-5*x; sin(x)}), "x <- 5 * x; sin(x)")
})
