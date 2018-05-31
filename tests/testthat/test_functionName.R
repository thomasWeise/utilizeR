library("utilizeR")
context("function.name")

test_that("function.name", {
  expect_identical(function.name(sin), "sin")

  fzt <- function(x) {sin(x)};
  res1 <- function.name(fzt);  res1 <- force(res1);
  fztx <- fzt;
  res2 <- function.name(fztx); res2 <- force(res2);
  res3 <- function.name(fzt);  res3 <- force(res3);
  ff <- fzt;
  res4 <- function.name(ff);   res4 <- force(res4);
  res5 <- function.name(fztx); res5 <- force(res5);
  res6 <- function.name(fzt);  res6 <- force(res6);
  expect_identical(res1, "fzt")
  expect_identical(res2, "fzt")
  expect_identical(res3, "fzt")
  expect_identical(res4, "ff")
  expect_identical(res5, "ff")
  expect_identical(res6, "ff")

  library(methods);
  expect_identical(function.name(setClass), "methods::setClass");
  expect_identical(function.name(function.name), "utilizeR::function.name");
})
