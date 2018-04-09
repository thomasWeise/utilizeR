library("utilizeR")
context("ignoreErrors")

test_that("ignoreErrors (1)", {
  a <- 23;
  ignoreErrors(a <- 10);
  expect_identical(a, 10);
})

test_that("ignoreErrors (1)", {
  a <- 23;
  ignoreErrors({a <- 10; stop("xx"); a <- 11;});
  expect_identical(a, 10);
})
