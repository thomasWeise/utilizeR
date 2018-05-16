library("utilizeR")
context("find.finite")

test_that("find.finite log", {

  f <- log;

  r <- find.finite.nw(5, 10, f);
  expect_identical(r, c(5, f(5)));

  r <- find.finite.nw(-1, 1, f);
  expect_length(r, 2);
  expect_identical(f(r[1]), r[2]);
  expect_gte(r[1], 0);
  expect_lt(r[1], 1e-4);


  r <- find.finite.nw(-100000, 100000, f);
  expect_length(r, 2);
  expect_identical(f(r[1]), r[2]);
  expect_gte(r[1], 0);
  expect_lt(r[1], 1e-4);
})


test_that("find.finite asin", {

  f <- asin;

  r <- find.finite.nw(0, 10, f);
  expect_identical(r, c(0, f(0)));

  r <- find.finite.nw(-10, 10, f);
  expect_length(r, 2);
  expect_identical(f(r[1]), r[2]);
  expect_gte(r[1], -1);
  expect_lt(r[1], -1 + 1e-4);

  r <- find.finite.nw(-100000, 100000, f);
  expect_length(r, 2);
  expect_identical(f(r[1]), r[2]);
  expect_gte(r[1], -1);
  expect_lt(r[1], -1 + 1e-4);

  r <- find.finite.nw(10, -10, f);
  expect_length(r, 2);
  expect_identical(f(r[1]), r[2]);
  expect_lte(r[1], 1);
  expect_gt(r[1], 1 - 1e-4);

  r <- find.finite.nw(100000, -100000, f);
  expect_length(r, 2);
  expect_identical(f(r[1]), r[2]);
  expect_lte(r[1], 1);
  expect_gt(r[1], 1 - 1e-4);
})


test_that("find.finite 1/x", {

  f <- function(x) 1/x;

  r <- find.finite.nw(5, 10, f);
  expect_identical(r, c(5, f(5)));

  r <- find.finite.nw(0, 1, f);
  expect_length(r, 2);
  expect_identical(f(r[1]), r[2]);
  expect_gt(r[1], 0);
  expect_lt(r[1], 1e-4);

  r <- find.finite.nw(0, 100000, f);
  expect_length(r, 2);
  expect_identical(f(r[1]), r[2]);
  expect_gt(r[1], 0);
  expect_lt(r[1], 1e-4);

  r <- find.finite.nw(0, -1, f);
  expect_length(r, 2);
  expect_identical(f(r[1]), r[2]);
  expect_lt(r[1], 0);
  expect_gte(r[1], -1e-4);

  r <- find.finite.nw(0, -10000, f);
  expect_length(r, 2);
  expect_identical(f(r[1]), r[2]);
  expect_lt(r[1], 0);
  expect_gte(r[1], -1e-4);
})
