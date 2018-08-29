library("utilizeR")
context("is.* checks")

test_that("is.not.na.or.null", {
  expect_identical(is.not.na.or.null(NA), FALSE);
  expect_identical(is.not.na.or.null(NULL), FALSE);
  expect_identical(is.not.na.or.null(""), TRUE);
  expect_identical(is.not.na.or.null(c(1, 2)), TRUE);
  expect_identical(is.not.na.or.null(character(0)), TRUE);
  expect_identical(is.not.na.or.null(integer(0)), TRUE);
  expect_identical(is.not.na.or.null(invisible(NULL)), FALSE);
  expect_identical(is.not.na.or.null(list(1)), TRUE);
  expect_identical(is.not.na.or.null(list(2)), TRUE);
  expect_identical(is.not.na.or.null("a"), TRUE);
  expect_identical(is.not.na.or.null("bc"), TRUE);
  expect_identical(is.not.na.or.null(NA_character_), FALSE);
  expect_identical(is.not.na.or.null(c("a", "bc")), TRUE);
})


test_that("is.non.empty.list", {
  expect_identical(is.non.empty.list(NA), FALSE);
  expect_identical(is.non.empty.list(NULL), FALSE);
  expect_identical(is.non.empty.list(""), FALSE);
  expect_identical(is.non.empty.list(c(1, 2)), FALSE);
  expect_identical(is.non.empty.list(character(0)), FALSE);
  expect_identical(is.non.empty.list(integer(0)), FALSE);
  expect_identical(is.non.empty.list(invisible(NULL)), FALSE);
  expect_identical(is.non.empty.list(list(1)), TRUE);
  expect_identical(is.non.empty.list(list(2)), TRUE);
  expect_identical(is.non.empty.list("a"), FALSE);
  expect_identical(is.non.empty.list("bc"), FALSE);
  expect_identical(is.non.empty.list(NA_character_), FALSE);
  expect_identical(is.non.empty.list(c("a", "bc")), FALSE);
})


test_that("is.non.empty.vector", {
  expect_identical(is.non.empty.vector(NA), FALSE);
  expect_identical(is.non.empty.vector(NULL), FALSE);
  expect_identical(is.non.empty.vector(""), TRUE);
  expect_identical(is.non.empty.vector(c(1, 2)), TRUE);
  expect_identical(is.non.empty.vector(character(0)), FALSE);
  expect_identical(is.non.empty.vector(integer(0)), FALSE);
  expect_identical(is.non.empty.vector(invisible(NULL)), FALSE);
  expect_identical(is.non.empty.vector(list(1)), FALSE);
  expect_identical(is.non.empty.vector(list(2)), FALSE);
  expect_identical(is.non.empty.vector("a"), TRUE);
  expect_identical(is.non.empty.vector("bc"), TRUE);
  expect_identical(is.non.empty.vector(NA_character_), FALSE);
  expect_identical(is.non.empty.vector(c("a", "bc")), TRUE);
})

test_that("is.non.empty.string", {
  expect_identical(is.non.empty.string(NA), FALSE);
  expect_identical(is.non.empty.string(NULL), FALSE);
  expect_identical(is.non.empty.string(""), FALSE);
  expect_identical(is.non.empty.string(c(1, 2)), FALSE);
  expect_identical(is.non.empty.string(character(0)), FALSE);
  expect_identical(is.non.empty.string(integer(0)), FALSE);
  expect_identical(is.non.empty.string(invisible(NULL)), FALSE);
  expect_identical(is.non.empty.string(list(1)), FALSE);
  expect_identical(is.non.empty.string(list(2)), FALSE);
  expect_identical(is.non.empty.string("a"), TRUE);
  expect_identical(is.non.empty.string("bc"), TRUE);
  expect_identical(is.non.empty.string(c("a", "bc")), FALSE);
  expect_identical(is.non.empty.string(NA_character_), FALSE);
})
