library("utilizeR")
context("makeLogger")

test_that("makeLogger 1", {
  l <- makeLogger(TRUE);
  expect_true(is.function(l));
  expect_false(is.null(l));
  expect_identical(names(formals(l)), c("string"));
  expect_null(l("test"));
})


test_that("makeLogger 2", {
  l <- makeLogger(FALSE);
  expect_true(is.null(l));
})


test_that("makeLogger 3", {
  f <- tempfile();

  l <- makeLogger(f);
  expect_true(is.function(l));
  expect_false(is.null(l));
  expect_identical(names(formals(l)), c("string"));

  expect_null(l("test"));
  expect_true(file.exists(f));
  lines <- readLines(con=f);
  expect_length(lines, 1L);

  expect_null(l("test"));
  expect_true(file.exists(f));
  lines <- readLines(con=f);
  expect_length(lines, 2L);

  expect_null(l("test"));
  expect_true(file.exists(f));
  lines <- readLines(con=f);
  expect_length(lines, 3L);

  expect_null(l("test"));
  expect_true(file.exists(f));
  lines <- readLines(con=f);
  expect_length(lines, 4L);

  unlink(f);
})



test_that("makeLogger 4", {
  l <- makeLogger(TRUE);
  expect_true(is.function(l));
  expect_false(is.null(l));
  expect_identical(names(formals(l)), c("string"));

  l2 <- makeLogger(l);
  expect_identical(l, l2);
})
