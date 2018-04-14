library("utilizeR")
context("path.batchProcessor")


test_that("path.batchProcessor", {

  dir <- tempfile();
  dir.create(dir, showWarnings=FALSE, recursive=TRUE);

  dir.a <- file.path(dir, "a");
  dir.create(dir.a, showWarnings=FALSE, recursive=TRUE);

  dir.a.1 <- file.path(dir.a, "1");
  dir.create(dir.a.1, showWarnings=FALSE, recursive=TRUE);

  f1 <- file.path(dir.a.1, "xx.txt");
  file.create(f1);
  f2 <- file.path(dir.a.1, "xy.txt");
  file.create(f2);

  dest <- file.path(dir, "x", "y");
  processor <- path.batchProcessor(processor=function(source,dest) {
    return(list(src=source, dst=dest));
    }, dest=dest, suffix=".csv");

  lst <- processor(dir.a, f1);
  expect_identical(lst$src, f1);
  expect_identical(lst$dst, file.path(dest, "1", "xx.csv"));
  xd <- file.path(dest, "1");
  expect_true(dir.exists(xd));
  unlink(xd, recursive=TRUE);

  lst <- processor(dir.a, f2);
  expect_identical(lst$src, f2);
  expect_identical(lst$dst, file.path(dest, "1", "xy.csv"));
  xd <- file.path(dest, "1");
  expect_true(dir.exists(xd));
  unlink(xd, recursive=TRUE);

  lst <- processor(dir.a, c(f1, f2));
  expect_identical(lst$src, c(f1, f2));
  expect_identical(lst$dst, file.path(dest, "1", "x.csv"));
  xd <- file.path(dest, "1");
  expect_true(dir.exists(xd));


  unlink(dir, recursive = TRUE);
})
