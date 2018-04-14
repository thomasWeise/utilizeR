library("utilizeR")
context("path.batchApply")


.file.make <- function(dir, name) {
  con <- file(file.path(dir, name), open="wt");
  writeLines(text=c(dir, name), con=con);
  close(con);
}

.make.dirs <- function() {
  dir <- tempfile();
  dir.create(dir, showWarnings=FALSE, recursive=TRUE);

  dir.a <- file.path(dir, "a");
  dir.create(dir.a, showWarnings=FALSE, recursive=TRUE);
  .file.make(dir.a, "1.txt");
  .file.make(dir.a, "2.txt");
  .file.make(dir.a, "3.txt");

  dir.b <- file.path(dir, "b");
  dir.create(dir.b, showWarnings=FALSE, recursive=TRUE);

  dir.b.1 <- file.path(dir.b, "1");
  dir.create(dir.b.1, showWarnings=FALSE, recursive=TRUE);
  .file.make(dir.b.1, "4.txt");
  .file.make(dir.b.1, "5.txt");

  dir.b.2 <- file.path(dir.b, "2");
  dir.create(dir.b.2, showWarnings=FALSE, recursive=TRUE);
  .file.make(dir.b.2, "6.txt");
  .file.make(dir.b.2, "x.csv");
  .file.make(dir.b.2, "y.csv");
  .file.make(dir.b.2, "z.csv");

  dir.c <- file.path(dir, "c");
  dir.create(dir.c, showWarnings=FALSE, recursive=TRUE);
  .file.make(dir.c, "7.txt");
  .file.make(dir.c, "a.csv");
  .file.make(dir.c, "b.csv");
  .file.make(dir.c, "8.txt");

  return(dir);
}

.test.batchApply <- function(cores) {
  dir <- .make.dirs();

  func <- function(root, path) {
    path <- unname(unlist(lapply(X=path,
                   FUN=function(n)
                        paste(basename(dirname(n)),
                              "/",
                              basename(n),
                              sep="", collapse="")),
                   recursive=TRUE));
    sort(path);
    return(paste(path, sep="", collapse="-"));
  }
  single.pattern <- path.extensionRegExp("txt");
  single.env <- new.env();
  assign(x=single.pattern, value=func, envir=single.env);

  all.pattern <- path.extensionRegExp("csv");
  all.env <- new.env();
  assign(x=all.pattern, value=func, envir=all.env);

  result <- path.batchApply(path=dir, file.single=single.env, file.in.folder = all.env, cores=cores);

  unlink(dir, recursive=TRUE);

  expect_length(result, 10L);
  expect_identical(result[1], "a/1.txt");
  expect_identical(result[2], "a/2.txt");
  expect_identical(result[3], "a/3.txt");
  expect_identical(result[4], "1/4.txt");
  expect_identical(result[5], "1/5.txt");
  expect_identical(result[6], "2/x.csv-2/y.csv-2/z.csv");
  expect_identical(result[7], "2/6.txt");
  expect_identical(result[8], "c/a.csv-c/b.csv");
  expect_identical(result[9], "c/7.txt");
  expect_identical(result[10], "c/8.txt");

  return(result);
}



test_that("path.batchApply cores=1", {
  .test.batchApply(1L);
})


test_that("path.batchApply cores=2", {
  .test.batchApply(2L);
})

test_that("path.batchApply cores=3", {
  .test.batchApply(3L);
})

test_that("path.batchApply cores=4", {
  .test.batchApply(4L);
})

test_that("path.batchApply cores=5", {
  .test.batchApply(5L);
})

test_that("path.batchApply cores=6", {
  .test.batchApply(6L);
})

test_that("path.batchApply cores=7", {
  .test.batchApply(7L);
})

test_that("path.batchApply cores=8", {
  .test.batchApply(8L);
})

test_that("path.batchApply cores=9", {
  .test.batchApply(9L);
})

test_that("path.batchApply cores=10", {
  .test.batchApply(10L);
})

test_that("path.batchApply cores=11", {
  .test.batchApply(11L);
})

test_that("path.batchApply cores=12", {
  .test.batchApply(12L);
})

test_that("path.batchApply cores=13", {
  .test.batchApply(13L);
})
