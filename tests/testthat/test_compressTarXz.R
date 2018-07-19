library("utilizeR")
context("compress.tar.xz")


test_that("compress.tar.xz", {
  src <- tempfile();

  write.csv(x=matrix(runif(n=1000), nrow=100, ncol=10), file=src);
  expect_true(file.exists(src));

  res <- compress.tar.xz(src);
  if(!is.null(res)) {
    expect_true(file.exists(res));
    expect_gt(file.size(res), 100L);
    unlink(res, recursive = FALSE);
  }

  unlink(src, recursive = FALSE);
})
