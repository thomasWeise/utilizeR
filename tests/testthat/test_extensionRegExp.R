library("utilizeR")
context("path.extensionRegExp")

test_that("path.extensionRegExp", {
   exp <- path.extensionRegExp(c("html", "jpg"))
   expect_identical(exp, "^.*\\.(html|jpg)$");
   expect_identical(length(grep(exp, "hellojpg")) > 0L, FALSE);
   expect_identical(length(grep(exp, "hello.jpg")) > 0L, TRUE);
   expect_identical(length(grep(exp, "hello.html")) > 0L, TRUE);
   expect_identical(length(grep(exp, "hellohtml")) > 0L, FALSE);
   expect_identical(length(grep(exp, "hello.htm")) > 0L, FALSE);
   expect_identical(length(grep(exp, "hello.html.txt")) > 0L, FALSE);
})
