library("utilizeR")
context("string.commonPrefix")

test_that("string.commonPrefix", {
  expect_identical(string.commonPrefix(""),
                   "");

  expect_identical(string.commonPrefix("a"),
                   "a");


  expect_identical(string.commonPrefix("ab"),
                   "ab");


  expect_identical(string.commonPrefix(c("a", "")),
                   "");
  expect_identical(string.commonPrefix(c("a", "b")),
                   "");
  expect_identical(string.commonPrefix(c("a", "a")),
                   "a");
  expect_identical(string.commonPrefix(c("a", "bb")),
                   "");
  expect_identical(string.commonPrefix(c("a", "ab")),
                   "a");


  expect_identical(string.commonPrefix(c("", "a")),
                   "");
  expect_identical(string.commonPrefix(c("b", "a")),
                   "");
  expect_identical(string.commonPrefix(c("a", "a")),
                   "a");
  expect_identical(string.commonPrefix(c("bb", "a")),
                   "");
  expect_identical(string.commonPrefix(c("ab", "a")),
                   "a");


  expect_identical(string.commonPrefix(c("aaaaa", "aaaaa")),
                   "aaaaa");
  expect_identical(string.commonPrefix(c("aaaaaa", "aaaaab")),
                   "aaaaa");
  expect_identical(string.commonPrefix(c("aaaaaa", "aaaaaa")),
                   "aaaaaa");
  expect_identical(string.commonPrefix(c("aaaaaa", "aaaaabb")),
                   "aaaaa");
  expect_identical(string.commonPrefix(c("aaaaaa", "aaaaaab")),
                   "aaaaaa");



  expect_identical(string.commonPrefix(c("aaaaa", "aaaaa", "a")),
                   "a");
  expect_identical(string.commonPrefix(c("aaaaaa", "aaaaab", "abc")),
                   "a");
  expect_identical(string.commonPrefix(c("aa", "aaaaaa", "aaaaaa")),
                   "aa");
  expect_identical(string.commonPrefix(c("aad", "aaaaaa", "aaaaabb")),
                   "aa");
  expect_identical(string.commonPrefix(c("aaaaaa", "acaaa", "aaaaaab")),
                   "a");
})
