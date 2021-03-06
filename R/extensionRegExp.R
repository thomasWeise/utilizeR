#' @title Create a Regular Expression that can match a File Name versus One or
#'   Multiple File Extension(s)
#' @description For a given list \code{extensions} of file extensions, e.g.,
#'   \code{c("txt", "csv")} or just a single file extension, this function
#'   creates a regular expression while will match a file path that ends in any
#'   of the extensions (with a leading ".").
#' @param extensions either a single string or array of strings, each holding a
#'   file extension
#' @param before.extension a string required to occur immediately before the
#'   file extension
#' @return the regular expression which will match them
#' @export path.extensionRegExp
#' @examples
#' exp <- path.extensionRegExp(c("html", "jpg"))
#' exp
#' # [1] "^.*\\.(html|jpg)$"
#' length(grep(exp, "hellojpg")) > 0L
#' # [1] FALSE
#' length(grep(exp, "hello.jpg")) > 0L
#' # [1] TRUE
#' length(grep(exp, "hello.html")) > 0L
#' # [1] TRUE
#' length(grep(exp, "hellohtml")) > 0L
#' # [1] FALSE
#' length(grep(exp, "hello.htm")) > 0L
#' # [1] FALSE
#' length(grep(exp, "hello.html.txt")) > 0L
#' # [1] FALSE
path.extensionRegExp <- function(extensions, before.extension=NULL) {
  start <- "^.*";
  if((!(is.null(before.extension))) && (nchar(before.extension) > 0L)) {
    start <- paste(start, "\\Q", before.extension, "\\E", sep="", collapse="");
  }
  if(length(extensions) == 1L) {
    paste(start, "\\.", extensions, "$", sep="", collapse="")
  } else {
    paste(start, "\\.(", paste(extensions, sep="", collapse="|"), ")$", sep="", collapse="")
  }
}
