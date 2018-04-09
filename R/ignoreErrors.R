# a function ignoring its arguments
.ignore <- function(e) NULL

#' @title Execute the Expression and Ignore All Errors
#' @description Execute an expression and ignore all errors.
#' @param exp the expression
#' @export ignoreErrors
ignoreErrors <- function(exp) {
  tryCatch(suppressWarnings(exp), error=.ignore)
}
