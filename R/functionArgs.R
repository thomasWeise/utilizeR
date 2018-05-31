#' @title Get the Names of the Arguments of a Function
#' @description Get the names of the arguments of a function as a vector of
#'   strings.
#' @param f the function
#' @return the vector of function arguments, or \code{NULL} if the function has
#'   no arguments
#' @export function.args
#' @examples
#' function.args(sin)
#' # [1] "x"
#' f <- function(x) sin(x);
#' function.args(f)
#' # [1] "x"
#' f <- function() { }
#' function.args(f)
#' # NULL
function.args <- function(f) {
  if(is.primitive(f)) { f <- args(f); }
  names(formals(f));
}
