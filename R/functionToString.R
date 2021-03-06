#' @title Convert a Function Body to a String
#' @description Convert a function body to a string
#' @param f the function
#' @return a string representation of the function
#' @export function.toString
#' @include functionName.R
#' @examples
#' function.toString(function(x,y) x+5*y)
#' # x+5*y
#' function.toString(function(x) sin(x))
#' # sin(x)
#' function.toString(sin)
#' # sin
function.toString <- function(f) {
  # get the body of the function
  ret        <- lapply(X=deparse(body(f)), FUN=trimws);
  ret.length <- length(ret);

  # handle the case of primitive functions for which we have no body
  if((ret.length == 1L) && (identical(ret[[1L]], "NULL"))) {
    return(function.name(f));
  }

  # remove useless surrounding "{ }" <- this is probably wrong
  while((ret.length > 2L) &&
     identical(ret[[1L]], "{") &&
     identical(ret[[ret.length]], "}")) {
    ret <- ret[2L:(ret.length - 1L)];
    ret.length <- ret.length - 2L;
  }

  # if we only have one line, we can return now
  if(ret.length <= 1L) { return(ret[[1]]); }

  # otherwise, we add ";" to each line not ending with "}"
  for(i in 1:(ret.length-1L)) {
    ret.i   <- ret[[i]];
    ret.i.l <- nchar(ret.i);
    if(!identical(substr(ret.i, ret.i.l, ret.i.l), "}")) {
      ret[[i]] <- paste(ret.i, ";", sep="", collapse="");
    }
  }

  # ...and paste everything together
  return(paste(ret, sep="", collapse=" "));
}
