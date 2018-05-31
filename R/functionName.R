#' @title Get the Name of a Function as String
#' @description Get the Name of a Function as String
#' @param f the function
#' @return a string representing the name of the function
#' @export function.name
#' @include ignoreErrors.R
#' @examples
#' function.name(sin)
#' # sin(x)
#' library(methods);
#' function.name(setClass)
#' # methods::setClass
#' f <- function(k) sin(k);
#' ff <- f;
#' function.name(ff);
#' # f
#' nest <- function(z) function.name(z);
#' nest(ff);
#' # f
#' nest(sin);
#' # sin
function.name <- function(f) {
  # first, we try to resolve the name of the function directly
  name <- paste(as.character(substitute(f)), sep="", collapse="");

  # now we check the loaded namespaces if we can find the function
  # we do so backwards,
  namespaces <- loadedNamespaces();
  for(i in seq.int(from=length(namespaces), to=1L, by=(-1L))) {
    nse <- namespaces[i];
    env <- getNamespace(nse);
    for(candidate in ls(env)) {
      if(identical(f, env[[candidate]])) {
        if(identical(nse, "base")) {
          return(candidate);
        }
        return(paste(nse, candidate, sep="::", collapse=""));
      }
    }
  }

  # now we iterate over all environments in the call stack and try to find the
  # shortest variable that matches the function
  for(i in 0L:(sys.nframe() - 1L)) {
    env <- sys.frame(i);
    notUpdated <- TRUE;

    for(candidate in ls(env)) {
      ttt <- FALSE;
      ignoreErrors(ttt <- identical(f, env[[candidate]]));
      if(ttt) {
        if(notUpdated || (nchar(name) > nchar(candidate))) {
          notUpdated <- FALSE;
          name       <- candidate;
        }
      }
    }
    if(!notUpdated) {
      return(name);
    }
  }

  return(name);
}
