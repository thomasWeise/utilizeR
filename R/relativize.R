#' Relativize a Path versus another Path
#'
#' Relativizes a path \code{path} versus a directory \code{dir}. Both
#' \code{path} and \code{dir} are normalized. If \code{dir} and \code{path}
#' identify the same object, \code{""} is returned. Otherwise, if the returned
#' path is appended to \code{dir} via \code{\link{file.path}}, the resulting
#' path should identify \code{path}.
#'
#' Inspired by the function of the same name in the \code{nonmemica} package,
#' see https://github.com/cran/nonmemica/blob/master/R/path.R.
#' @param path the path to be normalized
#' @param dir the reference directory to normalize against
#' @param sep path separator
#' @export path.relativize
path.relativize <- function(path, dir=getwd(), sep=.Platform$file.sep){

  # first obtain normalized equivalents of the two paths
  path.normalized <- normalizePath(path, winslash=sep, mustWork=FALSE);
  dir.normalized  <- normalizePath(dir, winslash=sep, mustWork=FALSE);

  # if both are identical...
  if(identical(path.normalized, dir.normalized)) {
    return(""); # return empty string
  }

  # windows: test for different drives
  if(!identical(substr(path.normalized, 1L, 1L),
                substr(dir.normalized,  1L, 1L))){
    return(path.normalized);
  }

  # split the paths at the separator
  path.normalized <- strsplit(path.normalized, sep)[[1]];
  dir.normalized  <- strsplit(dir.normalized, sep)[[1]];

  # find the common start
  index                  <- 1L;
  path.normalized.length <- length(path.normalized);
  dir.normalized.length  <- length(dir.normalized);
  length.max <- min(path.normalized.length, dir.normalized.length);
  while( (index <= length.max) &&
         identical(path.normalized[[index]], dir.normalized[[index]])) {
    index <- index + 1L;
  }

  do.call(file.path, as.list( # re-combine the paths
          # non-common part of dir becomes ".."
          c( rep('..', dir.normalized.length - index + 1L),
          # non-common part of path is added
             tail(path.normalized, path.normalized.length - index + 1L))))
}
