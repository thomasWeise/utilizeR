#' @title Check whether Something is NOT \code{NULL} or \code{NA}
#' @description  Check whether an object is NOT \code{NULL} or \code{NA}.
#' @param t the object to check
#' @return \code{TRUE} if neither \code{is.na(t)} nor \code{is.null(t)} holds,
#'   \code{FALSE} otherwise
#' @export is.not.na.or.null
is.not.na.or.null <- function(t) {
  if(is.null(t)) { return(FALSE); }
  if((is.vector(t) || is.list(t)) &&
     (length(t) != 1L)) { return(TRUE); }
  if(is.integer(t) ||
     is.numeric(t) ||
     is.character(t) ||
     is.logical(t)) {
    return(suppressWarnings(!is.na(t)));
  }
  return(TRUE);
}

#' @title Check whether Something is a non-empty list
#' @description  Check whether an object is a non-empty list.
#' @param t the object to check
#' @return \code{TRUE} if \code{is.null(t)} does not hold
#'   and \code{is.list(t)} and \code{length(t) > 0L} hold, \code{FALSE}
#'   otherwise
#' @export is.non.empty.list
is.non.empty.list <- function(t) {
  return((!(is.null(t))) && is.list(t) && (length(t) > 0L));
}

#' @title Check whether Something is a non-empty vector
#' @description  Check whether an object is a non-empty vector.
#' @param t the object to check
#' @return \code{TRUE} if neither \code{is.na(t)} nor \code{is.null(t)} holds
#'   and \code{is.vector(t)} and \code{length(t) > 0L} hold, \code{FALSE}
#'   otherwise
#' @export is.non.empty.vector
is.non.empty.vector <- function(t) {
  if(is.null(t)) { return(FALSE); }
  if(is.vector(t) && (!(is.list(t)))) {
    l <- length(t);
    if(l > 0L) {
      if((l == 1L) &&
         (is.integer(t) ||
          is.numeric(t) ||
          is.character(t) ||
          is.logical(t))) {
        return(suppressWarnings(!is.na(t)));
      }
    return(TRUE);
    }
  }
  return(FALSE);
}

#' @title Check whether Something is a non-empty string
#' @description  Check whether an object is a non-empty string.
#' @param t the object to check
#' @return \code{TRUE} if neither \code{is.na(t)} nor \code{is.null(t)} holds
#'   and \code{is.character(t)} and \code{length(t) > 0L} hold, \code{FALSE}
#'   otherwise
#' @export is.non.empty.string
is.non.empty.string <- function(t) {
  return((!(is.null(t))) &&
          (is.character(t) &&
          (length(t) == 1L) &&
          (!(is.na(t))) &&
            (nchar(t) > 0L)));
}
