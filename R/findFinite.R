#' @title Try to Find a Finite Value for a function \code{f}
#' @description Given is a unary function \code{f} and a coordinate \code{x1} as
#'   well as a value \code{x2}. The general goal is to find an \code{x} value
#'   for which \code{f} becomes finite and which is as close as possible to
#'   \code{x1} from the range \code{min(x1,x2)...max(x1,x2)}. If \code{f(x1)} is
#'   finite, return \code{c(x1, f(x1)}. Otherwise, try to find a value \code{t}
#'   within the interval delimited by \code{x1} and \code{x2} for which \code{f}
#'   takes on a finite value, then return \code{c(t, f(t)}. If no such value can
#'   be discovered, return \code{c(x1, f(x1)}. This function will produce many
#'   warnings, see \code{\link{find.finite.nw}} for a function not producing
#'   warnings at the cost of lower speed.
#' @param x1 the \code{x} coordinate at which we want to evaluate \code{f} (and
#'   also the first interval limit)
#' @param x2 the second interval limit, should be either larger or smaller than
#'   \code{x1}
#' @param f the unary function
#' @return a tuple \code{x, f(x)}, which results from the attempt to find a
#'   finite function value
#' @seealso find.finite.nw
#' @export find.finite
find.finite <- function(x1, x2, f) {

  rx <- x1;
  ry <- f(rx);
  if(!is.finite(ry)) {

    # create 64 sample points first, using vector approach for speed-up
    xt <- x1 + (((1L:64L)/64) * (x2 - x1));
    yt <- f(xt);
    i  <- which(is.finite(yt));
    if(length(i) > 0L) {
      # ok, at least one element was finite, so let's pick this
      i  <- i[1L];
      rx <- xt[i];
      ry <- yt[i];
      x2 <- rx;
    }

    for(i in 1L:128L) {
      if(x1 == x2) { break; }
      xt <- 0.5*(x1 + x2);
      yt <- f(xt);
      if(is.finite(yt)) {
# if yt is finite for a point between x1 and x2, we can step closer towards x1
        ry <- yt;
        rx <- xt;
        x2 <- xt;
      } else {
# if yt is not finite, then we should stop closer towards x2
        x1 <- xt;
      }
    }
  }

  return(c(rx, ry)); # return result
}

#' @title Try to Find a Finite Value for a function \code{f}, and suppress
#'   warnings
#' @description Given is a unary function \code{f} and a coordinate \code{x1} as
#'   well as a value \code{x2}. The general goal is to find an \code{x} value
#'   for which \code{f} becomes finite and which is as close as possible to
#'   \code{x1} from the range \code{min(x1,x2)...max(x1,x2)}. If \code{f(x1)} is
#'   finite, return \code{c(x1, f(x1)}. Otherwise, try to find a value \code{t}
#'   within the interval delimited by \code{x1} and \code{x2} for which \code{f}
#'   takes on a finite value, then return \code{c(t, f(t)}. If no such value can
#'   be discovered, return \code{c(x1, f(x1)}. This function will produce no
#'   warnings, at the cost of slower performance (see
#'   \code{\link{find.finite}}).
#' @param x1 the \code{x} coordinate at which we want to evaluate \code{f} (and
#'   also the first interval limit)
#' @param x2 the second interval limit, should be either larger or smaller than
#'   \code{x1}
#' @param f the unary function
#' @return a tuple \code{x, f(x)}, which results from the attempt to find a
#'   finite function value
#' @seealso find.finite
#' @export find.finite.nw
find.finite.nw <- function(x1, x2, f) suppressWarnings(find.finite(x1, x2, f))
