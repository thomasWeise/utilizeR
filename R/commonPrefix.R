#' @title Find the Longest Common Prefix of a Set of Strings
#' @description For a set \code{strings} of strings, this method returns the
#'   longest common prefix. Inspired by
#'   \url{http://stackoverflow.com/questions/27470481/} and
#'   \url{http://stackoverflow.com/questions/26285010}
#' @param strings the strings
#' @return the longest common prefix string
#' @export string.commonPrefix
string.commonPrefix <- function(strings) {
  len <- length(strings);

  if(len <= 0L) { return(""); }
  if(len <= 1L) { return(strings[1L]); }

  # the length of the shortest string
  n <- min(nchar(strings))
  if(n <= 0L) { return(""); }

  # get the vectors of integers representing characters of the same length n
  vectors <- lapply(X=strings,
                    FUN=function(s) charToRaw(s)[1L:n]);

  # get the first converted string
  a <- vectors[[1L]];

  # compare it to all others
  vectors <- do.call(cbind, lapply(vectors[-1], function(x) {x == a}));

  # find the earliest disagreement
  for(i in 1L:n) {
    if(!all(vectors[i,])) { i <- i - 1L; break;}
  }

  # the answer
  if(i <= 0L) { return(""); }
  rawToChar(a[1L:i])
}
