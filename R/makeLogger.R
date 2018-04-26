
# create a logger string from a set of strings
.stringer <- function(...) {
  paste(Sys.time(), ": ",
        paste(..., sep="", collapse=""),
        sep="", collapse="");
}

#' @title Create a Logger Function
#' @description Create a logger function which can be used to store status
#'   output either to a file or stdout. A logger function is a function which
#'   accepts any number of arguments via the ellipse parameter \code{...}. These
#'   arguments are concatenated via \code{\link{paste}(..., sep="",
#'   collapse="")} and printed to the destination. The destination might be a
#'   file or the console or whatever.
#' @param logging should progress information be printed: either \code{TRUE} for
#'   printing to the console via \code{\link{print}}, \code{FALSE} for no
#'   logging, or a path to a file receiving logging information
#' @param cores the number of processing cores. This is not really relevant
#'   itself, but (\code{>1L} will issue a warning for console-based logging.
#' @export makeLogger
makeLogger <- function(logging, cores=1L) {
  # check and creat the logging destination
  logging <- force(logging);
  if(is.character(logging)) {
    # logging is a path to a file, so we first try to normalize it
    logging <- normalizePath(logging, mustWork = FALSE);
    # and make sure the hosting directory exists
    dir.create(path=dirname(logging), showWarnings = FALSE, recursive = TRUE);

    # now we can make sure that the log file exists
    if(!file.exists(logging)) {
      # if the file does not yet exist, create it
      file.create(logging, showWarnings = FALSE);
    }

    # which means that now we can really normalize the path
    logging <- normalizePath(logging);
    logging <- force(logging);

    # the logger is now a function writing to the file
    logger <- function(...) {
      logging <- force(logging);
      # open the file for appending text
      con <- file(logging, "at");
      # writing the time and the text
      writeLines(text=.stringer(...), con=con);
      # closing the file
      close(con);
      invisible(NULL); # return an invisible null
    }
  } else {
    if(isTRUE(logging)) {
      if((cores > 1L) && (Sys.getenv("RSTUDIO") == "1")) {
        warning(paste("Setting logging to TRUE with cores=",
                      cores, " will may crash/fail in RStudio.",
                      sep="", collapse=""))
      }
      logger <- function(...) {
        # print current time and text to stdout
        cat(.stringer(..., "\n"));
        invisible(NULL); # return invisible null
      }
    } else {
      if(is.function(logging)) {
        # if the logger is a function
        if(identical(names(formals(logging)), c("..."))) {
          # we return that function only if the arguments match
          return(logging);
        }
      }
      logger <- NULL;
    }
  }
  logger <- force(logger);

  return(logger);
}
