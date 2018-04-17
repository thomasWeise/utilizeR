#' @title Create a Logger Function
#' @description Create a logger function which can be used to store status
#'   output either to a file or stdout.
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
    file.create(logging, showWarnings = FALSE);
    # which means that now we can really normalize the path
    logging <- normalizePath(logging);
    logging <- force(logging);

    # the logger is now a function writing to the file
    logger <- function(string) {
      logging <- force(logging);
      # open the file for appending text
      con <- file(logging, "at");
      # writing the time and the text
      writeLines(text=paste(Sys.time(), ": ", string, sep="", collapse=""), con=con);
      # closing the file
      close(con);
      return(NULL); # return null
    }
  } else {
    if(isTRUE(logging)) {
      if(cores > 1L) {
        warning(paste("Setting logging to TRUE with cores=",
                      cores, " will may crash/fail in RStudio.",
                      sep="", collapse=""))
      }
      logger <- function(string) {
        # print current time and text to stdout
        print(paste(Sys.time(), ": ", string));
        return(NULL); # return null
      }
    } else {
      if(is.function(logging)) {
        # if the logger is a function
        if(identical(names(formals(logging)), c("string"))) {
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
