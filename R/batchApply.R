#' @include extensionRegExp.R

#' @title Process The Files and Sub-Directories in a Directory Recursively (and
#' Potentially in Parallel)
#'
#' @description The sub-folders under a given path are iteratively processed.
#' For each folder, the function \code{check.directory} returns either
#' \code{TRUE} if we should investigate it (recursively) or \code{FALSE}, if
#' we should ignore it. By default we assume \code{TRUE}. Files in a folder
#' can either be processed one-by-one (via \code{file.single}) or all together
#' (via \code{file.in.folder}). Both \code{file.single} and
#' \code{file.in.folder} are environments. The names in these environments are
#' regular expressions which are matched against the files in the folders. The
#' values are handler methods of the type \code{function(root, path(s))}.
#' (\code{paths} for \code{file.in.folder}, \code{path} for
#' \code{file.single}). These handlers are applied to all the single files
#' (\code{file.single}) or the list of files in a folder
#' (\code{file.in.folder}) that match to their names. The names could, e.g.,
#' be regular expressions matching to file extensions created by
#' \code{\link{path.extensionRegExp}}.
#'
#' The processing can either take place sequentially (for values of
#' \code{core<=1L}) or in parallel (\code{cores>1L}). In the latter case, we
#' first gather all handler applications that need to be done and then pass
#' them to \code{\link[parallel]{mclapply}}.
#'
#' This function is not supposed to return anything useful. Instead, the idea
#' is that the handlers would read their input files and create output files
#' storing their result.
#'
#' @param path the path to explore recursively
#' @param file.single an environment or named list with file consumers of the
#'   form \code{function(root, path)}, where \code{root} will be the normalized
#'   root path (\code{path}) and \code{path} the path to a file whose name
#'   matches to the regular expression provided as name of consumer
#' @param file.in.folder an environment or named list with file consumers of the form
#'   \code{function(root, paths)}, where \code{root} will be the normalized root
#'   path (\code{path}) and \code{paths} is the array of the paths of all files
#'   in a folder whose name matches to the regular expression provided as name
#'   of consumer
#' @param check.directory a function of the form \code{function(root, path)}
#'   which returns \code{TRUE} if we should recurse into the directory
#'   \code{path} and \code{false} if not
#' @param cores the number of cores to use. If \code{cores <= 1L}, the consumers
#'   are applied sequentially. For \code{cores>1L}, we use the
#'   \code{\link[parallel]{mclapply}} function provided by the \code{parallel}
#'   package to invoke all the consumers with the specified number of cores
#' @param  logging the logging setup, see \code{\link{makeLogger}}
#' @return an unnamed and recursively unlisted vector of the results of all the
#'   processors, with no pre-defined order
#' @export path.batchApply
#' @importFrom parallel mclapply
#' @seealso path.extensionRegExp
path.batchApply <- function(path=getwd(),
                            file.single=emptyenv(),
                            file.in.folder=emptyenv(),
                            check.directory=NULL,
                            cores=1L,
                            logging=FALSE) {

  .root <- normalizePath(path);
  logging <- makeLogger(logging, cores);

  if(is.null(check.directory)) {
    check.directory <- function(root, path) TRUE;
  }
  check.directory <- force(check.directory);

  if((!(is.null(file.single)))) {
    file.single <- as.environment(file.single);
    file.single <- lapply(X=ls(file.single), FUN=function(pattern)
      .make.single.calls(pattern, get(x=pattern, pos=file.single)));
  }

  if((!(is.null(file.in.folder)))) {
    file.in.folder <- as.environment(file.in.folder);
    file.in.folder <- lapply(X=ls(file.in.folder),
                       FUN=function(pattern)
                         .make.in.folder.calls(pattern,
                                               get(x=pattern, pos=file.in.folder)));
  }

  make.calls <- unlist(c(file.in.folder, file.single), recursive = TRUE);

  if(cores > 1L) {
    if(!(is.null(logging))) {
      logging("beginning with parallel batch-processing of directory ", .root,
              " on ", cores, " cores - first we collect all jobs.");
    }
    calls <- unlist(.path.batchApply.par(root=.root,
                                         path=.root,
                                         make.calls=make.calls,
                                         check.directory=check.directory), recursive = TRUE);
    if(!(is.null(logging))) {
      logging("finished collecting ", length(calls),
              " jobs in total for directory  ", .root,
              ", now invoking them on ", cores,
              " cores.");
    }
   result <- mclapply(X=calls,
                      FUN=function(f) f(),
                      mc.cores=cores,
                      mc.preschedule=FALSE);
  } else {
    if(!(is.null(logging))) {
      logging("beginning sequential batch-processing of directory ", .root);
    }
    result <- .path.batchApply.seq(root=.root,
                                   path=.root,
                                   make.calls=make.calls,
                                   check.directory=check.directory);
  }

  result <- unname(unlist(result, recursive=TRUE));

  if(!(is.null(logging))) {
    logging("finished batch-processing directory ", .root,
            ", generated result list of length ", length(result),
            ".");
  }


  return(result);
}

# Create the functions that match file list towards regular expressions and return
# a list of function calls to be invoked for the "all" kind of processors.
.make.in.folder.calls <- function(pattern, consumer) {
  pattern  <- force(pattern);
  consumer <- force(consumer);
  f1 <- function(root, paths) {
    pattern  <- force(pattern);
    consumer <- force(consumer);
    paths    <- paths[vapply(X=paths,
                             FUN=function(file) { length(grep(pattern, file)) > 0L },
                             FUN.VALUE=FALSE)];
    if(length(paths) <= 0L) { return(NULL); }
    paths <- force(paths);
    root  <- force(root);
    f2    <- function() consumer(root, paths);
    f2    <- force(f2);
    f2    <- c(f2);
    f2    <- force(f2);
    return(f2);
  }
  f1 <- force(f1);
  return(f1);
}

# Create the functions that match file list towards regular expressions and
# return a list of function calls to be invoked for the "single" kind of
# processors.
.make.single.calls <- function(pattern, consumer) {
  pattern  <- force(pattern);
  consumer <- force(consumer);

  f1 <- function(root, paths) {
    pattern  <- force(pattern);
    all      <- force(all);
    consumer <- force(consumer);
    paths    <- paths[vapply(X=paths,
                      FUN=function(file) { length(grep(pattern, file)) > 0L },
                      FUN.VALUE=FALSE)];
    if(length(paths) <= 0L) { return(NULL); }
    paths <- force(paths);
    root  <- force(root);
    f2    <- lapply(X=paths, FUN=function(path) {
      path <- force(path);
      root <- force(root);
      f3 <- function() consumer(root, path);
      f3 <- force(f3);
      return(f3);
    });
    f2 <- force(f2);
    return(f2);
  }


  f1 <- force(f1);
  return(f1);
}


# Create the processors functions for parallel invocation, i.e.,
# build a list of function calls to invoke
.path.batchApply.par <- function(root,
                          path,
                          make.calls,
                          check.directory) {
  retval <- NULL;

  # can the path be processed?
  if(isTRUE(file.exists(path)) &&
     (!identical(path, ".") || identical(path, ".."))) {

    # ok, the path exists and is either a directory or file
    if(isTRUE(file.info(path)$isdir)) {

      # the path is a directory - but is it acceptable?
      if(check.directory(root, path)) { # yes

        # recursively apply the invocation to all sub-directories
        retval <- lapply(X=sort(list.dirs(path=path, full.names=TRUE, recursive=FALSE)),
                         FUN=function(path) {
                           ret <- .path.batchApply.par(root=root,
                                                       path=path,
                                                       make.calls=make.calls,
                                                       check.directory=check.directory);
                           ret <- force(ret);
                           return(ret);
                         });

        # now we want to create all the function invocations for the files
        # and append them to those obtained from the recursive call
        paths   <- sort(list.files(path=path, full.names=TRUE));
        paths   <- force(paths);
        retval2 <- lapply(X=make.calls, FUN=function(f) f(root, paths));

        # combine the return values
        if(is.null(retval) || (length(retval) <= 0L)) {
          retval <- retval2;
        } else {
          if((!(is.null(retval2))) && (length(retval2) > 0L)) {
            retval <- c(retval, retval2);
          }
        }
      }
    } else {
      # the path is a file
      paths <- c(path);
      paths <- force(paths);
      retval <- lapply(X=make.calls, FUN=function(f) f(root, paths));
    }
  }

  return(retval);
}

# perform a sequential call
.call.seq <- function(f, root, paths) {
  ret <- f(root, paths)
  if(is.null(ret) || (length(ret) <= 0L)) { return(NULL); }
  ret <- unlist(ret, recursive=TRUE);
  if(is.null(ret) || (length(ret) <= 0L)) { return(NULL); }
  return(lapply(X=ret, FUN=function(f) f()))
}

# Create the results via a sequential invocation
.path.batchApply.seq <- function(root,
                                 path,
                                 make.calls,
                                 check.directory) {
  retval <- NULL;

  # can the path be processed?
  if(isTRUE(file.exists(path)) &&
     (!identical(path, ".") || identical(path, ".."))) {

    # ok, the path exists and is either a directory or file
    if(isTRUE(file.info(path)$isdir)) {

      # the path is a directory - but is it acceptable?
      if(check.directory(root, path)) { # yes

        # recursively apply the invocation to all sub-directories
        retval <- lapply(X=sort(list.dirs(path=path, full.names=TRUE, recursive=FALSE)),
                         FUN=function(path) {
                           ret <- .path.batchApply.seq(root=root,
                                                       path=path,
                                                       make.calls=make.calls,
                                                       check.directory=check.directory);
                           ret <- force(ret);
                           return(ret);
                         });

        # now we want to create all the function invocations for the files
        # and append them to those obtained from the recursive call
        paths   <- sort(list.files(path=path, full.names=TRUE));
        paths   <- force(paths);
        # make the calls
        retval2 <- lapply(X=make.calls, FUN=.call.seq, root, paths);

        # combine the return values
        if(is.null(retval) || (length(retval) <= 0L)) {
          retval <- retval2;
        } else {
          if((!(is.null(retval2))) && (length(retval2) > 0L)) {
            retval <- c(retval, retval2);
          }
        }
      }
    } else {
      # the path is a file
      paths <- c(path);
      paths <- force(paths);
      retval <- lapply(X=make.calls, FUN=.call.seq, root, paths);
    }
  }

  return(retval);
}
