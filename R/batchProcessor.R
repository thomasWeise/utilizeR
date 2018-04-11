#' @title Create a Batch Processor
#' @description Create a batch processor function for
#'   \code{\link{path.batchApply}} by mapping a function \code{processor} of
#'   type \code{function(path(s), dest)} to a function \code{function(root,
#'   path(s))} for a given basic destination directory \code{dest} and adding a
#'   \code{suffix} to a given set of paths.
#' @param processor the \code{function(path(s), dest)} to be applied
#' @param dest the destination folder
#' @param suffix the suffix
#' @return the batch processor function of type \code{function(path(s), dest)}
#' @export path.batchProcessor
#' @importFrom tools file_path_sans_ext
path.batchProcessor <- function(processor, dest, suffix) {
  processor <- force(processor);
  dest      <- force(dest);
  suffix    <- force(suffix);

  f <- function(root, paths) {
    root      <- force(root);
    paths     <- force(paths);
    processor <- force(processor);
    dest      <- force(dest);
    suffix    <- force(suffix);

    prefix    <- string.commonPrefix(file_path_sans_ext(paths, compression=TRUE));

    if(isTRUE(dir.exists(prefix))) {
      prefix <- file.path(prefix, suffix);
    } else {
      prefix <- paste(prefix, suffix, sep="", collapse="");
    }

    destination  <- file.path(dest,
                    path.relativize(path=prefix, dir=root));
    dir.create(path=dirname(destination), showWarnings=FALSE, recursive=TRUE);

    processor(paths, destination);
  }

  f <- force(f);
  return(f);
}
