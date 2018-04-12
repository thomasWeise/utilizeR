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

  # we want to avoid file names of the form "prefix__suffix" and translate them
  # to "prefix_suffix". Also we don't want "/a/_b" but rather "/a/b". Hence we
  # first check whether the file name suffix starts with a dodgy character.
  start     <- substr(suffix, 1L, 1L);
  start     <- ((start == ".") || (start == "_")  ||
                (start == "/") || (start == "\\") ||
                (start == ":"));
  start     <- force(start);

  # create the processor function
  f <- function(root, paths) {
    root      <- force(root);
    paths     <- force(paths);
    processor <- force(processor);
    dest      <- force(dest);
    suffix    <- force(suffix);
    start     <- force(start);

    # extract the common prefix of all source files
    prefix    <- string.commonPrefix(file_path_sans_ext(paths, compression=TRUE));

    use <- suffix;
    if(isTRUE(dir.exists(prefix))) {
      # Ok, the prefix identifies an directoy. Then, the final file is just
      # prefix/suffix, unless suffix starts with a dodgy character, in which
      # case we omit the first suffix character.
      if(start) { use <- substr(suffix, 2L, length(suffix)); }
      # Create the prefix/suffix path
      prefix <- file.path(prefix, use);
    } else {
      # If the prefix is of the form directory/xyz, where xyz is a common part
      # of a file name, we can add the suffix to the prefix. However, we want to
      # avoid doubling of characters like "_".
      if(start) {
        # The suffix starts with a dodgy character.
        lp <- length(prefix);
        if(lp > 0L) {
          # The prefix has non-zero length, so we check its last character.
          end <- substr(prefix, lp, lp);
          if((end == ".") || (end == "_")  ||
             (end == "/") || (end == "\\") ||
             (end == ":")) {
            # Both the last character of the prefix and the first character of
            # the suffix are dodgy. We omit the last character of the suffix,
            # since the first character of the suffix may be "." and we want
            # "abc_.txt" to become "abc.txt" rather than "abc_txt".
            prefix <- substr(prefix, 1L, (lp - 1L));
          }
        } else {
          # Oddly, there is no prefix, just a suffix starting with a dodgy
          # character, so we omit that first char.
          use <- substr(suffix, 2L, length(suffix));
        }
      }
      # Combine the prefix and the suffix.
      prefix <- paste(prefix, suffix, sep="", collapse="");
    }

    # The final path is the concatenation of the relative path of the prefix
    # towards the root dir appended to the destination dir.
    destination  <- file.path(dest,
                    path.relativize(path=prefix, dir=root));

    # We ensure that the directory where destination file should be put exists.
    dir.create(path=dirname(destination), showWarnings=FALSE, recursive=TRUE);

    # Now we can invoke the processor and pass in the path(s) as well as the
    # destination file.
    processor(paths, destination);
  }

  f <- force(f);
  return(f);
}
