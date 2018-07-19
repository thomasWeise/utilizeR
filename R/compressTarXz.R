#' @title Compress a File or Directory Using the System's Own .tar.xz
#'   Installation
#' @description Use the \code{tar} command installation of the operating system
#'   to compress a file or folder to a tar.xz archive of minimal size.
#' @param sourcePath the path to the file or folder to compress
#' @param targetPath the path to the archive to create
#' @param logging the logger to use
#' @return the path to the newly created archive, or \code{NULL} on failure
#' @include makeLogger.R
#' @export compress.tar.xz
compress.tar.xz <- function(sourcePath,
                            targetPath=NULL,
                            logging=TRUE) {
  logging <- makeLogger(logging);

  sourcePath   <- normalizePath(sourcePath, mustWork = TRUE);
  sourceFolder <- normalizePath(dirname(sourcePath), mustWork = TRUE);
  sourceName   <- basename(sourcePath);

  doCreate <- FALSE;
  if(is.null(targetPath)) {
    targetFolder <- sourceFolder;
    targetName   <- paste(sourceName, ".tar.xz", sep="", collapse="");
    targetPath   <- suppressWarnings(normalizePath(file.path(targetFolder, targetName), mustWork = FALSE));
  } else {
    targetPath <- suppressWarnings(normalizePath(targetPath, mustWork = FALSE));
    if(dir.exists(targetPath)) {
      targetFolder <- targetPath;
      targetName   <- paste(sourceName, ".tar.xz", sep="", collapse="");
      targetPath   <- suppressWarnings(normalizePath(file.path(targetFolder, targetName), mustWork = FALSE));
    } else {
      targetName   <- basename(targetPath);
      targetFolder <- suppressWarnings(normalizePath(dirname(targetPath), mustWork = FALSE));
      doCreate     <- TRUE;
    }
  }

  tar <- Sys.which("tar");
  if( is.null(tar) ||
     (length(tar) != 1L) ||
      is.null(tar[[1L]]) ||
     (nchar(tar[[1L]]) <= 0L) ||
     (!(file.exists(tar[[1L]])))) {
    if(!(is.null(logging))) {
      logging("No tar executable found to compress '",
              sourcePath, "' to '", targetPath, "'.");
    }
    return(NULL);
  }

  tar <- tar[[1L]];

  if(!(is.null(logging))) {
    logging("Compressing source '",
            sourcePath,
            "' to archive '", targetPath,
            "' using tar executable '", tar, "'.");
  }

  if(doCreate) {
    dir.create(path=targetFolder, showWarnings = FALSE, recursive = TRUE);
  }

  wd <- getwd();
  setwd(sourceFolder);
  ret <- system2(command=tar,
                 args=c("-cJf",
                        targetPath,
                        sourceName),
                 env="XZ_OPT=-9e",
                 wait=TRUE,
                 stdout=FALSE,
                 stderr=FALSE);
  setwd(wd);

  if(!(is.null(logging))) {
    if(ret == 0) {
      logging("Succesfully compressed '", sourcePath, "' to archive '", targetPath, "'.");
    } else {
      logging("Failed to compress '", sourcePath, "' to archive '", targetPath, "', return code is ", ret, ".");
    }
  }

  if(ret == 0) {
    return(targetPath);
  }
  return(NULL);
}
