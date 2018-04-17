# R Package with Shared Utility Routines

[<img alt="Travis CI Build Status" src="https://img.shields.io/travis/thomasWeise/utilizeR/master.svg" height="20"/>](https://travis-ci.org/thomasWeise/utilizeR/)

## Introduction

Here we store some utilities shared by our other packages.

One important export is the function `ignoreErrors(exp)` which executes the expression and ignores all errors or warnings.
This is useful when dealing with expressions that maybe assign some result (like `ignoreErrors(result <- dodgyFunction())`) but may fail for reasons which can safely be ignored.

Another useful utility is the `path.batchApply` which follows a visitor design pattern to (recursively) feed a supplied function with all files in a directory that fit to an (also supplied) regular expression.
The result is returned as a list, much like the `lapply`, if wanted.
`path.batchApply` can also automatically parallelize the computation by using `mclapply` from the `parallel` package internally.
This allows for a more convenient and automatic batch processing of data in folders.
`path.batchProcessor` can create a wrapper for an input/output-file-style processor to adapt it to the `path.batchApply` interface.
The other functions like `path.commonPrefix`, `path.extensionRegExp`, and `path.relativize` also are building blocks that are used by and fit to the `path.batchApply` code.

This library also includes some shared classes, such as `functionOrNULL` and `numericOrNULL`, which we use in some of our other classes to allow for members which can either be `NULL` or, well, a function or numeric vector.
    
## Installation

You can install the package directl from GitHub by using the package
[`devtools`](http://cran.r-project.org/web/packages/devtools/index.html) as
follows:

    library(devtools)
    install_github("thomasWeise/utilizeR")

If `devtools` is not yet installed on your machine, you need to FIRST do

    install.packages("devtools")
    
If you get the error `Installation failed: NULL : 'rcmd_safe_env' is not an exported object from 'namespace:callr'` during the installation attempt, please do the following:

1. Install the package `callr` by typing `install.packages("callr")` in the `R` console and hitting enter.
2. Exit your `R` session and start `R` again.
3. Try the installation of `utilizeR` again as described above.

## License

The copyright holder of this package is Prof. Dr. Thomas Weise (see Contact).
The package is licensed under the  GNU LESSER GENERAL PUBLIC LICENSE Version 3, 29 June 2007.

## Contact

If you have any questions or suggestions, please contact
[Prof. Dr. Thomas Weise](http://iao.hfuu.edu.cn/team/director) of the
[Institute of Applied Optimization](http://iao.hfuu.edu.cn/) at
[Hefei University](http://www.hfuu.edu.cn) in
Hefei, Anhui, China via
email to [tweise@hfuu.edu.cn](mailto:tweise@hfuu.edu.cn).
