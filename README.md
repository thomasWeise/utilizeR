# R Package with Shared Utility Routines

[<img alt="Travis CI Build Status" src="https://img.shields.io/travis/thomasWeise/utilizeR/master.svg" height="20"/>](https://travis-ci.org/thomasWeise/utilizeR/)

## Introduction

Here we store some utilities shared by our other packages.
    
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
