#' @title Either a \code{function} or \code{NULL}
#' @description A combination of the \code{function} class and \code{NULL}.
#'   Using this class allows us to have member variables in S4 classes which are
#'   null-able functions.
#' @importFrom methods setClassUnion
#' @exportClass functionOrNULL
setClassUnion("functionOrNULL", c("function","NULL"))

#' @title Either a \code{numeric} \code{vector} or \code{NULL}
#' @description A combination of the \code{numeric} class and \code{NULL}. Using
#'   this class allows us to have member variables in S4 classes which are
#'   null-able numeric vectors.
#' @importFrom methods setClassUnion
#' @exportClass numericOrNULL
setClassUnion("numericOrNULL", c("numeric","NULL"))

#' @title Either a \code{character} \code{vector} or \code{NULL}
#' @description A combination of the \code{character} class and \code{NULL}.
#'   Using this class allows us to have member variables in S4 classes which are
#'   null-able character vectors.
#' @importFrom methods setClassUnion
#' @exportClass characterOrNULL
setClassUnion("characterOrNULL", c("character","NULL"))
