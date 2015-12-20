
#' Run an expression in a temporary directory
#'
#' @param expr Expression to evaluate.
#' @param tmpdir Temporary directory to evaluate it in. By default
#'   a new temporary direcotory is created.
#' @param remove Whether to remove the temporary directory
#'   after the expression is evaluated.
#' @return Return value of the expression.
#'
#' @keywords internal

with_tempdir <- function(expr, tmpdir = tempfile(), remove = TRUE) {

  if (remove) on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  if (!file.exists(tmpdir)) dir.create(tmpdir)

  with_wd(tmpdir, expr)
}

#' Run an expression with a different working directory
#'
#' @param wd Temporary working directory to use.
#' @param expr Expression to evaluate.
#' @return Return value of the expression.
#'
#' @keywords internal

with_wd <- function(wd, expr) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(wd)
  expr
}

#' Empty sting from \code{NA}
#'
#' If \code{x} is a single \code{NA} (any mode),
#' and empty string is returned. Otherwise \code{x}
#' is returned.
#'
#' @param x Input.
#' @return \code{x} or and empty string.
#'
#' @keywords internal

na_to_empty <- function(x) {
  if (length(x) == 1 && is.na(x)) "" else x
}

#' Replace are whitespace with non-breakable space in a string
#'
#' @param ... Input, it will be concatenated using \code{paste0}.
#' @return Character.
#'
#' @keywords internal

dont_break <- function(...) {
  gsub("\\s+", "\u00a0", paste0(...))
}

#' Empty string if input is \code{NULL}.
#'
#' If \code{x} is \code{NULL}, then an empty string is returned,
#' otherwise \code{expr}.
#'
#' @param x Condition.
#' @param expr Expression.
#' @return Expression or an empty string.
#'
#' @keywords internal

null_or <- function(x, expr) {
  if (is.null(x)) "" else expr
}
