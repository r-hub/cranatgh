
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

  withr::with_dir(tmpdir, expr)
}

get_random_id <- function() {
  paste(sample(letters, 6, replace = TRUE), collapse = "")
}

crayon_timeline <- function() {
  tibble::tibble(
    date = c(
      "2014-09-23T22:19:16+00:00",
      "2014-10-15T17:51:51+00:00",
      "2015-04-09T00:30:34+00:00"),
    version = c("1.0.0", "1.1.0", "1.2.0")
  )
}
