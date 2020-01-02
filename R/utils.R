
`%||%` <- function(l, r) if (is.null(l)) r else l

cran_mirror <- "https://cloud.r-project.org"

set_names <- function(x, nms) {
  stopifnot(
    length(x) == length(nms),
    is.character(nms),
    all(!is.na(nms)),
    all(nms != "")
  )
  names(x) <- nms
  x
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

nullna_or <- function(x, expr) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) "" else expr
}

fix_maintainer <- function(x) {
  x <- sub("\\s*<", " <", x)

  ## ': end of single quote
  ## ": start of double quote
  ## ': single quote (within double quotes)
  ## ": end of double quote
  ## ': start of single quote for the rest of the string
  x <- gsub("'", paste0("'", '"', "'", '"', "'"), x)

  if (toupper(x) == "ORPHANED") x <- "cran@R-project.org"
  x
}
