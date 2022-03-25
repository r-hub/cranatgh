
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
#' @param ... Input, it will be concatenated using [base::paste0()].
#' @return Character.
#'
#' @keywords internal

dont_break <- function(...) {
  gsub("\\s+", "\u00a0", paste0(...))
}

#' Empty string if input is `NULL`
#'
#' If `x` is `NULL`, then an empty string, otherwise the value of `expr`.
#'
#' @param x Condition.
#' @param expr Expression.
#' @return Expression or an empty string.
#'
#' @keywords internal

nullna_or <- function(x, expr) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) "" else expr
}

fix_maintainer <- function(maint, auth) {
  if (is.na(maint)) maint <- auth
  maint <- sub("\\s*<", " <", maint)

  ## ': end of single quote
  ## ": start of double quote
  ## ': single quote (within double quotes)
  ## ": end of double quote
  ## ': start of single quote for the rest of the string
  maint <- gsub("'", paste0("'", '"', "'", '"', "'"), maint)

  if (is.na(maint)) maint <- "??? <???@???>"
  if (!grepl("<.*>", maint)) maint <- paste0(maint, " <???@???>")
  if (toupper(maint) == "ORPHANED") maint <- "ORPHANED <cran@R-project.org>"
  maint
}
