
#' Create a GitHub description string for a package
#'
#' On GitHub the description cannot contain newline characters,
#' and HTML tags are not allowed, either. But it is possible to include
#' non-breaking space characters, and this results for a nice
#' multi-line description for most packages.
#'
#' @param pkg Package name, or a `description` object.
#' @return Character scalar, an appropriate GitHub repository
#'   description.
#'
#' @keywords internal

make_description <- function(pkg) {
  if (is.character(pkg)) {
    pkg <- tryCatch(
      pkgsearch::cran_package(pkg),
      error = function(e) NULL
    )

  } else if (inherits(pkg, "description")) {
    pkg <- list(
      Package = pkg$get("Package"),
      Title = pkg$get("Title"),
      URL = pkg$get("URL"),
      BugReports = pkg$get("BugReports")
    )

  } else {
    stop("'pkg' must be a character scalar or a 'description' object")
  }

  dsc <- paste(
    sep = "  ",
    dont_break(":exclamation: This is a read-only mirror of the CRAN R package repository."),
    dont_break(
      pkg$Package, " \u2014 ", pkg$Title,
      nullna_or(pkg$URL, paste0(". Homepage: ", pkg$URL))
    ),
    nullna_or(pkg$BugReports, dont_break("Report bugs for this package: ", pkg$BugReports))
  )

  # Limit is 350 characters, but be conservative
  if (nchar(dsc) > 320) dsc <- paste0(substr(dsc, 1, 320), " ...")

  dsc
}
