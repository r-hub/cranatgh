
#' Get the timeline of a package from crandb
#'
#' @param package Package name.
#' @return The timeline, in a named list, where names are
#'   package versions, and entries are dates. If the package is not
#'   in crandb, \code{NULL} is returned. A special entry named
#'   \code{archived} is included for archived packages.
#'
#' @keywords internal
#' @importFrom crandb package

get_crandb_timeline <- function(package) {
  crandb_versions <- tryCatch(
    package(package, "all"),
    error = function(e) NULL
  )
  crandb_versions$timeline
}

#' Get all versions of a package from crandb
#'
#' @param package Package name.
#' @return Character vector of package versions. If the package is not
#'   in crandb, \code{NULL} is returned.
#'
#' @keywords internal
#' @importFrom crandb package

get_crandb_versions <- function(package) {
  crandb_versions <- tryCatch(
    package(package, "all"),
    error = function(e) NULL
  )
  names(crandb_versions$timeline)
}

#' All active packages on CRAN
#'
#' @return Character vector of all active CRAN packages.
#'
#' @keywords internal
#' @importFrom crandb list_packages

get_all_cran_packages <- function() {
  names(list_packages(format = "short", limit = 1000000L))
}

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
#' @importFrom crandb package

make_description <- function(pkg) {
  if (is.character(pkg)) {
    pkg <- tryCatch(
      package(pkg),
      error = function(e) NULL
    )

  } else if (is(pkg, "description")) {
    pkg <- list(
      Package = pkg$get("Package"),
      Title = pkg$get("Title"),
      URL = pkg$get("URL"),
      BugReports = pkg$get("BugReports")
    )

  } else {
    stop("'pkg' must be a character scalar or a 'description' object")
  }

  paste(
    sep = "  ",
    dont_break(":exclamation: This is a read-only mirror of the CRAN R package repository."),
    dont_break(
      pkg$Package, " \u2014 ", pkg$Title,
      null_or(pkg$URL, paste0(". Homepage: ", pkg$URL))
    ),
    null_or(pkg$BugReports, dont_break("Report bugs for this package: ", pkg$BugReports))
  )
}

#' Get the URL of a CRAN package
#'
#' @param package Package name.
#' @return Character scalar, the URL.
#'
#' @keywords internal

cran_url <- function(package) {
  paste0("http://cran.r-project.org/web/packages/", package)
}

#' Get the Metacran (\url{http://www.r-pkg.org} URL of a package
#'
#' @param package Package name.
#' @return The Metacran URL.
#'
#' @keywords internal

metacran_url <- function(package) {
  paste0("http://www.r-pkg.org/pkg/", package)
}
