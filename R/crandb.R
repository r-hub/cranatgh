
#' @importFrom crandb package

get_crandb_timeline <- function(package) {
  crandb_versions <- tryCatch(
    package(package, "all"),
    error = function(e) NULL
  )
  crandb_versions$timeline
}


#' @importFrom crandb package

get_crandb_versions <- function(package) {
  crandb_versions <- tryCatch(
    package(package, "all"),
    error = function(e) NULL
  )
  names(crandb_versions$timeline)
}

#' @importFrom crandb list_packages

get_all_cran_packages <- function() {
  names(list_packages(format = "short", limit = 1000000L))
}

#' @importFrom crandb package

make_description <- function(pkg) {
  if (is.character(pkg)) {
    pkg <- tryCatch(
      package(package),
      error = function(e) NULL
    )
  }

  paste(
    sep = "  ",
    dont_break(":exclamation: This is a read-only mirror of the CRAN R package repository."),
    dont_break(
      package, " \u2014 ", pkg$Title,
      null_or(pkg$URL, paste0(". Homepage: ", pkg$URL))
    ),
    null_or(pkg$BugReports, dont_break("Report bugs for this package: ", pkg$BugReports))
  )
}

cran_url <- function(package) {
  paste0("http://cran.r-project.org/web/packages/", package)
}

metacran_url <- function(package) {
  paste0("http://www.r-pkg.org/pkg/", package)
}
