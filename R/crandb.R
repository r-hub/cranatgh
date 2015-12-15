
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
