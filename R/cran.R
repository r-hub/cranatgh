
archive_cache <- local({
  cac <- NULL
  function() {
    if (is.null(cac)) {
      cac <<- pkgcache::cran_archive_cache$new(cran_mirror = cran_mirror)
    }
    cac
  }
})

metadata_cache <- local({
  cmt <- NULL
  function() {
    if (is.null(cmt)) {
      cmt <<- pkgcache::cranlike_metadata_cache$new(
        platforms = "source",
        bioc = FALSE
      )
    }
    cmt
  }
})

get_cran_versions <- function(package) {
  archive <- archive_cache()$list(packages = package)
  current <- metadata_cache()$list(packages = package)

  tb <- data.frame(
    stringsAsFactors = FALSE,
    date = c(archive$mtime, current$published),
    version = c(archive$version, current$version)
  )

  # some old version strings have an extra date after a space, e.g.
  # lme 3.0-0 (1999/06/26)
  # remove everything after a space
  tb$version <- sub(" .*$", "", tb$version)
  tb
}

#' Get the names and version numbers of all CRAN packages
#'
#' Including archived ones.
#'
#' @importFrom httr GET stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom utils tail
#' @importFrom stats na.omit
#' @return Character vector of all package names.
#'
#' @keywords internal

get_all_cran_packages <- function() {
  archive <- archive_cache()$list()
  current <- metadata_cache()$list()

  pkgs <- data.frame(
    stringsAsFactore = FALSE,
    package = c(archive$package, current$package),
    version = c(archive$version, current$version)
  )

  drev <- function(x) {
    x[rev(seq_len(nrow(x))), ]
  }

  # Keep the last version for every package
  # GitHub is case sensitive, so we merge packages that differ by case only.
  pkgs <- drev(pkgs)
  pkgs <- pkgs[!duplicated(tolower(pkgs$package)), ]
  pkgs <- drev(pkgs)
  pkgs
}
