
get_cran_versions <- function(package) {
  tryCatch({
    set_names(
      pkgsearch::cran_package_history(package)[, c("date", "Version")],
      c("date", "version")
    )
  }, package_not_found_error = function(e) {
    tibble::tibble(date = character(), version = character())
  })
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
  packages <- get_packages_rds()
  archive <- get_archive_rds()
  archive <- archive[vapply(archive, NROW, integer(1)) > 0]

  pkgs <- unique(c(packages[, "Package"], names(archive)))
  cur_ver <- structure(packages[, "Version"], names = packages[, "Package"])
  old_ver <- na.omit(vapply(
    archive,
    function(x) {
      n <- basename(rownames(x))
      v <- sub("\\.tar\\.gz$", "", sub("^.*_", "", n))
      pv <- package_version(v, strict=FALSE)
      latest <- as.character(max(pv, na.rm = TRUE))
      c(latest, NA_character_)[1]
    },
    character(1)
  ))
  ver <- ifelse(pkgs %in% names(cur_ver), unname(cur_ver[pkgs]), unname(old_ver[pkgs]))
  df <- data.frame(
    stringsAsFactors = FALSE,
    package = pkgs,
    version = ver
  )

  df
}

get_packages_rds <- function() {
  # TODO: cache
  url <- "https://cloud.r-project.org/src/contrib/PACKAGES.rds"
  tmp <- tempfile()
  on.exit(unlink(tmp))
  utils::download.file(url, tmp, quiet = TRUE)
  readRDS(tmp)
}

get_archive_rds <- function() {
  # TODO: cache
  url <- "https://cloud.r-project.org/src/contrib/Meta/archive.rds"
  read_remote_rds(url)
}

read_remote_rds <- function(rds) {
  con <- url(rds)
  on.exit(close(con))
  readRDS(con)
}
