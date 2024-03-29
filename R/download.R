
#' Possible download URLs for CRAN packages
#'
#' @param package Package name.
#' @param version Package version.
#' @return Character vector of URLs.
#'
#' @keywords internal

package_urls <- function(package, version) {
  # work around some mistakes, file names do not match the version number
  if (package == "HTML" && version == "0.4") {
    "https://cran.rstudio.com/src/contrib/Archive/HTML/HTML_0.4-1.tar.gz"

  } else if (package == "timeslab" && version == "1.0") {
    "https://cran.r-project.org/src/contrib/Archive/timeslab/timeslab_1.0-1.tar.gz"

  } else {
    c(sprintf("%s/src/contrib/%s_%s.tar.gz", cran_mirror, package, version),
      sprintf("%s/src/contrib/Archive/%s/%s_%s.tar.gz", cran_mirror,
              package, package, version),
      sprintf("%s/src/contrib/4.4.0/Recommended/%s_%s.tar.gz", cran_mirror, package, version)
    )
  }
}

#' Download CRAN source package into the current directory
#'
#' It puts the downloaded tarball into the current working directory.
#'
#' @param package Package name.
#' @param version Package version.
#' @return Name of the downloaded file
#'
#' @keywords internal

get_package_tarball <- function(package, version) {
  urls <- package_urls(package, version)
  for (url in urls) {
    dest_file <- basename(url)
    if (res <- try_download(url, dest_file)) break
  }

  if (!res) stop("Cannot download package ", package)
  dest_file
}

#' Try a package download from CRAN
#'
#' It tries to write the file directly on disk.
#' Unsuccessful downloads result an error.
#'
#' @param url Download url
#' @param dest_file Destination file
#' @return Logical scalar, whether the download was successfull.
#'
#' @keywords internal
#' @importFrom httr GET write_disk status_code

try_download <- function(url, dest_file) {
  if (file.exists(dest_file)) return(TRUE)
  resp <- GET(url, write_disk(dest_file))
  if (status_code(resp) != 200) {
    unlink(dest_file)
    FALSE
  } else {
    TRUE
  }
}
