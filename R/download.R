
#' Possible download URLs for CRAN packages
#'
#' @param package Package name.
#' @param version Package version.
#' @return Character vector of URLs.

package_urls <- function(package, version) {
  c(sprintf("%s/src/contrib/%s_%s.tar.gz", cran_mirror, package, version),
    sprintf("%s/src/contrib/Archive/%s/%s_%s.tar.gz", cran_mirror,
            package, package, version))
}

#' Download CRAN source package into the current directory
#'
#' It puts the downloaded tarball into the current working directory.
#'
#' @param package Package name.
#' @param version Package version.
#' @return Name of the downloaded file

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
