
#' Update all (active) CRAN packages
#'
#' @return A list of results. For successful updates \code{TRUE}.
#'   For unsuccessful ones, a \code{try-error} object.
#'
#' @export

add_all_packages <- function() {
  pkgs <- get_all_cran_packages()

  res <- structure(
    replicate(length(pkgs), NULL),
    names = pkgs
  )

  for (i in seq_along(pkgs)) {
    cat(pkgs[[i]], "\n")
    res[[i]] <- try(add_package(pkgs[[i]]), silent = TRUE)
  }

  res
}

#' Update the repository description of some packages
#'
#' @param pkgs Packages to update. By default all active CRAN
#'   packages are updated.
#' @param sleep Numeric scalar, the amount of time to wait between
#'   GitHub API calls, in seconds. Defaults to one second.
#' @return A list of results. For successful updates the
#'   response of the GitHub API call. For unsuccessful ones,
#'   a \code{try-error} object.
#'
#' @export

update_all_descriptions <- function(pkgs = get_all_cran_packages(),
                                    sleep = 1) {

  res <- structure(
    replicate(length(pkgs), NULL),
    names = pkgs
  )

  for (i in seq_along(pkgs)) {
    cat(pkgs[[i]], "\n")
    res[[i]] <- try(update_description(pkgs[[i]]), silent = TRUE)
    Sys.sleep(sleep)
  }

  res
}
