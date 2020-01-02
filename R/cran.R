
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
