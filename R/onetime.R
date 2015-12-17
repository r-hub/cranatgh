
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
