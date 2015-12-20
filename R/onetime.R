
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
