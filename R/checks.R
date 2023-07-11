
#' Check if the CRAN@GH mirror of a package is up to date
#'
#' It returns a character vector of the missing version numbers. If
#' a package is up to date, then an empty character vector is returned.
#'
#' @param package Name of the package.
#' @return Character vector.
#'
#' @export

check_package <- function(package) {
  crandb_versions <- get_cran_versions(package)
  github_versions <- get_github_versions(package)

  setdiff(crandb_versions$version, github_versions)
}

get_all_github_packages <- function() {
  status <- read_status_data()
  data.frame(
    stringsAsFactors = FALSE,
    package = names(status),
    version = unname(unclass(status))
  )
}


#' Check if all packages are up to date on the CRAN@GH mirror
#'
#' @return A data frame with columns `package`, `version_cran`,
#' `version_github`, `status`. If a package is missing from CRAN/GitHub,
#' then its version is `NA`. Possible values of `status` are
#' * `"ok"`: mirror is current,
#' * `"removed"`: still on the mirror, but it was removed from CRAN,
#' * `"missing"`: missing from the mirror entirely,
#' * `"outdated"`: outdated on the mirror.
#'
#' @export

get_mirror_status <- function() {
  cr <- get_all_cran_packages()
  gh <- get_all_github_packages()

  pkgs <- merge(
    cr,
    gh,
    by = "package",
    all = TRUE,
    suffixes = c("_cran", "_github")
  )

  pkgs$status <- NA_character_
  pkgs$status[!is.na(pkgs$version_github) & !is.na(pkgs$version_cran) &
              pkgs$version_cran == pkgs$version_github] <- "ok"
  pkgs$status[is.na(pkgs$version_cran)] <- "removed"
  pkgs$status[is.na(pkgs$version_github)] <- "missing"
  pkgs$status[!is.na(pkgs$version_github) & !is.na(pkgs$version_cran) &
              pkgs$version_cran != pkgs$version_github] <- "outdated"

  pkgs
}

#' Update all missing and outdated packages
#'
#' @param max_failures Maximum number of failures. Many failures will
#' cause the updates to stop.
#' @param update_cache_after Update cache after this many iterations.
#' @param error_on_failure Whether to error if an update fails.
#' @return Data frame, similar to the return value of [get_mirror_status()],
#' with the extra `result` column, which is `"success"` for successful,
#' `"failure"` for unsuccessful updated, and `NA` otherwise.
#'
#' @export
#' @importFrom cli cli_alert_success no cli_verbatim cli_alert_danger

update_all_packages <- function(max_failures = 10, update_cache_after = 100,
                                error_on_failure = TRUE) {
  status <- get_mirror_status()

  upd <- status$package[status$status %in% c("missing", "outdated")]
  cli_alert_info("Need to update {no(length(upd))} package{?s}.")

  failures <- 0L
  batch <- 0L
  status$result <- NA_character_
  for (pkg in upd) {
    if (failures > max_failures) {
      update_status_cache(status)
      stop("Too many update failures, stopping")
    }
    if (batch == update_cache_after) {
      update_status_cache(status)
      batch <- 0L
    }
    tryCatch({
      add_package(pkg)
      cli_alert_success("Successfully updated {.pkg {pkg}}")
      status$result[match(pkg, status$package)] <- "success"
      batch <- batch + 1L
    }, error = function(e) {
      failures <<- failures + 1L
      cli_alert_danger("Failed to update {.pkg {pkg}}")
      cli_verbatim(conditionMessage(e))
      status$result[match(pkg, status$package)] <- "failure"
    })
  }

  if (length(upd)) {
    update_status_cache(status)
    cli_alert_info("Done with {length(upd)} update{?s}")
  }

  status <- status[!is.na(status$result), ]
  if (error_on_failure && any(status$result == "failure")) {
    stop("Failed to update ", sum(status$result == "failue"), " packages")
  }
  status
}

#' Set up the features of the GitHub repos
#'
#' Including:
#' * set repo description,
#' * set repo link,
#' * disable issues,
#' * disable wiki,
#' * disable project.
#'
#' @param pkgs Packages to update. Defaults to all packages.
#'
#' @export

setup_repo_features <- function(pkgs = NULL) {
  # TODO
  stop("Not implemented yet")
}

#' @importFrom yaml write_yaml

write_status_data <- function(status, path) {
  stopifnot(inherits(status, "cranatgh_status"))
  if (isTRUE(file.info(path)$isdir)) {
    path <- file.path(path, "cranatgh-status.yml")
  }
  status <- as.list(status)
  write_yaml(status, path)
}

#' @importFrom yaml read_yaml

read_status_data <- function(path = default_status_url()) {
  if (! grepl("^https?://", path) && isTRUE(file.info(path)$isdir)) {
    path <- file.path(path, "cranatgh-status.yml")
  }
  status <- read_yaml(path)
  status <- unlist(status)
  class(status) <- "cranatgh_status"
  status
}
