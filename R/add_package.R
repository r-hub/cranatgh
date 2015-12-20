
#' Add or update a package in CRAN @@ GitHub
#'
#' @param package Name of the package
#' @return Invisible `TRUE` if the package was successfully updated,
#'   `FALSE` otherwise.
#'
#' @importFrom gh gh
#' @export

add_package <- function(package) {

  crandb_timeline <- get_crandb_timeline(package)
  crandb_versions <- setdiff(names(crandb_timeline), "archived")
  github_versions <- get_github_versions(package)

  if (length(crandb_versions) == 0) {
    stop("Package not in CRANDB, how is this possible?")
  }

  missing_versions <- setdiff(crandb_versions, github_versions)

  with_tempdir(
    {
      add_missing_versions(
        package,
        missing_versions,
        new_package = length(github_versions) == 0,
        timeline = crandb_timeline
      )
    }
  )

  invisible(TRUE)
}


add_missing_versions <- function(package, versions, new_package, timeline) {

  if (length(versions) == 0) return()

  if (new_package) {
    create_git_repo(package)
  } else {
    clone_git_repo(package)
  }

  for (ver in versions) {
    metadata <- add_missing_version(package, ver, timeline[[ver]])
  }

  if (new_package) create_gh_repo(package, metadata)

  push_to_github(package)

  if (!new_package) update_description(package, metadata)
}


#' @importFrom description description
#' @importFrom utils untar

add_missing_version <- function(package, version, date) {

  file.rename(file.path(package, ".git"), "dot-git")

  tar_file <- get_package_tarball(package, version)
  untar(tar_file)
  unlink(tar_file)

  unlink(file.path(package, ".git"))
  file.rename("dot-git", file.path(package, ".git"))

  setwd(package)
  on.exit(setwd(".."), add = TRUE)

  git("status")
  git("add", "-A", ".")
  git("status")

  metadata <- description$new()

  git(
    env = paste0("GIT_COMMITTER_DATE='", date, "'"),
    "commit",
    "--allow-empty",
    "-m", paste0("'version ", version, "'"),
    "--date", date,
    "--author", paste0("'", metadata$get_maintainer(), "'")
  )

  git("tag", version)

  metadata
}
