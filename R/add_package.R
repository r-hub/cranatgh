
#' Add or update a package in CRAN @@ GitHub
#'
#' The update is performed based on the current state of the
#' crandb database (See \url{https://github.com/metacran/crandb}.)
#'
#' All versions that are missing form the GitHub repository
#' at \url{https://github.com/cran}, are added.
#'
#' If the package is missing from the GitHub mirror completely,
#' then it is created.
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

#' Add some (or all) versions of a package to the GitHub mirror
#'
#' @param package Name of the package to update.
#' @param versions Character vector, package versions to add.
#' @param new_package Logical scalar, whether the package is new. If
#'   the package is new, then its repo does not exists (yet) on GitHub.
#' @param timeline The full timeline of the package, from crandb.
#'
#' @keywords internal

add_missing_versions <- function(package, versions, new_package, timeline) {

  if (length(versions) == 0) return()

  if (new_package) {
    create_git_repo(package)
  } else {
    clone_git_repo(package)
  }

  set_git_user(package)

  for (ver in versions) {
    metadata <- add_missing_version(package, ver, timeline[[ver]])
  }

  if (new_package) create_gh_repo(package, make_description(metadata))

  push_to_github(package)

  if (!new_package) update_description(package, make_description(metadata))
}


#' Add a single missing version of a package to the GitHub mirror
#'
#' It assumes that the package's repository exists on GitHub,
#' and that the working directory contains a clone of of it.
#'
#' It then downloads the specified version from CRAN and adds it
#' to the local clone. Finally, it pushes the repo to GitHub.
#'
#' @param package Name of the package to update.
#' @param version Character scalar, the version to add.
#' @param date The exact date and time the version was built for CRAN.
#' @return The package's metadata, i.e. the contents of the `DESCRIPTION`
#'   file, in a `desciption` object, see the `description` package.
#'
#' @keywords internal
#' @importFrom description description
#' @importFrom utils untar

add_missing_version <- function(package, version, date) {

  ## Rename the .git directory. We'll need it later
  file.rename(file.path(package, ".git"), "dot-git")

  ## Remove everything from the old version
  ## TODO

  ## Put the new version in place
  tar_file <- get_package_tarball(package, version)
  untar(tar_file)
  unlink(tar_file)

  ## Put back the .git directory
  ## The unlink is for the occasional case when there is already
  ## a .git directory in the package. This is junk anyway, and it
  ## should not be there
  unlink(file.path(package, ".git"))
  file.rename("dot-git", file.path(package, ".git"))

  setwd(package)
  on.exit(setwd(".."), add = TRUE)

  ## Add all the new files
  git("status")
  git("add", "-A", ".")
  git("status")

  ## Package information from DESCRIPTION
  metadata <- description$new()

  ## Commit the new version
  git(
    env = paste0("GIT_COMMITTER_DATE='", date, "'"),
    "commit",
    "--allow-empty",
    "-m", paste0("'version ", version, "'"),
    "--date", date,
    "--author", paste0("'", fix_maintainer(metadata$get_maintainer()), "'")
  )

  git("tag", version)

  metadata
}
