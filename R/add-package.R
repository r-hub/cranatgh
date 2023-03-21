
#' Add or update a package in CRAN @@ GitHub
#'
#' The update is performed based on the current state of the
#' crandb database (See <https://github.com/metacran/crandb>.)
#'
#' All versions that are missing form the GitHub repository
#' at <https://github.com/cran>, are added.
#'
#' If the package is missing from the GitHub mirror completely,
#' then it is created.
#'
#' @param package Name of the package
#' @param reset Whether to remove and re-add all the commits.
#'   This is useful for commit errors, e.g. an older version of
#'   cranatgh failed to remove files from the repo that were
#'   deleted in the new package versions.
#' @return Invisible `TRUE` if the package was successfully updated,
#'   `FALSE` otherwise.
#'
#' @importFrom gh gh
#' @importFrom cli cli_h2 cli_alert_info no
#' @export

add_package <- function(package, reset = FALSE) {
  crandb_versions <- get_cran_versions(package)
  github_versions <- get_github_versions(package)

  cli_h2("Adding package {.pkg {package}}")
  if (nrow(crandb_versions) == 0) {
    warning("Package not in CRANDB")
    return()
  }

  missing_versions <- if (reset) {
    crandb_versions$version
  } else {
    setdiff(crandb_versions$version, github_versions)
  }
  cli_alert_info(c(
    "{.pkg {package}} has {no(length(missing_versions))} ",
    "missing version{?s./:/s:} {missing_versions}"
  ))

  ## Note that if reset == TRUE, then all versions are missing,
  ## but new_package will be still FALSE, because we don't need
  ## to create the GitHub repo
  add_missing_versions(
    package,
    missing_versions,
    new_package = length(github_versions) == 0,
    timeline = crandb_versions,
    reset = reset
  )
}


#' Add some (or all) versions of a package to the GitHub mirror
#'
#' @param versions Character vector, package versions to add.
#' @param new_package Logical scalar, whether the package is new. If
#'   the package is new, then its repo does not exists (yet) on GitHub.
#' @param timeline The full timeline of the package, from crandb, a two
#'   column data frame, with columns "version" and "date".
#' @inheritParams add_package
#'
#' @keywords internal

add_missing_versions <- function(package, versions, new_package,
                                 timeline, reset) {

  if (length(versions) == 0) return(invisible())

  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  change_to_cranatgh_home()

  if (new_package || reset) create_git_repo(package)

  if (!file.exists(package)) clone_git_repo(package)

  set_git_user(package)

  for (ver in versions) {
    ts <- timeline$date[match(ver, timeline$version)]
    metadata <- add_missing_version(package, ver, ts)
  }

  desc <- make_description(metadata)

  if (new_package) create_gh_repo(package, desc)

  push_to_github(package, forced_push = reset)

  if (!new_package) update_description(package, desc)

  invisible()
}

change_to_cranatgh_home <- function() {
  home <- default_tree_location()
  if (is.na(home)) dir.create(home <- tempfile())
  setwd(home)
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
#' @importFrom desc description
#' @importFrom utils untar
#' @importFrom cli cli_process_start cli_process_done

add_missing_version <- function(package, version, date) {

  proc <- cli_process_start("Adding {.pkg {package}} {version}")

  ## Rename the .git directory. We'll need it later
  file.rename(file.path(package, ".git"), "dot-git")

  ## Remove everything from the old version
  unlink(package, recursive = TRUE, force = TRUE)
  dir.create(package)

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

  ## To prevent an error like "detected dubious ownership in repository"
  if (.Platform$OS.type == "unix") {
    system("chown -R `id -un`:`id -gn` .")
  }

  ## Add all the new files
  git("status")
  git("add", "-A", ".")
  git("status")

  ## Package information from DESCRIPTION
  metadata <- description$new()
  maint <- metadata$get_maintainer()
  auth <- metadata$get("Author")

  ## Commit the new version
  git(
    env = c("GIT_COMMITTER_DATE" = date),
    "commit",
    "--allow-empty",
    "-m", paste0("version ", version),
    "--date", date,
    "--author", fix_maintainer(maint, auth)
  )

  git("tag", version)

  cli_process_done(proc)

  metadata
}
