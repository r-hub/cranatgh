
default_cranatgh_user <- function() {
  Sys.getenv("CRANATGH_USER", "cran-robot")
}

default_cranatgh_email <- function() {
  Sys.getenv("CRANATGH_EMAIL", "csardi.gabor+cran@gmail.com")
}

default_cranatgh_org <- function() {
  Sys.getenv("CRANATGH_ORG", "cran")
}

default_tree_location <- function() {
  Sys.getenv("CRANATGH_TREES", NA_character_)
}

default_local_mirror_directory <- function() {
  Sys.getenv("CRANATGH_LOCAL_CRAN_MIRROR", NA_character_)
}

default_cranatgh_repo <- function() {
  Sys.getenv("CRANATGH_DEFAULT_REPO", "https://github.com/r-hub/cranatgh")
}

default_status_url <- function() {
  Sys.getenv(
    "CRANATGH_STATUS_URL",
    "https://r-hub.github.io/cranatgh/cranatgh-status.yml"
  )
}

#' Get the token to be used for GitHub API calls
#'
#' It is taken from the `CRANATGH_GITHUB_TOKEN`,
#' `GITHUB_PAT` or `GITHUB_TOKEN` environment variables, whichever is
#' found first.
#'
#' @return Character scalar, the token, or `NA` if the
#' environment variable mentioned above are not set.
#'
#' @keywords internal

get_gh_token <- function() {
  token <- Sys.getenv("CRANATGH_GITHUB_TOKEN", NA_character_)
  if (is.na(token)) token <- Sys.getenv("GITHUB_PAT", NA_character_)
  if (is.na(token)) token <- Sys.getenv("GITHUB_TOKEN", NA_character_)
  token
}

#' The clone URL of a package at GitHub
#'
#' It is always an HTTPS URL currently.
#'
#' @param package Name of the package.
#' @return The url.
#'
#' @keywords internal

get_clone_url <- function(package) {

  owner <- default_cranatgh_org()
  token <- get_gh_token()
  token <- if (is.na(token)) "" else paste0(token, "@")

  sprintf("https://%sgithub.com/%s/%s.git", token, owner, package)
}

#' Get all versions of a package in the GitHub mirror
#'
#' @param package Package name.
#' @return Character vector of version numbers.
#'
#' @keywords internal

get_github_versions <- function(package) {
  github_versions <- tryCatch(
    gh(
      "/repos/:owner/:repo/tags",
      owner = "cran",
      repo = package,
      .limit = Inf,
      .token = get_gh_token()
    ),
    error = function(e) list(),
    warning = function(e) list()
  )
  github_versions <- vapply(github_versions, "[[", FUN.VALUE = "", "name")
  github_versions <- grep("R-", github_versions, value = TRUE, invert = TRUE)

  rev(github_versions)
}

#' Clone a repository from the GitHub CRAN mirror
#'
#' @param package Package name.
#' @return Output of the command line git call.
#'
#' @importFrom cli cli_alert_info
#' @keywords internal

clone_git_repo <- function(package) {
  url <- get_clone_url(package)
  proc <- cli_process_start("Cloning GitHub repo from {.url {safe_url(url)}}")
  git("clone", url)
  cli_process_done(proc)
}

safe_url <- function(url) {
  sub("://[-:a-z0-9]+@", "://<token>@", url)
}

#' Push the package to GitHub
#'
#' @param package Package name.
#' @param forced_push Whether to make a forced push.
#' @return Output of the command line git call.
#'
#' @keywords internal

push_to_github <- function(package, forced_push = FALSE) {
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(package)

  add_gh_remote(package)

  proc <- cli_process_start("Pushing {.pkg {package}} to GitHub")
  git("push", "--tags", if (forced_push) "-f", "-u", "origin", "master")
  cli_process_done(proc)
}

#' Add a CRAN at GitHub remote to a local git tree
#'
#' It is assumed that the git tree of the package is
#' in the working directory.
#'
#' If there is already a remote called `origin`, then it does nothing.
#'
#' @param package Package name.
#'
#' @keywords internal

add_gh_remote <- function(package) {

  current <- git("remote")$stdout
  if (! grepl("\\borigin\\b", current)) {
    git(
      "remote", "add", "origin",
      get_clone_url(package)
    )
  }
}

#' Remove a GitHub remote
#'
#' This is only used for the unit tests. Otherwise we don't remove
#' repositories from GitHub, even if a package is archived.
#'
#' @param package Package name.
#'
#' @keywords internal

remove_gh_repo <- function(package) {

  gh(
    "DELETE /repos/:owner/:repo",
    owner = default_cranatgh_org(),
    repo = package,
    .token = get_gh_token()
  )
}

#' Create a repository for a package at GitHub
#'
#' @param package Package name.
#' @param description Repository description.
#'
#' @keywords internal
#' @importFrom httr POST add_headers status_code

create_gh_repo <- function(package,
                           description = make_description(package)) {

  tryCatch(
    gh(
      "POST /orgs/:org/repos",
      org = default_cranatgh_org(),
      name = package,
      description = description,
      .token = get_gh_token()
    ),
    error = function(e) {
      warning("Cannot create GH repo, already exists? ", e)
    }
  )
}


#' Update description on GitHub
#'
#' Updates the repostory description and homepage.
#'
#' @param package Name of the package
#' @param description Description on the GitHub page.
#'
#' @export
#' @importFrom cli cli_process_start cli_process_done

update_description <- function(package,
                               description = make_description(package)) {

  description <- clean_description(description)

  proc <- cli_process_start("Updating repo description for {.pkg {package}}")
  gh("PATCH /repos/:owner/:repo",
     owner = default_cranatgh_org(),
     repo = package,
     name = package,
     description = description,
     homepage = "",
     .token = get_gh_token()
  )
  cli_process_done(proc)
}

#' Clean a string, so that it can be used as a repo description
#'
#' Currently it just removes newline characters.
#'
#' @param description Character scalar.
#' @return Character scalr. Cleaned up description.
#'
#' @keywords internal

clean_description <- function(description) {
  description <- unname(description)
  description <- gsub("\n", " ", description)
  description
}

graphql_query <- function(query) {

}
