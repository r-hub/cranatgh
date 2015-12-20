
#' Get the token to be used for GitHub API calls
#'
#' It is taken from the \code{GITHUB_TOKEN} environment
#' variable.
#'
#' @return Character scalar, the token, or \code{NA} is the
#'   \code{GITHUB_TOKEN} environment variable is not set.
#'
#' @keywords internal

get_gh_token <- function() {
  Sys.getenv("GITHUB_TOKEN", NA_character_)
}

#' The GitHub repository or organization of the mirror
#'
#' @return Character scalar.
#'
#' @keywords internal

get_gh_owner <- function(package) "cran"

#' The clone URL of a package at GitHub
#'
#' It is always an HTTPS URL currently.
#'
#' @param package Name of the package.
#' @return The url.
#'
#' @keywords internal

get_clone_url <- function(package) {

  owner <- get_gh_owner(package)
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
      .limit = Inf
    ),
    condition = function(e) list()
  )
  github_versions <- vapply(github_versions, "[[", FUN.VALUE = "", "name")
  github_versions <- grep("R-", github_versions, value = TRUE, invert = TRUE)

  github_versions
}

#' Clone a repository from the GitHub CRAN mirror
#'
#' @param package Package name.
#' @return Output of the command line git call.
#'
#' @keywords internal

clone_git_repo <- function(package) {
  git("clone", get_clone_url(package))
}


#' Push the package to GitHub
#'
#' @param package Package name.
#' @return Output of the command line git call.
#'
#' @keywords internal

push_to_github <- function(package) {
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(package)

  add_gh_remote(package)

  git("push", "--tags", "-u", "origin", "master")
}

#' Add a CRAN at GitHub remote to a local git tree
#'
#' It is assumed that the git tree of the package is
#' in the working directory.
#'
#' If there is already a remote called \code{origin}, then
#' nothing is done.
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
    owner = get_gh_owner(package),
    repo = package
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

  gh("POST /orgs/:org/repos",
    org = get_gh_owner(),
    name = package,
    description = description
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

update_description <- function(package,
                               description = make_description(package)) {

  description <- clean_description(description)

  gh("PATCH /repos/:owner/:repo",
     owner = get_gh_owner(),
     repo = package,
     name = package,
     description = description,
     homepage = ""
  )
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
