
get_gh_token <- function() {
  Sys.getenv("GITHUB_TOKEN", NA_character_)
}

get_gh_owner <- function(package) "cran"

get_clone_url <- function(package) {

  owner <- get_gh_owner(package)
  token <- get_gh_token()
  token <- if (is.na(token)) "" else paste0(token, "@")

  sprintf("https://%sgithub.com/%s/%s.git", token, owner, package)
}

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


clone_git_repo <- function(package) {
  git("clone", get_clone_url(package))
}


#' Push the package to GitHub
#'
#' @param package Package name.

push_to_github <- function(package) {
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(package)

  add_gh_remote(package)

  git("push", "--tags", "-u", "origin", "master")
}


add_gh_remote <- function(package) {

  current <- git("remote")$stdout
  if (! grepl("\\borigin\\b", current)) {
    git(
      "remote", "add", "origin",
      get_clone_url(package)
    )
  }
}


remove_gh_repo <- function(package) {

  gh(
    "DELETE /repos/:owner/:repo",
    owner = get_gh_owner(package),
    repo = package
  )
}


#' @importFrom httr POST add_headers status_code

create_gh_repo <- function(package, description) {

  description <- clean_description(description)

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
#' @param description \code{Title} field in the \code{DESCRIPTION} file.
#'
#' @export

update_description <- function(package, description) {

  description <- clean_description(description)

  gh("PATCH /repos/:owner/:repo",
     owner = get_gh_owner(),
     repo = package,
     name = package,
     description = description
  )
}

clean_description <- function(description) {
  description <- unname(description)
  description <- gsub("\n", " ", description)
  description
}
