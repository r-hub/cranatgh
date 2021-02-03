
get_status_cache <- function() {
  tmp <- tempfile()
  dir.create(tmp)

  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(tmp)

  url <- default_cranatgh_repo()
  user <- default_cranatgh_user()
  token <- get_gh_token()
  url <- sub("^https://", paste0("https://", user, ":", token, "@"), url)

  proc <- cli_process_start("Cloning GitHub repo from {.url {safe_url(url)}}")
  git("clone", url, "--branch", "gh-pages")
  cli_process_done(proc)

  file.path(tmp, dir())
}

push_status_cache <- function(tree) {
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(tree)
  git("add", "cranatgh-status.yml")
  git("commit", "-m", "updates")
  proc <- cli_process_start("Push status cache to GitHub")
  git("push")
  cli_process_done(proc)
  invisible()
}

update_status_cache <- function(result) {
  tree <- get_status_cache()

  status <- read_status_data(tree)

  good <- which(result$result == "success")
  if (length(good) == 0) return()

  upd <- result$package[good]
  status[upd] <- result$version_cran[good]

  status <- status[order(names(status))]
  class(status) <- "cranatgh_status"

  write_status_data(status, tree)

  push_status_cache(tree)
}
