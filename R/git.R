
git <- function(..., env = character()) {

  stdout <- tempfile()
  stderr <- tempfile()

  status <- system2(
    "git",
    args = unlist(list(...)),
    env = env,
    stdout = stdout,
    stderr = stderr
  )

  if (status != 0) stop("Error: ", status, ", ", stderr)

  list(
    status = status,
    stdout = readChar(stdout, file.info(stdout)$size),
    stderr = readChar(stderr, file.info(stderr)$size)
  )
}


create_git_repo <- function(package) {
  dir.create(package)
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(package)
  git("init", ".")
}
