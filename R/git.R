
#' Run a git command using system git
#'
#' System git must be properly installed and it must be in the
#' \code{PATH}.
#'
#' @param ... Arguments to pass to git.
#' @param env Named character vector, environment variables
#'   to set for git.
#' @return A list containing the \code{status}, \code{stdout},
#'   \code{stderr} of the system git command.
#'
#' @keywords internal

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

#' Create an empty local git tree
#'
#' @param package Package name, directory to create a git tree in.
#' @return The return value of the call to git.
#'
#' @keywords internal

create_git_repo <- function(package) {
  dir.create(package)
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(package)
  git("init", ".")
}

set_git_user <- function(package) {
  with_wd(package, {
    git("config", "--local", "user.name", "cran-robot")
    git("config", "--local", "user.email", "csardi.gabor+cran@gmail.com")
  })
}
