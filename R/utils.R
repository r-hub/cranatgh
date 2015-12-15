
with_tempdir <- function(expr, tmpdir = tempfile(), remove = TRUE) {

  if (remove) on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  if (!file.exists(tmpdir)) dir.create(tmpdir)

  with_wd(tmpdir, expr)
}

with_wd <- function(wd, expr) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(wd)
  expr
}

na_to_empty <- function(x) {
  if (length(x) == 1 && is.na(x)) "" else x
}
