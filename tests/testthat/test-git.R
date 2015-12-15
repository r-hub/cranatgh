
context("Git related")

test_that("create_git_repo", {

  tmpdir <- tempfile()
  with_tempdir(
    tmpdir = tmpdir,
    {
      wd <- getwd()
      create_git_repo("mypackage")
      expect_true(file.exists(file.path(tmpdir, "mypackage", ".git")))
      expect_equal(wd, getwd())
    }
  )
})
