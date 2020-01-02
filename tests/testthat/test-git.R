
context("Git related")

test_that("git", {
  v <- git("version")
  expect_identical(v$status, 0L)
  expect_identical(v$stderr, "")
  expect_match(v$stdout, "^git version")
})

test_that("create_git_repo", {

  tmpdir <- tempfile()
  with_tempdir(tmpdir = tmpdir, {
    wd <- getwd()
    create_git_repo("mypackage")
    expect_true(file.exists(file.path(tmpdir, "mypackage", ".git")))
    expect_equal(wd, getwd())
  })
})

test_that("set_git_user", {
  tmpdir <- tempfile()
  with_tempdir(tmpdir = tmpdir, {
    create_git_repo("foobar")
    set_git_user("foobar", user = "userr", email = "emaill")
    setwd("foobar")
    expect_equal(git("config", "--local", "user.name")$stdout, "userr\n")
    expect_equal(git("config", "--local", "user.email")$stdout, "emaill\n")
  })
})
