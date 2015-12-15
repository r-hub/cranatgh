
context("GitHub related")

test_that("get_github_versions", {

  skip_on_cran()
  if (!is_online()) skip("Cannot reach GitHub")

  with_mock(
    `gh::gh` = github_testthat,

    expect_equal(
      get_github_versions("testthat"),
      c("0.10.0", "0.9.1", "0.9", "0.8.1", "0.8", "0.7.1", "0.7", "0.6",
        "0.5", "0.4", "0.3", "0.2", "0.1.1", "0.1")
    )
  )
})


test_that("get_github_versions with package not on github", {

  skip_on_cran()
  if (!is_online()) skip("Cannot reach GitHub")

  with_mock(
    `gh::gh` = function(...) stop("does not exist"),

    expect_equal(
      get_github_versions("444"),
      character()
    )
  )
})


test_that("clone_git_repo", {

  skip_on_cran()
  if (!is_online()) skip("Cannot reach GitHub")

  tmpdir <- tempfile()
  with_tempdir(
    tmpdir = tmpdir,
    {
      clone_git_repo("crayon")
      expect_true(file.exists(file.path(tmpdir, "crayon")))
      expect_true(file.exists(file.path(tmpdir, "crayon", ".git")))
      expect_true(file.info(file.path(tmpdir, "crayon", ".git"))$isdir)
    }
  )

})


test_that("add_gh_remote", {

  skip_on_cran()
  if (!is_online()) skip("Cannot reach GitHub")

  with_tempdir({
    git("init", ".")
    cat("test", file = "test.txt")
    git("add", "test.txt")
    git("commit", "-m", "testing")

    with_mock(
      `cranatgh::get_clone_url` =
        function(...) "git@github.com:gaborcsardi/playground.git",

      add_gh_remote("foobar")
    )
    expect_true(grepl("origin", git("remote")$stdout, fixed = TRUE))

    with_mock(
      `cranatgh::get_clone_url` =
        function(...) "git@github.com:gaborcsardi/playground.git",

      expect_silent(add_gh_remote("foobar"))
    )
    expect_true(grepl("origin", git("remote")$stdout, fixed = TRUE))
  })
})


test_that("create_gh_repo", {

  skip_on_cran()
  if (!is_online()) skip("Cannot reach GitHub")
  if (is.na(get_gh_token())) skip("Need to set GITHUB_TOKEN")

  with_tempdir({
    git("init", ".")
    cat("test", file = "test.txt")
    git("add", "test.txt")
    git("commit", "-m", "testing")

    with_mock(
      `cranatgh::get_clone_url` =
        function(...) "git@github.com:metacran/cranatghtest.git",
      `cranatgh::get_gh_owner` = function(...) "metacran",
      try(remove_gh_repo("cranatghtest"), silent = TRUE)
    )

    expect_error(
      gh::gh("/repos/metacran/cranatghtest"),
      "GitHub API error"
    )

    with_mock(
      `cranatgh::get_clone_url` =
        function(...) "git@github.com:metacran/cranatghtest.git",
      `cranatgh::get_gh_owner` = function(...) "metacran",
      create_gh_repo("cranatghtest", description = "just testing")
    )

    x <- gh::gh("/repos/metacran/cranatghtest")
    expect_equal(x$name, "cranatghtest")
  })
})


test_that("push_to_github", {

  skip_on_cran()
  if (!is_online()) skip("Cannot reach GitHub")
  if (is.na(get_gh_token())) skip("Need to set GITHUB_TOKEN")

  with_tempdir({
    dir.create("cranatghtest")
    setwd("cranatghtest")
    git("init", ".")
    cat("test", file = "test.txt")
    git("add", "test.txt")
    git("commit", "-m", "testing")
    setwd("..")

    with_mock(
      `cranatgh::get_clone_url` =
        function(...) "git@github.com:metacran/cranatghtest.git",
      `cranatgh::get_gh_owner` = function(...) "metacran",
      try(remove_gh_repo("cranatghtest"), silent = TRUE),
      create_gh_repo("cranatghtest", description = "just testing")
    )

    with_mock(
      `cranatgh::get_clone_url` =
        function(...) {
          sprintf(
            "https://%s@github.com/metacran/cranatghtest.git",
            get_gh_token()
          )
        },
      `cranatgh::get_gh_owner` = function(...) "metacran",
      push_to_github("cranatghtest")
    )

    setwd("cranatghtest")
    expect_true(grepl("origin", git("remote")$stdout, fixed = TRUE))

    x <- gh::gh("/repos/metacran/cranatghtest/commits")
    expect_equal(length(x), 1)

  })
})

test_that("update_description", {

  skip_on_cran()
  if (!is_online()) skip("Cannot reach GitHub")
  if (is.na(get_gh_token())) skip("Need to set GITHUB_TOKEN")

  with_mock(
    `cranatgh::get_clone_url` =
      function(...) "git@github.com:metacran/cranatghtest.git",
    `cranatgh::get_gh_owner` = function(...) "metacran",
    try(remove_gh_repo("cranatghtest"), silent = TRUE),
    create_gh_repo("cranatghtest", description = "just testing")
  )

  with_mock(
    `cranatgh::get_clone_url` =
      function(...) "git@github.com:metacran/cranatghtest.git",
    `cranatgh::get_gh_owner` = function(...) "metacran",
    update_description("cranatghtest", "new description")
  )

  x <- gh::gh("/repos/metacran/cranatghtest")
  expect_equal(x$description, "new description")

})
