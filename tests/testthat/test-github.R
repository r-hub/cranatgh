
context("GitHub related")

test_that("get_github_versions", {

  if (!has_gh_access()) skip("Cannot reach GitHub")
  skip_github_tests()

  expect_equal(
    get_github_versions("igraph0"),
    c("0.5.5-1", "0.5.5-2", "0.5.5-3", "0.5.5", "0.5.6-1", "0.5.6-2",
      "0.5.6", "0.5.7")
  )
})


test_that("get_github_versions with package not on github", {

  if (!has_gh_access()) skip("Cannot reach GitHub")
  skip_github_tests()

  expect_equal(
    get_github_versions("dfgd--gsdfgxfg444"),
    character()
  )
})


test_that("clone_git_repo", {

  if (!has_gh_access()) skip("Cannot reach GitHub")
  skip_github_tests()

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

  if (!has_gh_access()) skip("Cannot reach GitHub")
  skip_github_tests()

  with_tempdir({
    git("init", ".")
    cat("test", file = "test.txt")
    git("add", "test.txt")
    git("commit", "-m", "testing")
    add_gh_remote("foobar")
    expect_true(grepl("origin", git("remote")$stdout, fixed = TRUE))

    expect_silent(add_gh_remote("foobar"))
    expect_true(grepl("origin", git("remote")$stdout, fixed = TRUE))
  })
})


test_that("create_gh_repo", {

  if (!has_gh_access()) skip("Cannot reach GitHub")
  skip_github_tests()

  with_tempdir({
    git("init", ".")
    cat("test", file = "test.txt")
    git("add", "test.txt")
    git("commit", "-m", "testing")

    package <- paste0("cranatghtest", get_random_id())

    ## order matters here, because remove_gh_repo() uses the env var
    on.exit(try(remove_gh_repo(package)), add = TRUE)
    withr::local_envvar(c("CRANATGH_ORG" = "metacran"))

    create_gh_repo(package, description = "just testing")

    x <- gh::gh("/repos/metacran/:repo", repo = package)
    expect_equal(x$name, package)
  })
})


test_that("push_to_github", {

  if (!has_gh_access()) skip("Cannot reach GitHub")
  skip_github_tests()

  package <- paste0("cranatghtest", get_random_id())

  with_tempdir({
    dir.create(package)
    setwd(package)
    git("init", ".")
    cat("test", file = "test.txt")
    git("add", "test.txt")
    git("commit", "-m", "testing")
    setwd("..")

    ## order matters here, because remove_gh_repo() uses the env var
    on.exit(try(remove_gh_repo(package)), add = TRUE)
    withr::local_envvar(c("CRANATGH_ORG" = "metacran"))

    create_gh_repo(package, description = "just testing")
    push_to_github(package)

    setwd(package)
    expect_true(grepl("origin", git("remote")$stdout, fixed = TRUE))

    x <- gh::gh("/repos/metacran/:repo/commits", repo = package)
    expect_equal(length(x), 1)
  })
})

test_that("update_description", {

  if (!has_gh_access()) skip("Cannot reach GitHub")
  skip_github_tests()

  package <- paste0("cranatghtest", get_random_id())

  ## order matters here, because remove_gh_repo() uses the env var
  on.exit(try(remove_gh_repo(package)), add = TRUE)
  withr::local_envvar(c("CRANATGH_ORG" = "metacran"))

  create_gh_repo(package, description = "just testing")

  update_description(package, "new description")

  x <- gh::gh("/repos/metacran/:repo", repo = package)
  expect_equal(x$description, "new description")
})
