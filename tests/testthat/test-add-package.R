
context("Adding a package to CRAN@GH")

test_that("add_missing_version on new repo", {

  if (!has_gh_access()) skip("Cannot reach GitHub")
  skip_github_tests()

  with_tempdir({
    create_git_repo("crayon")
    add_missing_version("crayon", "1.0.0", "2014-09-23T22:19:16+00:00")
    expect_true(file.exists(file.path("crayon", ".git")))

    setwd("crayon")
    expect_true(grepl("version 1.0.0", git("log")$stdout, fixed = TRUE))
    expect_true(grepl("^1\\.0\\.0\\s$", git("tag")$stdout))
  })
})


test_that("add_missing_version on existing repo", {

  if (!has_gh_access()) skip("Cannot reach GitHub")
  skip_github_tests()

  with_tempdir({
    clone_git_repo("crayon")

    ## Go back to version 1.3.0
    setwd("crayon")
    git("tag", "-d", "1.3.1")
    git("reset", "1.3.0")
    git("checkout", "--", ".")
    setwd("..")

    add_missing_version("crayon", "1.3.1", "2015-07-13T04:05:12+00:00")

    setwd("crayon")
    expect_true(grepl("version 1.3.1", git("log")$stdout, fixed = TRUE))
    expect_true(grepl("\\n1\\.3\\.1", git("tag")$stdout))
  })
})


test_that("add_missing_versions for new package", {

  if (!has_gh_access()) skip("Cannot reach GitHub")
  skip_github_tests()

  mockery::stub(add_missing_versions, "create_gh_repo", function(...) { })
  mockery::stub(add_missing_versions, "push_to_github", function(...) { })
  mockery::stub(add_missing_versions, "update_description", function(...) { })

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  withr::local_envvar(list(CRANATGH_TREES = tmp))

  add_missing_versions(
    "crayon",
    versions = c("1.0.0", "1.1.0", "1.2.0"),
    new_package = TRUE,
    timeline = crayon_timeline()
  )

  withr::with_dir(tmp, {
    setwd("crayon")
    expect_true(grepl("version 1.0.0", git("log")$stdout, fixed = TRUE))
    expect_true(grepl("version 1.1.0", git("log")$stdout, fixed = TRUE))
    expect_true(grepl("version 1.2.0", git("log")$stdout, fixed = TRUE))

    expect_true(
      grepl("1.0.0\n1.1.0\n1.2.0\n", git("tag")$stdout, fixed = TRUE)
    )
  })
})


test_that("add_missing_versions for existing package", {

  if (!has_gh_access()) skip("Cannot reach GitHub")
  skip_github_tests()

  mockery::stub(add_missing_versions, "create_gh_repo", function(...) { })
  mockery::stub(add_missing_versions, "push_to_github", function(...) { })
  mockery::stub(add_missing_versions, "update_description", function(...) { })
  mockery::stub(add_missing_versions, "clone_git_repo", function(package, ...) {
    git("clone", get_clone_url(package))
    ## Go back to version 1.0.0
    withr::with_dir("crayon", {
      git("tag", "-d", "1.3.1")
      git("tag", "-d", "1.3.0")
      git("tag", "-d", "1.2.1")
      git("tag", "-d", "1.2.0")
      git("tag", "-d", "1.1.0")
      git("reset", "1.0.0")
      git("checkout", "--", ".")
    })
  })

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  withr::local_envvar(list(CRANATGH_TREES = tmp))

  add_missing_versions(
    "crayon",
    versions = c("1.1.0", "1.2.0"),
    new_package = FALSE,
    timeline = crayon_timeline()[-1,],
    reset = FALSE
  )

  withr::with_dir(tmp, {
    setwd("crayon")
    expect_true(grepl("version 1.0.0", git("log")$stdout, fixed = TRUE))
    expect_true(grepl("version 1.1.0", git("log")$stdout, fixed = TRUE))
    expect_true(grepl("version 1.2.0", git("log")$stdout, fixed = TRUE))
    expect_true(
      grepl("1.0.0\n1.1.0\n1.2.0\n", git("tag")$stdout, fixed = TRUE)
    )
  })
})
