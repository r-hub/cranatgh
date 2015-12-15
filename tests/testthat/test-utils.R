
context("Utility functions")

test_that("with_tempdir works", {

  wd <- getwd()
  tmp <- tempfile()

  ## We need to do this, because on some systems tempdir() is a symlink
  dir.create(tmp)
  setwd(tmp)
  real_tmp <- getwd()
  setwd(wd)

  ## Just in case with_tempdir fails
  on.exit(setwd(wd), add = TRUE)

  with_mock(
    `base::tempfile` = function(...) tmp,

    ## Command is run in the tmp directory
    expect_output(
      with_tempdir(cat("hello",  getwd())),
      paste("hello", real_tmp)
    ),

    ## Tmp directory is cleaned up after
    expect_false(file.exists(tmp)),

    ## Working directory is restored
    expect_equal(wd, getwd())
  )
})
