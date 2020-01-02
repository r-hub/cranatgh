
context("Crandb related")

test_that("get_crandb_versions", {

  if (!is_online()) skip("Offline")

  expect_equal(
    get_cran_versions("igraph0")$version,
    c("0.5.5", "0.5.5-1", "0.5.5-2", "0.5.5-3", "0.5.6", "0.5.6-1",
      "0.5.6-2", "0.5.7")
  )
})


test_that("get_crandb_versions for non-exixting packages", {

  if (!is_online()) skip("Offline")

  expect_equal(
    get_cran_versions("xdfsdfgwetsdf444")$version,
    character()
  )
})
