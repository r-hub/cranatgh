
context("Crandb related")

test_that("get_crandb_versions", {

  skip_on_cran()
  if (!is_online()) skip("Cannot reach GitHub")

  with_mock(
    `crandb::package` = crandb_igraph,

    expect_equal(
      get_crandb_versions("igraph"),
      c("0.1.1", "0.1.2", "0.2", "0.2.1", "0.3.1", "0.3.2", "0.3.3",
        "0.4", "0.4.1", "0.4.2", "0.4.3", "0.4.4", "0.4.5", "0.5", "0.5.1",
        "0.5.2", "0.5.2-2", "0.5.3", "0.5.4", "0.5.4-1", "0.5.5", "0.5.5-1",
        "0.5.5-2", "0.5.5-3", "0.5.5-4", "0.6", "0.6-1", "0.6-2", "0.6-3",
        "0.6.4", "0.6.5", "0.6.5-1", "0.6.5-2", "0.6.6", "0.7.0", "0.7.1",
        "1.0.0", "1.0.1")
    )
  )
})


test_that("get_crandb_versions for non-exixting packages", {

  skip_on_cran()
  if (!is_online()) skip("Cannot reach GitHub")

  with_mock(
    `crandb::package` = function(...) stop("does not exist"),

    expect_equal(
      get_crandb_versions("444"),
      NULL
    )
  )
})
