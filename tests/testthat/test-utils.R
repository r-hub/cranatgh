
context("Utility functions")

test_that("set_names", {

  expect_identical(
    set_names(numeric(), character()),
    structure(numeric(), names = character())
  )
  expect_identical(
    set_names(1:3, letters[1:3]),
    structure(1:3, names = letters[1:3])
  )
  expect_identical(
    set_names(list(1,2,3), letters[1:3]),
    structure(list(1,2,3), names = letters[1:3])
  )

  expect_error(set_names(1:3, "a"))
  expect_error(set_names(1:2, letters[1:3]))
  expect_error(set_names(NULL, character()))
  expect_error(set_names(1:3, 1:3))
  expect_error(set_names(1, NA_character_))
  expect_error(set_names(1:3, c("foo", "", "bar")))
})

test_that("dont_break", {

  cases <- list(
    list("", ""),
    list(character(), character()),
    list(letters, letters),
    list(paste(1:10, letters[1:10]), paste0(1:10, "\u00a0", letters[1:10])),
    list("a    b", "a\u00a0b"),
    list("a\n \nb\nc", "a\u00a0b\u00a0c")
  )

  for (c in cases) expect_identical(dont_break(c[[1]]), c[[2]], info = c[[1]])
})

test_that("nullna_or", {
  expect_identical(nullna_or(NULL, ""), "")
  expect_identical(nullna_or(NA, ""), "")
  expect_identical(nullna_or("foo", "foobar"), "foobar")
  expect_identical(nullna_or(c(NA, "foo"), "xxx"), "xxx")
})

test_that("fix_maintainer", {

  cases <- list(
    list("  <foo@bar>", " <foo@bar>"),
    list("ORPHANED", "cran@R-project.org")
  )

  for (c in cases) {
    expect_identical(fix_maintainer(c[[1]]), c[[2]], info = c[[1]])
  }
})
