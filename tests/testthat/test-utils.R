context("utils")

test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})

test_that("remove_non_numerics works", {
  l <- list(
    "a" = 2,
    "b" = "a",
    "c" = 2.1,
    "d" = array(0, c(2, 2, 1))
  )
  l <- remove_non_numerics(l)
  expect_named(l, c("a", "c", "d"))
})
