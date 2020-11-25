context("utils")

test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("r_exp works", {

  ret <- r_exp(1, 0.1)
  expect_true(ret >= 0.0)
  expect_true(ret <= 1.0)

})

test_that("bernoulli_multi_p works", {

  ret <- bernoulli_multi_p(1, 0.1)
  expect_true(ret >= 0.0)
  expect_true(ret <= 1.0)

})


