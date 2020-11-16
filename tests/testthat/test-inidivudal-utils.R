test_that("check that Validated_state_update is working as expected", {

  human <- mockery::mock()
  S <- mockery::mock()

  api <- list(
    queue_state_update = mockery::mock()
  )

  expect_error(
    hypatia::validated_state_update(api, human, S, 10001, 10000), "*")
  expect_error(
    hypatia::validated_state_update(api, human, S, 933.5, 10000), "*")
  expect_error(hypatia::validated_state_update(api, human, S, -10, 10000), "*")
  expect_error(hypatia::validated_state_update(api, human, S, NA, 10000), "*")
})

