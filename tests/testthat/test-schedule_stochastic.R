test_that("test scheduler", {
  api <- mock_api(
    list(
      human = list(
        S = c(2),
        E = c(3),
        A = c(4),
        D = c(1),
        birth = -c(20, 24, 5, 39) * 365 + 5,
        IB = c(.2, .3, .5, .9),
        zeta = c(.2, .3, .5, .9)
  )))


  api$get_scheduled = mockery::mock(4, 1)


  schedule_infections(
    api,
    events,
    clinical_infections,
    treated,
    infected_humans
  )

  mockery::expect_args(
    api$schedule,
    2,
    events$infection,
    3,
    12
  )


})
