test_that("test create_setup_process", { # WORKING locally

  individuals <- list(human = mockery::mock())
  states <- list(E = mockery::mock())
  variables <- list( discrete_age = mockery::mock())
  problist <- seq(1, 17, 1)
  parameters <- list(prob_hosp = list(mockery::mock(problist)))
  exposed <- NULL
  events <- mockery::mock()


  ret <- create_setup_process(
      individuals,
      states,
      events,
      variables
    )

  api <- list(get_parameters = mockery::mock(),
              get_state = mockery::mock(),
              get_variable = mockery::mock(),
              schedule = mockery::mock()
              )

  target <- mockery::mock()
  r_erlang_mock <- mockery::mock(c(TRUE, TRUE, TRUE, TRUE))

  ret(api)

  mockery::expect_args(api$get_parameters, 1)
  mockery::expect_args(api$get_state, 1, individuals$human, states$E)
  mockery::expect_args(api$get_variable, 1, individuals$human,
                       variables$discrete_age, exposed)

  with_mock(
    'hypatia::r_erlang' = r_erlang_mock,
    ret <- api$schedule(
      events,
      target,
      rep(.2, 4)
    )
  )

})
