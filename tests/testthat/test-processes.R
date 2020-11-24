test_that("test create_setup_process", {

  individuals <- list(human = mockery::mock())
  states <- list(E = mockery::mock())
  variables <- list( discrete_age = seq(1,17,1))
  problist <- seq(1, 17, 1)
  parameters <- list(prob_hosp = c(0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0,
                                   0, 1))
  exposed <- NULL
  events <- mockery::mock()


  ret <- create_setup_process(
      individuals,
      states,
      events,
      variables
    )

  api <- list(get_parameters = mockery::mock(parameters),
              get_state = mockery::mock(),
              get_variable = mockery::mock(seq(17)),
              schedule = mockery::mock()
              )

  target <- mockery::mock()
  bernoulli_mock <- mockery::mock(
    c(TRUE, FALSE, TRUE, FALSE))
  r_erlang_mock <- mockery::mock(c(TRUE, TRUE, TRUE, TRUE))

  ret(api)

  # mockery::expect_args(api$get_parameters, 1)
  # mockery::expect_args(api$get_state, 1, individuals$human, states$E)
  # mockery::expect_args(api$get_variable, 1, individuals$human,
  #                      variables$discrete_age, exposed)
  #
  # with_mock(
  #   'hypatia::r_erlang' = r_erlang_mock,
  #   ret <- api$schedule(
  #     events,
  #     target,
  #     rep(.2, 4)
  #   )
  # )

})

test_that("test create_setup_process if statements", {

  individuals <- list(human = mockery::mock())
  states <- list(E = mockery::mock())
  variables <- list(discrete_age = c(3,8,12))
  #problist <- seq(1, 3, 1)
  parameters <- list(prob_hosp = c(0.5, 0.3, 0.2, 0.1, 0.6, 0.3, 0.2, 0.6,
                                   0.5, 0.3, 0.2, 0.1, 0.6, 0.5, 0.2, 0.6, 0.1),
                     dur_E = 30)
  exposed <- c(500, 200, 300, 400, 500, 200, 300, 400,
               500, 200, 300, 400, 500, 200, 300, 400, 500)
  events <- list(severe_infection = 70, mild_infection = 150)


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
)
  bernoulli_mock <- mockery::mock(
    c(TRUE, FALSE, TRUE, FALSE)

  ret(api)

   mockery::expect_args(api$get_parameters, 1)
   mockery::expect_args(api$get_state, 1, individuals$human, states$E)
   mockery::expect_args(api$get_variable, 1, individuals$human,
                        variables$discrete_age, exposed)

   with_mock(
     'hypatia:::bernoulli_multi_p' = bernoulli_mock,

  with_mock(
    'hypatia::r_erlang' = r_erlang_mock,
    ret <- api$schedule(
      events,
      target,
      rep(.2, 4)
    )
  )

})
