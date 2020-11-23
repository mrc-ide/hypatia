test_that("test create_hospitilisation_update_listener ", {

  ret <- create_hospitilisation_update_listener(
    "human",
    "states",
    "variables",
    "parameters",
    "events"
  )

  expect_type(ret, "closure")

  mock_hospitilisation_flow_process <- mockery::mock()

  with_mock(
    "hypatia::hospitilisation_flow_process" = mock_hospitilisation_flow_process,
    ret("api", "to_move")
  )

  mockery::expect_called(mock_hospitilisation_flow_process, 1)
  mockery::expect_args(
    mock_hospitilisation_flow_process, 1, "api", "to_move", "human", "states",
                       "variables", "parameters", "events")


})

test_that("test hospitilisation_flow_process", {

  individuals <- list(human = list(
    IOxGetLive = c(2),
    IOxGetDie = c(4),
    IMVGetLive = c(1),
    IMVGetDie = c(2),
    IRec = c(2)
  ))

  variables <- list(discrete_age = 10)

  problist <- rep(0.1, 10)

  parameters <- list(prob_severe = problist,
                     prob_hosp = mockery::mock(),
                     dur_E = mockery::mock(), ICU_beds = mockery::mock(),
                     prob_severe_death_treatment = problist,
                     prob_non_severe_death_treatment = problist,
                     hosp_beds = mockery::mock())

  events <- list(imv_get_live = mockery::mock(),
                 imv_get_die = mockery::mock(),
                 imv_not_get_live = mockery::mock(),
                 imv_not_get_die = mockery::mock(),
                 iox_get_live = mockery::mock(),
                 iox_get_die = mockery::mock(),
                 iox_not_get_live = mockery::mock(),
                 iox_not_get_die = mockery::mock())

  states <- list(IOxGetLive = mockery::mock(), IOxGetDie = mockery::mock(),
                 IMVGetLive = mockery::mock(), IMVGetDie = mockery::mock(),
                 IRec = mockery::mock())

  to_move <- seq(1, 100000, 1)
  target <- mockery::mock()
  r_erlang_mock <- mockery::mock(c(TRUE, TRUE, TRUE, TRUE))

  api <- mock_api(
    list(
      human = list(
        IOxGetLive = c(2),
        IOxGetDie = c(4),
        IMVGetLive = c(1),
        IMVGetDie = c(2),
        IRec = c(2)
      )
    ),
    timestep = 50,
    parameters = parameters
  )

  ret <- hospitilisation_flow_process(
    api,
    to_move,
    "human",
    states,
    variables,
    parameters,
    events
  )

  mockery::expect_args(api$get_variable, 1, individuals$human,
                       variables$discrete_age, to_move)

  mockery::expect_args(api$get_state, 1, individuals$human,
                       states$IMVGetDie, states$IMVGetLive, states$IRec)
  with_mock(
    'hypatia::r_erlang' = r_erlang_mock,
    ret3 <- api$schedule(
      events$imv_get_die,
      target,
      rep(.2, 4)
    )
  )

})

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
