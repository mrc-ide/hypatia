test_that("test scheduler", {
  R0 <- 2
  timestep <- 100
  dt <- 1
  time_period <- 1000
  tt_contact_matrix <- 0
  newpopulation <- 100000
  numberof_days <- 5

  parameters <- get_parameters("Afghanistan", R0, timestep, dt, time_period,
                        tt_contact_matrix, newpopulation, numberof_days)

  states <- create_states(parameters)
  variables <- create_variables(parameters)
  events <- create_events()
  individuals <- create_individuals(states, variables, events, parameters)

  create_event_based_processes(individuals, states, variables, events,
                               parameters)

  api <- mock_api(list(), parameters = parameters)
  events$mild_infection$listeners[[2]](api, 1)

  mockery::expect_args(
    create_infection_update_listener,
    1,
    individuals$human,
    states$IMild,
    c(api, 1)
  )

})

test_that("test create_infection_update_listener", { # WORKING

  human <- mockery::mock()
  IMild <- mockery::mock()

  ret <- create_infection_update_listener(
    human,
    IMild
  )

  api <- list(queue_state_update = mockery::mock())
  to_move <- mockery::mock()
  ret(api, to_move)
  mockery::expect_args(api$queue_state_update, 1, human, IMild, to_move)

})

test_that("test create_progression_listener", { # NOT WORKING

  events <- mockery::mock()
  shift <- 0
  duration <- mockery::mock()
  #erlang_mock = mockery::mock(c(0,1) + 0)
  func_mock <- mockery::mock(c(0,1))

  ret <- create_progression_listener(
    events,
    duration,
    shift,
    func_mock
  )
  api <- list(schedule = mockery::mock())
  target <- mockery::mock()
  event <- mockery::mock()

  ret(api, target)
  mockery::expect_args(api$schedule, 1, event, target, func_mock)

})


test_that("test create_exposure_update_listener", { #FAILS

  human <- mockery::mock()
  states <- mockery::mock()
  events <- mockery::mock()

  ret <- create_exposure_update_listener(
    human,
    states,
    events,
    variables,
    parameters
  )

  api <- list(get_variable = mockery::mock(), schedule = mockery::mock())
  variables <- list(discrete_age = mockery::mock())
  events <- list(severe_infection = mockery::mock(),
                 mild_infection = mockery::mock())
  problist <- seq(1,17,1)
  parameters <- list(dur_E = mockery::mock(),
                     prob_hosp = list(mockery::mock(problist)))
  to_move <- mockery::mock()
  discrete_age <- mockery::mock()
  erlang_mock <- mockery::mock(c(1, 1))


  ret(api, to_move)
  #mockery::expect_args(erlang, 1, erlang_mock)
  #mockery::expect_args(delay, 1, erlang_mock)
  mockery::expect_args(api$get_variable, 1, human, variables$discrete_age,
                       to_move)
  mockery::expect_args(api$schedule, 1, events$severe_infection, to_move, erlang_mock)

})

test_that("test create_hospitilisation_update_listener", { #FAILS

  human <- mockery::mock()
  states <- mockery::mock()
  events <- mockery::mock()
  variables <- mockery::mock()
  parameters <- mockery::mock()

  ret <- create_hospitilisation_update_listener(
    human,
    states,
    variables,
    parameters,
    events
  )

  api <- list()
  ret(api, to_move)
  to_move <- mockery::mock()
  mockery::expect_args(hospitilisation_flow_process, 1, api,
                       to_move,
                       human,
                       states,
                       variables,
                       parameters,
                       events)
})

test_that("test hospitilisation_flow_process", {

  human <- mockery::mock()

  ret <- hospitilisation_flow_process(
    api,
    to_move,
    human,
    states,
    variables,
    parameters,
    events)

  api <- list(get_variable = mockery::mock(), get_state = mockery::mock(),
              get_parameters = mockery::mock())

  variables <- list(discrete_age = mockery::mock())
  problist <- seq(1,17,1)
  parameters <- list(prob_severe = mockery::mock(problist), prob_hosp = mockery::mock(),
                     dur_E = mockery::mock(), ICU_beds = mockery::mock(),
                     prob_severe_death_treatment = mockery_mock(problist),
                     prob_non_severe_death_treatment = mockery_mock(problist),
                     hosp_beds = mockery::mock())
  events <- list(imv_get_live = mockery::mock(),
                 imv_get_die = mockery::mock(),
                 imv_not_get_live = mockery::mock(),
                 imv_not_get_die = mockery::mock(),
                 iox_get_live = mockery::mock(), iox_get_die = mockery::mock(),
                 iox_not_get_live = mockery::mock(),
                 iox_not_get_die = mockery::mock())
  states <- list(IOxGetLive = mockery::mock(), IOxGetDie = mockery::mock(),
                 IMVGetLive = mockery::mock(), IMVGetDie = mockery::mock(),
                 IRec = mockery::mock())

  ret(api, to_move)
  to_move <- mockery::mock()
  mockery::expect_args(hospitilisation_flow_process, 1, api,
                       to_move,
                       human,
                       states,
                       variables,
                       parameters,
                       events)

})


test_that("test initialise_progression", {

  human <- mockery::mock()
  event <- mockery::mock()
  human <- mockery::mock()
  duration <- mockery::mock()

  initialise_progression(api,
                         event,
                         human,
                         from_state,
                         duration

  )
  from_state <- mockery::mock()
  api <- list(schedule = mockery::mock(), get_state = mockery::mock())

  target <- mockery::mock()
  func <- mockery::mock(r_erlang(1, 1))
  func_mock <- mockery::mock(func)

  mockery::expect_args(api$schedule, 1, event, target, func_mock)
  mockery::expect_args(api$get_state, 1, human, from_state)

})
