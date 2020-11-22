test_that("test all listeners", { # WORKING

  parameters <- mockery::mock()
  states <- mockery::mock()
  variables <- mockery::mock()
  individuals <- mockery::mock()

  events <- list(exposure = mockery::mock(),
                 mild_infection = mockery::mock(),
                 severe_infection = mockery::mock(),
                 hospitilisation = mockery::mock(),
                 imv_get_live = mockery::mock(),
                 imv_get_die = mockery::mock(),
                 hospitilisation = mockery::mock(),
                 imv_get_live = mockery::mock(),
                 imv_get_die = mockery::mock(),
                 iox_get_live = mockery::mock(),
                 iox_get_die = mockery::mock(),
                 imv_not_get_live = mockery::mock(),
                 imv_not_get_die = mockery::mock(),
                 iox_not_get_live = mockery::mock(),
                 iox_not_get_die = mockery::mock(),
                 stepdown = mockery::mock(),
                 recovery = mockery::mock(),
                 death = mockery::mock()
  )

  create_event_based_processes(individuals, states, variables, events,
                               parameters)

  api <- list(queue_variable_update = mockery::mock())

  events$exposure$listeners[[2]](api, numeric(0))
  mockery::expect_called(api$queue_variable_update, 0)

  events$mild_infection$listeners[[2]](api, numeric(0))
  mockery::expect_called(api$queue_variable_update, 0)

  events$severe_infection$listeners[[2]](api, numeric(0))
  mockery::expect_called(api$queue_variable_update, 0)

  events$hospitilisation$listeners[[2]](api, numeric(0))
  mockery::expect_called(api$queue_variable_update, 0)

  events$imv_get_live$listeners[[2]](api, numeric(0))
  mockery::expect_called(api$queue_variable_update, 0)

  events$imv_get_die$listeners[[2]](api, numeric(0))
  mockery::expect_called(api$queue_variable_update, 0)

  events$iox_get_live$listeners[[2]](api, numeric(0))
  mockery::expect_called(api$queue_variable_update, 0)

  events$iox_get_die$listeners[[2]](api, numeric(0))
  mockery::expect_called(api$queue_variable_update, 0)

  events$imv_not_get_live$listeners[[2]](api, numeric(0))
  mockery::expect_called(api$queue_variable_update, 0)

  events$imv_not_get_die$listeners[[2]](api, numeric(0))
  mockery::expect_called(api$queue_variable_update, 0)

  events$iox_not_get_live$listeners[[2]](api, numeric(0))
  mockery::expect_called(api$queue_variable_update, 0)

  events$iox_not_get_die$listeners[[2]](api, numeric(0))
  mockery::expect_called(api$queue_variable_update, 0)

  events$stepdown$listeners[[2]](api, numeric(0))
  mockery::expect_called(api$queue_variable_update, 0)

  events$recovery$listeners[[1]](api, numeric(0))
  mockery::expect_called(api$queue_variable_update, 0)

  events$death$listeners[[1]](api, numeric(0))
  mockery::expect_called(api$queue_variable_update, 0)

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

test_that("test create_progression_listener", { # WORKING

  events <- mockery::mock()
  shift <- 0
  duration <- mockery::mock()
  r_erlang_mock <- mockery::mock(c(TRUE, TRUE, TRUE, TRUE))

  with_mock(
    'r_erlang' = r_erlang_mock,
    ret <- create_progression_listener(
      events,
      duration,
      shift,
      rep(.2, 4)
    )
  )

})


test_that("test create_exposure_update_listener", { #WORKING

  human <- mockery::mock()
  states <- mockery::mock()

  ret <- create_exposure_update_listener(
    human,
    states,
    events,
    variables,
    parameters
  )

  api <- list(get_variable = mockery::mock(), schedule = mockery::mock())
  variables <- list(discrete_age = mockery::mock())
  events <- list(severe_infection = mockery::mock())
  problist <- seq(1,17,1)
  parameters <- list(dur_E = mockery::mock(),
                     prob_hosp = list(mockery::mock(problist)))
  to_move <- mockery::mock()
  discrete_age <- mockery::mock()

  r_erlang_mock <- mockery::mock(c(TRUE, TRUE, TRUE, TRUE))
  ret(api, to_move)

  mockery::expect_args(api$get_variable, 1, human, variables$discrete_age,
                       to_move)

  with_mock(
    'r_erlang' = r_erlang_mock,
    ret <- api$schedule(
      events,
      to_move,
      rep(.2, 4)
    )
  )

})



test_that("test initialise_progression", {

  event <- mockery::mock()
  human <- mockery::mock()
  from_state <- mockery::mock()
  duration <- mockery::mock()
  r_erlang_mock <- mockery::mock(c(TRUE, TRUE, TRUE, TRUE))

  ret <- initialise_progression(event, human, from_state, duration)

  api <- list(schedule = mockery::mock(), get_state = mockery::mock())
  target <- mockery::mock()

  ret(api, target)

  mockery::expect_args(api$get_state, 1, human, from_state)

  with_mock(
    'r_erlang' = r_erlang_mock,
    ret3 <- api$schedule(
      event,
      target,
      rep(.2, 4)
    )
  )

  ##################################
  # human <- mockery::mock()
  # event <- mockery::mock()
  # human <- mockery::mock()
  # duration <- 5
  # from_state <- mockery::mock()
  #
  # # initialise_progression(api, event, human, from_state, duration)
  #
  # api <- list(schedule = mockery::mock(), get_state = mockery::mock())
  #
  # target <- mockery::mock()
  # r_erlang_mock <- mockery::mock(c(TRUE, TRUE, TRUE, TRUE))
  #
  # mockery::expect_args(initialise_progression, 1, api, event, human,
  #                      from-state, duration)

  #


})

test_that("test create_events", {

  events <- create_events()

  expect_equal(events$exposure, individual::Event$new("exposure"))
  expect_equal(events$mild_infection , individual::Event$new("mild_infection"))
  expect_equal(events$severe_infection , individual::Event$new("severe_infection"))
  expect_equal(events$hospitilisation , individual::Event$new("hospitilisation"))
  expect_equal(events$imv_get_live , individual::Event$new("imv_get_live"))
  expect_equal(events$imv_get_die , individual::Event$new("imv_get_die"))
  expect_equal(events$iox_get_live , individual::Event$new("iox_get_live"))
  expect_equal(events$iox_get_die , individual::Event$new("iox_get_die"))
  expect_equal(events$imv_not_get_live , individual::Event$new("imv_not_get_live"))
  expect_equal(events$imv_not_get_die , individual::Event$new("imv_not_get_die"))
  expect_equal(events$iox_not_get_live , individual::Event$new("iox_not_get_live"))
  expect_equal(events$iox_not_get_die , individual::Event$new("iox_not_get_die"))
  expect_equal(events$stepdown , individual::Event$new("stepdown"))
  expect_equal(events$recovery , individual::Event$new("recovery"))
  expect_equal(events$death , individual::Event$new("deaths"))

  expect_equal(length(events), 15)

})

