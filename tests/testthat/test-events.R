test_that("test all listeners", { # WORKING with data but not with mockdata

  parameters <- mockery::mock()
  states <-  list(E = mockery::mock(),
                  IMild = mockery::mock(),
                  ICase1 = mockery::mock(),
                  ICase2 = mockery::mock(),
                  IOxGetLive1 = mockery::mock(),
                  IOxGetLive2 = mockery::mock(),
                  IOxGetDie1 = mockery::mock(),
                  IOxGetDie2 = mockery::mock(),
                  IOxNotGetLive1 = mockery::mock(),
                  IOxNotGetLive2 = mockery::mock(),
                  IOxNotGetDie1 = mockery::mock(),
                  IOxNotGetDie2 = mockery::mock(),
                  IMVGetLive1 = mockery::mock(),
                  IMVGetLive2 = mockery::mock(),
                  IMVGetDie1 = mockery::mock(),
                  IMVGetDie2 = mockery::mock(),
                  IMVNotGetLive1 = mockery::mock(),
                  IMVNotGetLive2 = mockery::mock(),
                  IMVNotGetDie1 = mockery::mock(),
                  IMVNotGetDie2 = mockery::mock(),
                  IRec1 = mockery::mock(),
                  IRec2 = mockery::mock(),
                  R = mockery::mock(),
                  D = mockery::mock()
                  )
  variables <- mockery::mock()
  individuals <- list(human = mockery::mock())

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

  events$exposure  <- list(add_listeners = mockery::mock())
  events$mild_infection <- list(add_listeners = mockery::mock())
  events$severe_infection <- list(add_listeners = mockery::mock())
  events$hospitilisation <- list(add_listeners = mockery::mock())
  events$imv_get_live <- list(add_listeners = mockery::mock())
  events$imv_get_die <- list(add_listeners = mockery::mock())
  events$hospitilisation <- list(add_listeners = mockery::mock())
  events$imv_get_live <- list(add_listeners = mockery::mock())
  events$imv_get_die <- list(add_listeners = mockery::mock())
  events$iox_get_live <- list(add_listeners = mockery::mock())
  events$iox_get_die <- list(add_listeners = mockery::mock())
  events$imv_not_get_live <- list(add_listeners = mockery::mock())
  events$imv_not_get_die <- list(add_listeners = mockery::mock())
  events$iox_not_get_live <- list(add_listeners = mockery::mock())
  events$iox_not_get_die <- list(add_listeners = mockery::mock())
  events$stepdown <- list(add_listeners = mockery::mock())
  events$recovery <- list(add_listeners = mockery::mock())
  events$death <- list(add_listeners = mockery::mock())

  create_event_based_processes(individuals, states, variables, events,
                               parameters)

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

  skip("test create_progression_listener is an empty test and fails")

  events <- mockery::mock()
  event <- mockery::mock()
  shift <- 0
  duration <- 1
  target <- mockery::mock()
  func_mock <- mockery::mock()
  r_erlang_mock <- mockery::mock(c(TRUE, TRUE, TRUE, TRUE))

  ret <- create_progression_listener(
    events,
    duration,
    shift,
    func_mock
  )

  api <- list(schedule = mockery::mock())

  ret(api, target)

  with_mock(
    'hypatia::r_erlang' = r_erlang_mock,
    ret3 <- api$schedule(
      event,
      target,
      rep(.5, 4)
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
    'hypatia::r_erlang' = r_erlang_mock,
    ret <- api$schedule(
      events,
      to_move,
      rep(.2, 4)
    )
  )

})

test_that("test initialise_progression", { #WORKS locally

  event <- mockery::mock()
  human <- mockery::mock()
  from_state <- mockery::mock()
  duration <- 1
  r_erlang_mock <- mockery::mock(c(TRUE, TRUE, TRUE, TRUE))

  ret <- initialise_progression(event, human, from_state, duration)

  api <- list(schedule = mockery::mock(), get_state = mockery::mock())
  target <- mockery::mock()

  ret(api, target)

  mockery::expect_args(api$get_state, 1, human, from_state)

  with_mock(
    'hypatia::r_erlang' = r_erlang_mock,
    ret3 <- api$schedule(
      event,
      target,
      rep(.5, 4)
    )
  )

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

