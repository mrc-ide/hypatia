test_that("test that listeners do not call empty targets", {

  events <- create_events()
  individuals <- list(human = individual::Individual$new(
    "human", states = list(states$S, states$IMild)))

  states <- list(
    S = individual::State$new("S", 0.5),
    IMild = individual::State$new("IMild", 0.2)
    )

  variables <- list()
  parameters <- list()

  api <- mock_api(list(), parameters = parameters)

  create_event_based_processes(individuals, states, variables, events,
                               parameters)

  # test a few listeners: test they don't call for an empty target
  events$exposure$listeners[[2]](api, numeric(0))
  mockery::expect_called(api$queue_variable_update, 0)

  events$mild_infection$listeners[[2]](api, numeric(0))
  mockery::expect_called(api$queue_variable_update, 0)

})

test_that("test that listeners return a value", {

  events <- create_events()
  individuals <- list(human = individual::Individual$new(
    "human", states = list(S = S, E = E, IMild = IMild)))

  states <- list(
    S = individual::State$new("S", 0.5),
    E = individual::State$new("E", 0.3),
    IMild = individual::State$new("IMild", 0.2)
  )

  variables <- list()
  parameters <- list()

  api <- mock_api(list(), parameters = parameters)

  create_event_based_processes(individuals, states, variables, events,
                               parameters)


  events$exposure$add_listener(api, c(human, states$E))

  mockery::expect_args(
    api$queue_state_update,
    1,
    individuals$human,
    states$E,
    states$S,
    c(2, 4)
  )

  events$mild_infection$add_listeners(api, c(5, 6))



  events$exposure$listeners[[2]](api, c(2,4))
  #mockery::expect_called(api$queue_variable_update, 0)

  events$mild_infection$listener[[2]](api, c(5, 6))
  #mockery::expect_called(api$queue_variable_update, 0)

})




test_that("test all listeners", {

  parameters <- list()

  events <- create_events()
  states <- list(E = E, S = S)
  variables <- list( discrete_age = seq(1,17,1))

  individuals <-
    human = list(
      E = c(1),
      S = c(2)
    )

  create_event_based_processes(
    individuals,
    states,
    variables,
    events,
    parameters
  )

  api <- mock_api(
    list(
      human = list(
        U = c(1),
        A = c(2),
        S = c(4),
        D = c(3),
        birth = c(2, -365 * 20, -365 * 5, -365 * 7)
      )
    ),
    timestep = 50,
    parameters = parameters
  )

  listener <-   events$exposure$add_listeners()
  #events$mda_enrollment$listeners[[1]]

  m <- mockery::mock()
  mockery::stub(listener, 'administer_listener', m)
  with_mock(
    "malariasimulation:::bernoulli" = mockery::mock(c(1, 2)),
    listener(api, NULL)
  )


  bernoulli_mock <- mockery::mock(c(FALSE, TRUE, TRUE))
  mockery::stub(process, 'bernoulli_multi_p', bernoulli_mock)











  parameters <- mockery::mock()
  states <- list()

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

  api <- list()

  events$exposure$add_listeners(api, c(2, 4))
  events$mild_infection $add_listeners(api, c(2, 4))
  events$severe_infection$add_listeners(api, c(2, 4))
  events$hospitilisation$add_listeners(api, c(2, 4))
  events$imv_get_live$add_listeners(api, c(2, 4))
  events$imv_get_die$add_listeners(api, c(2, 4))
  events$hospitilisation$add_listeners(api, c(2, 4))
  events$imv_get_live$add_listeners(api, c(2, 4))
  events$imv_get_die$add_listeners(api, c(2, 4))
  events$iox_get_live$add_listeners(api, c(2, 4))
  events$iox_get_die$add_listeners(api, c(2, 4))
  events$imv_not_get_live $add_listeners(api, c(2, 4))
  events$imv_not_get_die $add_listeners(api, c(2, 4))
  events$iox_not_get_live$add_listeners(api, c(2, 4))
  events$iox_not_get_die$add_listeners(api, c(2, 4))
  events$stepdown$add_listeners(api, c(2, 4))
  events$recovery$add_listeners(api, c(2, 4))
  events$death$add_listeners(api, c(2, 4))

})

test_that("test create_infection_update_listener", {

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

test_that("test create_progression_listener", {

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

test_that("test create_exposure_update_listener", {

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

test_that("test initialise_progression", { #

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

  expect_equal(events$exposure,
               individual::Event$new("exposure"))
  expect_equal(events$mild_infection ,
               individual::Event$new("mild_infection"))
  expect_equal(events$severe_infection ,
               individual::Event$new("severe_infection"))
  expect_equal(events$hospitilisation, individual::Event$new("hospitilisation"))
  expect_equal(events$imv_get_live, individual::Event$new("imv_get_live"))
  expect_equal(events$imv_get_die, individual::Event$new("imv_get_die"))
  expect_equal(events$iox_get_live, individual::Event$new("iox_get_live"))
  expect_equal(events$iox_get_die, individual::Event$new("iox_get_die"))
  expect_equal(events$imv_not_get_live,
               individual::Event$new("imv_not_get_live"))
  expect_equal(events$imv_not_get_die,
               individual::Event$new("imv_not_get_die"))
  expect_equal(events$iox_not_get_live,
               individual::Event$new("iox_not_get_live"))
  expect_equal(events$iox_not_get_die, individual::Event$new("iox_not_get_die"))
  expect_equal(events$stepdown, individual::Event$new("stepdown"))
  expect_equal(events$recovery, individual::Event$new("recovery"))
  expect_equal(events$death, individual::Event$new("deaths"))

  expect_equal(length(events), 15)

})

