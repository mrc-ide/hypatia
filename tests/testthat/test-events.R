test_that("create_event_based_processes assigns a listener to each event", {

  pop <- get_population("Antigua and Barbuda")
  contact_matrix_set <- squire::get_mixing_matrix(iso3c = "ATG")
  parameters <- get_parameters(pop, 1, 100, contact_matrix_set)

  events <- create_events()
  states <- create_states(parameters)
  max_age <- 100
  variables <- create_variables(pop, max_age)
  indiv <- create_human(states, variables, events)

  create_event_based_processes(
    indiv$human,
    states,
    variables,
    events,
    parameters
  )

  for (event in events) {
    expect_gt(length(event$listeners), 0)
  }

})

test_that("test that listeners do not call empty targets", {

  events <- create_events()

  states <- list(
    E = individual::State$new("E", 0.5),
    IMild = individual::State$new("IMild", 0.2)
    )

  individuals <- list(human = individual::Individual$new(
    "human", states = list(states$E, states$IMild)))

  parameters <- list(human_population = 100000, average_age = 27)

  initial_age <- trunc(
    r_exp(parameters$human_population, parameters$average_age))

  variables <- list(
    age <- individual::Variable$new("age", 50 - initial_age))


  api <- mock_api(list(), parameters = parameters)

  create_event_based_processes(individuals, states, variables, events,
                               parameters)

  # test a few listeners: test they don't call for an empty target
  events$exposure$listeners[[1]](api, numeric(0))
  mockery::expect_called(api$queue_state_update, 1)

  events$mild_infection$listeners[[2]](api, numeric(0))
  mockery::expect_called(api$queue_state_update, 1)

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

  events <- list(exposure = mockery::mock())
  shift <- 0
  duration <- 1
  target <- mockery::mock()
  func_mock <- mockery::mock()
  r_erlang_mock <- mockery::mock(c(TRUE, TRUE, TRUE, TRUE))

  ret <- create_progression_listener(
    events$exposure,
    duration,
    shift,
    func_mock
  )

  api <- list(schedule = mockery::mock())

  mockery::stub(ret, 'r_erlang', mockery::mock(c(TRUE, TRUE, TRUE, TRUE)))

  with_mock(
    'hypatia::r_erlang' = r_erlang_mock,
    ret2 <- api$schedule(
      events$exposure,
      target,
      rep(.5, 4)
    )
  )

  ret(api, target)

  mockery::expect_args(
    api$schedule,
    1,
    event = events$exposure,
    target = target,
    func_mock = c(0.5, 0.5, 0.5, 0.5)
  )

  mockery::expect_called(api$schedule, 2)

})

test_that("test create_exposure_update_listener for hosp TRUE", {

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
  events <- list(exposure = mockery::mock())
  problist <- seq(1,17,1)
  exposure = mockery::mock()
  parameters <- list(dur_E = mockery::mock(),
                     prob_hosp = list(mockery::mock(problist)))
  to_move <- c(6, 7, 3, 2)
  discrete_age <- mockery::mock()
  target <- 100

  r_erlang_mock <- mockery::mock(c(TRUE, TRUE, TRUE, TRUE))
  mockery::stub(ret, 'r_erlang', mockery::mock(c(TRUE, TRUE, TRUE, TRUE)))

  mockery::stub(ret, 'bernoulli_multi_p', mockery::mock(TRUE))

  mockery::stub(
    ret,
    "hosp",
    mockery::mock()(
      length(to_move),
      parameters$prob_hosp
    )
  )

  with_mock(
    'hypatia::r_erlang' = r_erlang_mock,
    ret2 <- api$schedule(
      events$exposure,
      target,
      rep(.5, 4)
    )
  )

  ret(api, to_move)

  mockery::expect_args(api$get_variable, 1, human, variables$discrete_age,
                       to_move)

  mockery::expect_args(api$schedule, 1, event = events$exposure, target = 100,
    delay = c(0.5, 0.5, 0.5, 0.5))

})

test_that("test create_exposure_update_listener for hosp FALSE", {

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
  events <- list(exposure = mockery::mock())
  problist <- seq(1,17,1)
  exposure = mockery::mock()
  parameters <- list(dur_E = mockery::mock(),
                     prob_hosp = list(mockery::mock(problist)))
  to_move <- c(6, 7, 3, 2)
  discrete_age <- mockery::mock()
  target <- 100

  r_erlang_mock <- mockery::mock(c(TRUE, TRUE, TRUE, TRUE))
  mockery::stub(ret, 'r_erlang', mockery::mock(c(TRUE, TRUE, TRUE, TRUE)))

  mockery::stub(ret, 'bernoulli_multi_p', mockery::mock(FALSE))

  mockery::stub(
    ret,
    "hosp",
    mockery::mock()(
      length(to_move),
      parameters$prob_hosp
    )
  )

  with_mock(
    'hypatia::r_erlang' = r_erlang_mock,
    ret2 <- api$schedule(
      events$exposure,
      target,
      rep(.5, 4)
    )
  )

  ret(api, to_move)

  mockery::expect_args(api$get_variable, 1, human, variables$discrete_age,
                       to_move)

  mockery::expect_args(api$schedule, 1, event = events$exposure, target = 100,
                       delay = c(0.5, 0.5, 0.5, 0.5))

})


test_that("test initialise_progression", {

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

  expect_equal(length(events), 14)

})

