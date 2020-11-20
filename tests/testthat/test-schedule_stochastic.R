test_that("test scheduler", {

  pop <- squire::get_population("Afghanistan", simple_SEIR = FALSE)

  dt <- 1
  R0 <- 2
  time_period <- 1000
  tt_contact_matrix <- 0

  psq <- squire::parameters_explicit_SEEIR(
    population = pop$n,
    dt = dt,
    R0 = R0,
    tt_contact_matrix = tt_contact_matrix,
    time_period = time_period,
    contact_matrix_set = squire::contact_matrices[[1]])

  events <- create_events()
  states <- create_states(psq)
  variables <- create_variables(psq)
  parameters <- psq
  individuals <- create_individuals(states, variables, events, parameters)

  new_infection_process <- create_infection_process(
    individuals,
    states,
    variables,
    events
  )

  #parameters$severe_enabled = 1

  api <- mock_api(
    list(
      human = list(
        S = mockery::mock(),
        E = mockery::mock(),
        IMild = mockery::mock(),
        ICase1 = mockery::mock(),
      )
    ),
    timestep = 5,
    parameters = parameters
  )

  bernoulli_mock <- mockery::mock(
  #  c(TRUE, FALSE, TRUE, FALSE), # bitten
    c(TRUE),                     # infected
    c(TRUE),                     # clinical
    c(FALSE),                    # severe
    c(FALSE),                    # treatment
    c()                          # treatment successful
  )

  api$get_scheduled = mockery::mock(4, 1)

  with_mock(
    'hypatia:::bernoulli_multi_p' = bernoulli_mock,
    'hypatia:::bernoulli' = mockery::mock(numeric(0)), # mock seek treatment
    new_infection_process(api)
  )

  mockery::expect_called(bernoulli_mock, 4)

  mockery::expect_args(
    api$queue_variable_update,
    1,
    individuals$human,
    variables$ib,
    1.2,
    1
  )
  mockery::expect_args(
    api$queue_variable_update,
    2,
    individuals$human,
    variables$last_boosted_ib,
    5,
    1
  )
  mockery::expect_args(
    api$queue_variable_update,
    3,
    individuals$human,
    variables$is_severe,
    FALSE,
    3
  )
  mockery::expect_args(
    api$schedule,
    1,
    events$clinical_infection,
    3,
    12
  )
  mockery::expect_args(
    api$schedule,
    2,
    events$infection,
    3,
    12
  )

})
