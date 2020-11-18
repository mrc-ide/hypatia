test_that("test Create_states with S", {

  population <- squire::get_population("Afghanistan", simple_SEIR = FALSE)

  psq <- squire::parameters_explicit_SEEIR(
    population = population$n,
    dt = 1,
    R0 = 2,
    tt_contact_matrix = 0,
    time_period = 1000,
    contact_matrix_set = squire::contact_matrices[[1]])

  NR <- 0
  timestep <- 100
  ind <- 2

  Snew <- individual::State$new("S", sum(psq$S_0))

  states <- create_states(psq)

  expect_equal(Snew$initial_size, states[[1]]$initial_size[1])

})

test_that("test create_states", {

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

  dt <- psq$dt

  numberof_days <- 5

  beta <- squire::beta_est_explicit(psq$dur_IMild, psq$dur_ICase,
                                    psq$prob_hosp,
                                    psq$mix_mat_set[1, , ], R0 = R0)


  states <- create_states(psq)

  pe <- sum(psq$E1_0) + sum(psq$E2_0)

  IMild = individual::State$new("IMild", sum(psq$IMild_0))
  E = individual::State$new("E", pe)
  IOxNotGetDie1 = individual::State$new("IOxNotGetDie1",
                                        sum(psq$IOxNotGetDie1_0))
  IRec1 = individual::State$new("IRec1", sum(psq$IRec1_0))

  expect_equal(states$IMild, IMild)
  expect_equal(states$E, E)
  expect_equal(states$IOxNotGetDie1, IOxNotGetDie1)
  expect_equal(states$IRec1, IRec1)

})

test_that("test Probabilities_of_states", {

  population <- squire::get_population("Afghanistan", simple_SEIR = FALSE)

  dt <- 1

  psq <- squire::parameters_explicit_SEEIR(
    population = population$n,
    dt = dt,
    R0 = 2,
    tt_contact_matrix = 0,
    time_period = 1000,
    contact_matrix_set = squire::contact_matrices[[1]])

  pstates <- probabilities_of_states(dt, psq)

  expect_equal(pstates$pgamma_E, 1 - exp(-1.0 * (psq$gamma_E * dt)))

})

test_that("test create_continuous_age_variable", {

  pop <- squire::get_population("France", simple_SEIR = FALSE)

  age_cont <- create_continuous_age_variable(pop)

  expect_equal(length(age_cont), sum(pop$n))

})

test_that("test create_discrete_age_variable", {

  # TO BE DONE LATER
  pop <- squire::get_population("France", simple_SEIR = FALSE)

  expect_error(hypatia:::create_discrete_age_variable(pop), "*")

})
