test_that("test Create_states with S for 1st age group", {

  warnings()
  pars <- hypatia::Get_parameters_for_sirstochastic()

  population <- squire::get_population("Afghanistan", simple_SEIR = FALSE)

  psq <- squire::parameters_explicit_SEEIR(
    population = population$n,
    dt = 1,
    R0 = 2,
    tt_contact_matrix = 0,
    time_period = 1000,
    contact_matrix_set = squire::contact_matrices[[1]])

  NR <- 0
  newpopulation <- population$n[2]
  timestep <- 100
  ind <- 2

  Snew <- individual::State$new("S", sum(psq$S_0))

  states <- Create_states(psq)

  expect_equal(Snew$initial_size, states[[1]]$initial_size[1])

})

test_that("test Probabilities_of_states", {

  warnings()
  pars <- hypatia::Get_parameters_for_sirstochastic()

  population <- squire::get_population("Afghanistan", simple_SEIR = FALSE)

  dt <- 1

  psq <- squire::parameters_explicit_SEEIR(
    population = population$n,
    dt = dt,
    R0 = 2,
    tt_contact_matrix = 0,
    time_period = 1000,
    contact_matrix_set = squire::contact_matrices[[1]])

  pstates <- Probabilities_of_states(dt, psq)

  expect_equal(pstates$pgamma_E, 1 - exp(-1.0 * (psq$gamma_E * dt)))

})
