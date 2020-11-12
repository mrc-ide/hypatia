test_that("check that probability_of_infection is 0 if lamdba is empty", {

  warnings()
  pars <- hypatia::get_parameters_for_sirstochastic()

  population <- squire::get_population("Afghanistan", simple_SEIR = FALSE)

  dt <- 1

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

  states <- create_states(psq)

  indivs <- create_individuals(states, variables)

  pstates <- probabilities_of_states(dt, psq)

  infection_process(indivs$human, NULL, NULL, NULL, states$S, states$E1, ind,
                    population$n[2], 0.0, psq$mix_mat_set[1, ind ,], psq$dt)
  probability_of_infection <- 0
  expect_equal(probability_of_infection, 0)

})
