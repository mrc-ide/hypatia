test_that("test that create_processes and create_dataframes works", {

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

  newpopulation <-10000

  states <- create_states(psq)

  indivs <- create_individuals(states)

  pstates <- probabilities_of_states(dt, psq)

  processes <- create_processes(indivs$human, states, pstates, psq,
                                newpopulation, beta, E_I, numberof_days)

  expect_equal(length(processes), 23)

})
