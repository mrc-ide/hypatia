test_that("test create_states with S for 1st age group", {

  warnings()
  pars <- hypatia::Get_parameters_for_sirstochastic()

  population <- squire::get_population("Afghanistan", simple_SEIR = FALSE)

  psq <- hypatia::Parameters_explicit_SEIR(
    population = population$n,
    dt = 1,
    R0 = 2,
    tt_contact_matrix = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    time_period = 1000,
    contact_matrix_set = squire::contact_matrices[[1]])

  NR <- 0
  newpopulation <- population$n[2]
  timestep <- 100
  ind <- 2

  Snew <- individual::State$new("S", psq$S_0[1])

  states <- Create_states(psq)

  expect_equal(Snew$initial_size, states[[1]]$initial_size[1])

})
