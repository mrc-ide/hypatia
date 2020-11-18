test_that("test that create_individuals works", {

  # Get human from SQUIRE parameters
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

  # Create test human
  human <- individual::Individual$new(
    "human",
    states = list(
      states$S,
      states$E,
      states$IMild,
      states$ICase1,
      states$ICase2,
      states$IOxGetLive1,
      states$IOxGetLive2,
      states$IOxGetDie1,
      states$IOxGetDie2,
      states$IOxNotGetLive1,
      states$IOxNotGetLive2,
      states$IOxNotGetDie1,
      states$IOxNotGetDie2,
      states$IMVGetLive1,
      states$IMVGetLive2,
      states$IMVGetDie1,
      states$IMVGetDie2,
      states$IMVNotGetLive1,
      states$IMVNotGetLive2,
      states$IMVNotGetDie1,
      states$IMVNotGetDie2,
      states$IRec1,
      states$IRec2,
      states$R,
      states$D)
  )

  expect_equal(indivs$human, human)

})
