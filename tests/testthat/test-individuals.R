test_that("test that create_individuals works", {

  # Get population information
  pop <- get_population("Afghanistan")
  pop$n <- as.integer(pop$n/1000)
  R0 <- 2
  time_period <- 1000
  tt_contact_matrix <- 0

  psq <- get_parameters(
    country = "Afghanistan",
    population = pop$n,
    R0 = R0,
    time_period = time_period,
    tt_contact_matrix = tt_contact_matrix,
    contact_matrix_set = squire::contact_matrices[[1]]
  )

  beta <- squire::beta_est_explicit(psq$dur_IMild, psq$dur_ICase,
                                    psq$prob_hosp,
                                    psq$mix_mat_set[1, , ], R0 = R0)

  states <- create_states(psq)
  variables <- create_variables(pop, max_age = 100)
  events <- create_events()

  indivs <- create_human(states, variables, events)

  # Create test human
  human <- individual::Individual$new(
    "human",
    states = list(
      states$S,
      states$E,
      states$IMild,
      states$ICase,
      states$IOxGetLive,
      states$IOxGetDie,
      states$IOxNotGetLive,
      states$IOxNotGetDie,
      states$IMVGetLive,
      states$IMVGetDie,
      states$IMVNotGetLive,
      states$IMVNotGetDie,
      states$IRec,
      states$R,
      states$D),
    variables = variables,
    events = events
  )

  expect_equal(indivs$name, human$name)
  expect_equal(length(indivs$states), length(human$states))

})

test_that("test Create_states with S for 1st age group", {

  pop <- get_population("Afghanistan")
  pop$n <- as.integer(pop$n/1000)
  R0 <- 2
  time_period <- 1000
  tt_contact_matrix <- 0

  psq <- get_parameters(
    country = "Afghanistan",
    population = pop$n,
    R0 = R0,
    time_period = time_period,
    tt_contact_matrix = tt_contact_matrix,
    contact_matrix_set = squire::contact_matrices[[1]]
  )

  Snew <- individual::State$new("S", sum(psq$S_0))

  states <- create_states(psq)

  expect_equal(Snew$initial_size, states[[1]]$initial_size[1])

})
