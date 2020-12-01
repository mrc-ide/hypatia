test_that("run_simulation can parameterise and run an Afghan model for 10 days", {

  R0 <- 2
  timestep <- 100
  time_period <- 1000
  tt_contact_matrix <- 0
  newpopulation <- 100000
  numberof_days <- 5
  contact_matrix_set <- squire::contact_matrices[[1]]
  pop <- get_population("Afghanistan")

  psq <- get_parameters(
    population = pop$n,
    contact_matrix_set = contact_matrix_set,
    R0 = R0,
    time_period = time_period,
    tt_contact_matrix = tt_contact_matrix
  )

  output <- run_simulation(
    # "Afghanistan",
    # 1000,
    pop,
    psq,
    # contact_matrix_set,
    max_age = 100
  )

  expect_equal(length(output$timestep), 1000)
  expect_equal(nrow(output), 1000)

})

test_that("run_simulation with parameters = NULL", {

  pop <- get_population("Afghanistan")

  output <- run_simulation(
    # "Afghanistan",
    # 1000,
    pop,
    NULL,
    # squire::contact_matrices[[1]],
    max_age = 100
  )

  expect_equal(length(output$timestep), 1000)

})

test_that("run_simulation with timestep = NULL", {

  R0 <- 2
  timestep <- 100
  time_period <- 1000
  tt_contact_matrix <- 0
  newpopulation <- 100000
  numberof_days <- 5
  contact_matrix_set <- squire::contact_matrices[[1]]

  pop <- get_population("Afghanistan")

  psq <- get_parameters(
    population = pop$n,
    contact_matrix_set = contact_matrix_set,
    R0 = R0,
    time_period = time_period,
    tt_contact_matrix = tt_contact_matrix
  )

  output <- run_simulation(
    # "Afghanistan",
    #NULL,
    pop,
    psq,
    #contact_matrix_set,
    max_age = 100
  )

  expect_equal(length(output$timestep), 1000)

})
