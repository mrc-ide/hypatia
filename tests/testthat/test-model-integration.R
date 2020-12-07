test_that("run_simulation can parameterise and run an Afghan model for 10 days", {

  R0 <- 2
  time_period <- 1000
  tt_contact_matrix <- 0

  iso3c <- "AFG"
  pop <- get_population(iso3c)
  pop$n <- as.integer(pop$n/1000)
  psq <- get_parameters(
    population = pop$n,
    contact_matrix_set = squire::get_mixing_matrix(iso3c = iso3c),
    iso3c = iso3c,
    R0 = R0,
    time_period = time_period
  )

  output <- run_simulation(
    pop,
    psq,
    max_age = 100
  )

  expect_equal(length(output$timestep), 1000)
  expect_equal(nrow(output), 1000)

})

test_that("run_simulation with parameters = NULL", {

  pop <- get_population("ATG")
  pop$n <- as.integer(pop$n)

  output <- run_simulation(
    pop,
    NULL,
    max_age = 100
  )

  expect_equal(length(output$timestep), 365)

})

test_that("run 2 models with run_simulation", {

  R0 <- 2
  time_period <- 1000
  tt_contact_matrix <- 0

  iso3c <- "ATG"
  pop <- get_population(iso3c)

  psq <- get_parameters(
    population = pop$n,
    contact_matrix_set = squire::get_mixing_matrix(iso3c = iso3c),
    iso3c = iso3c,
    R0 = R0,
    time_period = time_period
  )

  iso3c <- "AFG"
  pop2 <- get_population(iso3c)

  psq2 <- get_parameters(
    population = pop2$n,
    contact_matrix_set = squire::get_mixing_matrix(iso3c = iso3c),
    iso3c = iso3c,
    R0 = R0,
    time_period = time_period
  )

  repetitions <- 2
  max_age <- 100

  overrides <-list(pop = list(pop, pop2), parameters = list(psq, psq2),
                          max_age = list(max_age, max_age))


  dfs <- run_multiple_models(
    repetitions,
    overrides,
    parallel = TRUE
  )


})
