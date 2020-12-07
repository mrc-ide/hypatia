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
