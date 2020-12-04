test_that("run_simulation can parameterise and run an Afghan model for 10 days", {

  R0 <- 2
  time_period <- 1000
  tt_contact_matrix <- 0

  pop <- get_population("AFG")

  psq <- get_parameters(
    iso3c = "AFG",
    population = pop$n,
    R0 = R0,
    time_period = time_period,
    tt_contact_matrix = tt_contact_matrix,
    contact_matrix = squire::contact_matrices[[1]]
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
