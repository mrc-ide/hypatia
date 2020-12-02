test_that("run_simulation can parameterise and run an Afghan model for 10 days", {

  R0 <- 2
  time_period <- 1000
  tt_contact_matrix <- 0
  contact_matrix_set <- squire::contact_matrices[[1]]
  pop <- get_population("AFG")

  psq <- get_parameters(
    iso3c = 'AFG',
    population = pop$n,
    contact_matrix_set = contact_matrix_set,
    R0 = R0,
    time_period = time_period,
    tt_contact_matrix = tt_contact_matrix
  )

  output <- run_simulation(
    pop,
    psq,
    max_age = 100
  )

  expect_equal(length(output$timestep), 1000)
  expect_equal(nrow(output), 1000)

})
