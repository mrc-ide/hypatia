test_that("run_simulation can parameterise and run an French model for 10 days", {

  expected_columns <- c('timestep')
  pop <- get_population("France")
  parameters <- get_parameters(
    pop,
    R0 = 2,
    time_period = 10,
    tt_contact_matrix = 0,
    contact_matrix_set = squire::contact_matrices[[1]]
  )

  output <- run_simulation(
    pop,
    parameters
  )

  expect_setequal(names(output), expected_columns)
  expect_equal(nrow(output), 10)

})
