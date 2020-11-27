test_that("run_simulation can parameterise and run an Afghan model for 10 days", {

  pop <- get_population("Afghanistan")
  parameters <- get_parameters(
    pop,
    R0 = 2,
    time_period = 10,
    tt_contact_matrix = 0,
    contact_matrix_set = squire::contact_matrices[[1]]
  )

  renderer <- NULL

  output <- run_simulation(
    pop,
    parameters,
    renderer
  )

  expect_setequal(names(output), 'timestep')
  expect_equal(nrow(output), 10)

})
