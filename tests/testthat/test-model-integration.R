test_that("run_simulation can parameterise and run an Afghan model for 10 days", {

  pop <- get_population("Afghanistan")

  time_period = 10
  parameters <- get_parameters(
    pop,
    R0 = 2,
    time_period = time_period,
    tt_contact_matrix = 0,
    contact_matrix_set = squire::contact_matrices[[1]]
  )

  output <- run_simulation(
    time_period,
    pop,
    parameters,
    processes = list()
  )

  expect_setequal(names(output), 'timestep')
  expect_equal(nrow(output), time_period)

})
