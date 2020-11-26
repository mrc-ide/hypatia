test_that("test get_parameters returns the correct values from SQUIRE", {

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

  expect_equal(psq$dt, 1)
  expect_equal(psq$dur_E, 4.6)
  expect_equal(psq$N_age, 17)
  expect_equal(length(psq$S_0), 17)
  expect_equal(length(psq$IRec1_0), 17)
  expect_equal(psq$time_period, 1000)
})
