test_that("test get_parameters return the correct values from SQUIRE", {

  R0 <- 2
  timestep <- 100
  dt <- 1
  time_period <- 1000
  tt_contact_matrix <- 0
  newpopulation <- 100000
  numberof_days <- 5

  psq <- get_parameters("Afghanistan", R0, timestep, dt, time_period,
                       tt_contact_matrix, newpopulation, numberof_days)


  expect_equal(psq$dt, 1)
  expect_equal(psq$dur_E, 4.6)
  expect_equal(psq$N_age, 17)
  expect_equal(length(psq$S_0), 17)
  expect_equal(length(psq$IRec1_0), 17)
})
