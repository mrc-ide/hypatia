test_that("test run_simulation", {

  R0 <- 2
  timestep <- 100
  dt <- 1
  time_period <- 1000
  tt_contact_matrix <- 0
  newpopulation <- 100000
  numberof_days <- 5

  df <- run_simulation("Afghanistan", R0, timestep, dt, time_period,
                       tt_contact_matrix, newpopulation, numberof_days)

  expect_true(is.data.frame(df))

})
