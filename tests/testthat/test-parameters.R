test_that("test get_parameters returns the correct values from SQUIRE", {

  R0 <- 2
  time_period <- 1000
  tt_contact_matrix <- 0
  contact_matrix_set <- squire::contact_matrices[[1]]

  pop <- get_population("Antigua and Barbuda")

  # psq <- get_parameters(
  #   "Afghanistan",
  #   pop,
  #   contact_matrix_set,
  #   time_period  #,
  #   # tt_contact_matrix = tt_contact_matrix,
  #   # R0 = R0
  # )

  psq <- get_parameters("Antigua and Barbuda", pop,
                               squire::get_mixing_matrix(iso3c = "ATG"),
                               100)

  expect_equal(psq$dt, 1)
  expect_equal(psq$dur_E, 4.6)
  expect_equal(psq$N_age, 17)
  expect_equal(length(psq$S_0), 17)
  expect_equal(length(psq$IRec1_0), 17)
  expect_equal(psq$time_period, 1000)

})
