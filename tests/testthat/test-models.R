test_that("squire SEEIR model runs", {

   pop <- squire::get_population("Afghanistan", simple_SEIR = FALSE)

   set.seed(123)
   r1 <- squire::run_explicit_SEEIR_model(
     population = pop$n,
     dt = 1,
     R0 = 2,
     time_period = 1000,
     replicates = 10,
     contact_matrix_set = squire::contact_matrices[[1]]
   )
  expect_type(r1$output, "double")

  o1 <- squire::format_output(r1)
  expect_true(
    sum(dplyr::filter(o1, t == min(t), replicate == 1)$y) == sum(pop$n))

  uc <- unique(o1$compartment)

  for (i in seq_along(uc)) {
    expect_equal(sum(o1$compartment == uc[i]), 1000 * 10)
  }
  
  set.seed(123)
  
  # Multiple R0s
  r2 <- squire::run_explicit_SEEIR_model(
    population = pop$n,
    dt = 1,
    R0 = c(2, 2),
    tt_R0 = c(0, 10),
    time_period = 1000,
    replicates = 10,
    contact_matrix_set = squire::contact_matrices[[1]]
  )
  expect_identical(r1$output, r2$output)
})

test_that("hypatia squire model runs", {
  
  
  # Create our population
  pop <- squire::get_population(iso3c = "ATG")
  
  # Scale it for speed
  pop$n <- round(pop$n / 10)
  
  # Create our equivalent parameters
  parameters <- squire::parameters_explicit_SEEIR(
    population = pop$n,
    country = "Antigua and Barbuda",
    contact_matrix_set = squire::get_mixing_matrix(iso3c = "ATG"),
  )
  
  # run our simulation
  output <- run_simulation(200, pop, parameters)
  
  # check the output
  expect_is(output, "data.frame")
  
  # check names
  names(output) <- gsub("(^human_)(\\w*)(_count)", "\\2", names(output))
  expect_named(
    output,
    c(
      "timestep",
      "S",
      "E",
      "IMild",
      "ICase",
      "IOxGetLive",
      "IOxGetDie",
      "IOxNotGetLive",
      "IOxNotGetDie",
      "IMVGetLive",
      "IMVGetDie",
      "IMVNotGetLive",
      "IMVNotGetDie",
      "IRec",
      "R",
      "D"
    )
  )
  
})