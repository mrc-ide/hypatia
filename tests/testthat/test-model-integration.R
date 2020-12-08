test_that("run_simulation can parameterise and run an Afghan model for 10 days", {

  R0 <- 2
  time_period <- 1000
  tt_contact_matrix <- 0

  iso3c <- "AFG"
  pop <- get_population(iso3c)
  pop$n <- as.integer(pop$n / 1000)
  psq <- get_parameters(
    population = pop$n,
    contact_matrix_set = squire::get_mixing_matrix(iso3c = iso3c),
    iso3c = iso3c,
    R0 = R0,
    time_period = time_period
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

test_that("run run_simulation twice using mockery_mock", {

  # simple overrides
  overrides <- list("pop" = list(1,3),
                    "parameters" = list(1,3),
                    "max_age" = list(1,3))

  # get our run function that we will do some mocking to
  run <- run_simulation_replicate

  # mock the function to just return a data frame made up of our overrides
  # to check they are being grabbed correctly
  mockery::stub(
    run,
    'run_simulation',
    function(pop, parameters, max_age) {
      data.frame("timestep" = 1,
                 "pop" = pop,
                 "parameters" = parameters,
                 "max_age" = max_age)
    }
  )

  # run our mocked function
  out <- run(repetitions = 2, overrides = overrides, parallel = FALSE)

  # Now we can check the arguments are correctly filled from overrides
  expect_true(out$repetition[1] == 1)
  expect_true(out$timestep[2] == 1)
  expect_true(out$pop[2] == 3)

})

test_that("run run_simulation twice using mockery_mock - another method", {

  # 2nd Test Type
  # simple overrides
  overrides <- list("pop" = list(1,3),
                    "parameters" = list(1,3),
                    "max_age" = list(1,3))
  # get our run function that we will do some mocking to
  run <- run_simulation_replicate
  # mock the function as just a plain mock
  run_simulation_mock <- mockery::mock(cycle = TRUE)
  mockery::stub(
    run,
    'run_simulation',
    run_simulation_mock
  )
  # run our mocked function

  out <- run(repetitions = 2, overrides = overrides, parallel = FALSE)

  mockery::expect_args(
    run_simulation_mock,
    1,
    pop = overrides$pop[[1]],
    parameters = overrides$parameters[[1]],
    max_age = overrides$max_age[[1]]
  )

  mockery::expect_args(
    run_simulation_mock,
    2,
    pop = overrides$pop[[2]],
    parameters = overrides$parameters[[2]],
    max_age = overrides$max_age[[2]]
  )

})

test_that("run 2 models with run_simulation sequentially on real data", {

  R0 <- 2
  time_period <- 1000
  tt_contact_matrix <- 0

  iso3c <- "ATG"
  pop <- get_population(iso3c)
  pop$n <- as.integer(pop$n / 100)

  psq <- get_parameters(
    population = pop$n,
    contact_matrix_set = squire::get_mixing_matrix(iso3c = iso3c),
    iso3c = iso3c,
    R0 = R0,
    time_period = time_period
  )

  iso3c <- "AFG"
  pop2 <- get_population(iso3c)
  pop2$n <- as.integer(pop2$n / 100)

  psq2 <- get_parameters(
    population = pop2$n,
    contact_matrix_set = squire::get_mixing_matrix(iso3c = iso3c),
    iso3c = iso3c,
    R0 = R0,
    time_period = time_period
  )

  repetitions <- 2
  max_age <- 100

  overrides <-list(pop = list(pop, pop2), parameters = list(psq, psq2),
                          max_age = list(max_age, max_age))


  dfs <- run_simulation_replicate(
    repetitions,
    overrides,
    parallel = FALSE
  )

  expect_equal(length(dfs), 17)
  expect_equal(length(dfs$timestep), 2000)
  expect_equal(dfs$human_S_count[1], 952)
  expect_equal(dfs$human_E_count[1], 20)
  expect_equal(dfs$human_E_count[2], 20)
  expect_true(dfs$repetition[1] == 1)
  expect_true(dfs$timestep[1] == 1)
  expect_true(dfs$timestep[2] == 2)

})
