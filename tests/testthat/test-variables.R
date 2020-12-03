test_that("create_variables returns the correct output", {

  pop <- get_population("Afghanistan")

  pop$n <- as.integer(pop$n/10000)
  theages <- create_variables(pop, 90)
  expect_length(length(theages$age), 1)

  expect_length(length(theages$discrete_age), 1)

})


test_that("create_continuous_age_variable creates the right number of ages", {

  pop <- get_population("Afghanistan")
  pop$n <- as.integer(pop$n/10000)
  age <- create_continuous_age_variable(pop, max_age = 100)
  ages <- create_continuous_age_variable(pop)
  expect_length(ages, sum(pop$n))

})

test_that("test create_continuous_age_variable", {

  pop <- squire::get_population(iso3c = "ATG", simple_SEIR = FALSE)
  pop$n <- as.integer(pop$n/100)
  age_cont <- create_continuous_age_variable(pop)

  expect_equal(length(age_cont), sum(pop$n))

})

test_that("test create_discrete_age_variable", {

  pop <- squire::get_population(iso3c = "ATG", simple_SEIR = FALSE)
  pop$n <- as.integer(pop$n/100)
  ages <- create_continuous_age_variable(pop = pop, max_age = 100)
  disc_ages <- create_discrete_age_variable(ages, pop)

  expect_equal(as.numeric(table(disc_ages)), pop$n)

})


test_that("test adjust_seeding_ages_works", {

  # Create our parameters
  pop <- squire::get_population(iso3c = "ATG")
  pop$n <- as.integer(pop$n)/100
  parameters <- get_parameters(
    population = pop$n, contact_matrix_set = squire::contact_matrices[1]
  )

  # Create our variables
  variables <- create_variables(pop)

  # adjust the seeding ages
  variables$discrete_age$initial_values <- adjust_seeding_ages(
    initial_values = variables$discrete_age$initial_values,
    parameters = parameters
  )

  # checks
  e1 <- parameters$E1_0
  expect_equal(
    tail(variables$discrete_age$initial_values, 20),
    rep(which(e1>0), e1[e1>0])
  )

})
