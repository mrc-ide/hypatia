test_that("create_variables returns the correct output", {

  pop <- get_population("AFG")

  pop$n <- as.integer(pop$n/10000)
  theages <- create_variables(pop, get_parameters("AFG"))
  expect_length(length(theages$age), 1)

  expect_length(theages$age$initial_values, sum(pop$n))

})


test_that("create_continuous_age_variable creates the right number of ages", {

  pop <- get_population("AFG")
  pop$n <- as.integer(pop$n/10000)
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
  ages <- create_continuous_age_variable(pop = pop)
  disc_ages <- create_discrete_age_variable(ages, pop)

  expect_equal(as.numeric(table(disc_ages)), pop$n)
})


test_that("test adjust_seeding_ages_works", {

  # Create our parameters
  pop <- squire::get_population(iso3c = "ATG")
  pop$n <- as.integer(pop$n)/100
  parameters <- get_parameters(
    iso3c = "ATG", population = pop$n
  )

  age_cont <- create_continuous_age_variable(pop)

  # adjust the seeding ages
  actual <- adjust_seeding_ages(
    initial_values = age_cont,
    parameters = parameters
  )

  # checks
  e1 <- parameters$E1_0
  expect_equal(
    tail(actual, 20),
    rep(which(e1>0), e1[e1>0])
  )

})
