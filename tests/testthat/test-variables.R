test_that("create_variables returns the correct output", {

  pop <- get_population("Afghanistan")
  pop$n <- as.integer(pop$n/1000)

  theages <- create_variables(pop, 90)
  expect_length(length(theages$age), 1)
  expect_length(length(theages$discrete_age), 1)

})


test_that("create_continuous_age_variable creates the right number of ages", {

  pop <- get_population("Afghanistan")
  pop$n <- as.integer(pop$n/1000)

  age <- create_continuous_age_variable(pop, max_age = 100)
  ages <- create_continuous_age_variable(pop)
  expect_length(ages, sum(pop$n))

})

test_that("test create_continuous_age_variable", {

  pop <- squire::get_population(iso3c = "ATG", simple_SEIR = FALSE)
  pop$n <- as.integer(pop$n/1000)

  age_cont <- create_continuous_age_variable(pop)

  expect_equal(length(age_cont), sum(pop$n))

})

test_that("test create_discrete_age_variable", {

  pop <- squire::get_population(iso3c = "ATG", simple_SEIR = FALSE)
  pop$n <- as.integer(pop$n/1000)

  ages <- create_continuous_age_variable(pop = pop, max_age = 100)
  disc_ages <- create_discrete_age_variable(ages, pop)

  expect_true(all(as.numeric(table(disc_ages)) == pop$n))

})