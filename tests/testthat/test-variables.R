
test_that("create_continuous_age_variable creates the right number of ages", {
  pop <- get_population("Afghanistan")
  age <- create_continuous_age_variable(pop, max_age = 100)
  ages <- create_continuous_age_variable(pop)
  expect_length(ages, sum(pop$n))
})
