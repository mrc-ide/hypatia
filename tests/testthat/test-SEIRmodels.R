test_that("check that validated_state_update is working as expected", {

  # How do we check an error message? This is not the correct way

  S <- individual::State$new('S', 10000)

  human <- individual::Individual$new('human', list(S))

  # ret <- hypatia::validate_state_update(api, human, S, 933.5, 10000)
  # expect_equal(ret,'Your index 933.5 for human:S is not an integer type is a double' )
  # ret <- hypatia::validate_state_update(api, human, S, -10, 10000)
  # expect_equal(ret,'Your index -10 for human:S is less than or equal to 0' )
  # ret <- hypatia::validate_state_update(api, human, S, NA, 10000)
  # expect_equal(ret,'Your index NA for human:S is not a number' )
})


test_that("test individual model with 10000 humans with immunity, age and location effects", {
  # Use hypatia::displaythemodel to plot

  pars <- hypatia::get_parameters_for_sirstochastic()

  population <- pars$N
  NI <- pars$I0
  NR <- 2
  pops <- population - NI - NR
  timestep <- pars$num/pars$dt

  S <- individual::State$new('S', pops)
  I <- individual::State$new('I', NI)
  R <- individual::State$new('R', NR)

  immunity <- individual::Variable$new('immunity', rep(0, pars$N))
  age <- individual::Variable$new('age', rep(0, pars$N))
  location <- individual::Variable$new('location', rep(0, pars$N))
  human <- individual::Individual$new('human', list(S, I, R), variables = list(immunity, age, location))

  processes <- list(
    hypatia::individual_S_to_I_2(S, I, human, immunity, age, location, pars),
    hypatia::individual_I_to_R_2(I, R, human, immunity, age, location, pars),
    hypatia::individual_R_to_S_2(S, R, human, immunity, age, location, pars),
    hypatia::render_state_sizes_2(S, I, R, human)
  )

  output <- individual::simulate(human, processes, timestep, parameters = list(immunity_level = .2, age_level=0.3, location_level = 0.4))

  df <-   data.frame(S = output$susceptable_counts, I = output$infected_counts, R = output$recovered_counts,
                                 time = output$time, type = "Individual",  legend = "Individual", stringsAsFactors = FALSE)

  expect_true(is.data.frame(df))

})

test_that("test individual model with 10000 humans with immunity, age and location effects", {

  # Use hypatia::displaythemodel to plot
  pars <- hypatia::get_parameters_for_sirstochastic()
  pars[["includeimmune"]] <- TRUE
  pars[["includeage"]] <- TRUE
  pars[["includelocation"]] <- TRUE
  pars[["novariations"]] <- FALSE

  population <- pars$N
  NI <- pars$I0
  NR <- 2
  pops <- population - NI - NR
  timestep <- pars$num/pars$dt

  S <- individual::State$new('S', pops)
  I <- individual::State$new('I', NI)
  R <- individual::State$new('R', NR)

  immunity <- individual::Variable$new('immunity', runif(population, 0, .1))
  rate=1/pars$average_age
  age <- individual::Variable$new('age', rexp(pars$N, rate))
  location <- individual::Variable$new('location', runif(population, 0, .2))
  human <- individual::Individual$new('human', list(S, I, R), variables = list(immunity, age, location))

  processes <- list(
    hypatia::individual_S_to_I_2(S, I, human, immunity, age, location, pars),
    hypatia::individual_I_to_R_2(I, R, human, immunity, age, location, pars),
    hypatia::individual_R_to_S_2(S, R, human, immunity, age, location, pars),
    hypatia::render_state_sizes_2(S, I, R, human)
  )

  output <- individual::simulate(human, processes, timestep, parameters = list(immunity_level = .2, age_level=0.3, location_level = 0.4))

  df <-   data.frame(S = output$susceptable_counts, I = output$infected_counts, R = output$recovered_counts, time = output$time, type = "Individual",  legend = "Individual", stringsAsFactors = FALSE)

  expect_true(is.data.frame(df))

})

test_that("test individual model with 10000 humans with immunity, age and location effects", {
  # Use hypatia::displaythemodel to plot

  pars <- hypatia::get_parameters_for_sirstochastic()
  pars[["includeimmune"]] <- TRUE
  pars[["includeage"]] <- TRUE
  pars[["includelocation"]] <- TRUE
  pars[["novariations"]] <- FALSE

  population <- pars$N
  NI <- pars$I0
  NR <- 2
  pops <- population - NI - NR
  timestep <- pars$num/pars$dt

  S <- individual::State$new('S', pops)
  I <- individual::State$new('I', NI)
  R <- individual::State$new('R', NR)

  immunity <- individual::Variable$new('immunity', runif(population, 0, .1))
  rate=1/pars$average_age
  age <- individual::Variable$new('age', rexp(pars$N, rate))
  location <- individual::Variable$new('location', runif(population, 0, .2))
  human <- individual::Individual$new('human', list(S, I, R), variables = list(immunity, age, location))

  processes <- list(
    hypatia::individual_S_to_I_2(S, I, human, immunity, age, location, pars),
    hypatia::individual_I_to_R_2(I, R, human, immunity, age, location, pars),
    hypatia::individual_R_to_S_2(S, R, human, immunity, age, location, pars),
    hypatia::render_state_sizes_2(S, I, R, human)
  )

  output <- individual::simulate(human, processes, timestep, parameters = list(immunity_level = .2, age_level=0.3, location_level = 0.4))

  df <-   data.frame(S = output$susceptable_counts, I = output$infected_counts, R = output$recovered_counts, time = output$time, type = "Individual",  legend = "Individual", stringsAsFactors = FALSE)

  expect_true(is.data.frame(df))

})

test_that("2 test individual model with 10000 humans with immunity, age and location effects switched off", {

# Use hypatia::displaythemodel to plot

pars <- hypatia::get_parameters_for_sirstochastic()

population <- pars$N
NI <- pars$I0
NR <- 2
pops <- population - NI - NR
timestep <- pars$num/pars$dt

S <- individual::State$new('S', pops)
I <- individual::State$new('I', NI)
R <- individual::State$new('R', NR)

immunity <- individual::Variable$new('immunity',  rep(0, pars$N))
age  <- individual::Variable$new('age', rep(0, pars$N))
location <- individual::Variable$new('location', rep(0, pars$N))
human <- individual::Individual$new('human', list(S, I, R), variables = list(immunity, age, location))

processes <- list(
  hypatia::individual_S_to_I_2(S, I, human, immunity, age, location, pars),
  hypatia::individual_I_to_R_2(I, R, human, immunity, age, location, pars),
  hypatia::individual_R_to_S_2(S, R, human, immunity, age, location, pars),
  hypatia::render_state_sizes_2(S, I, R, human)
)

output <- individual::simulate(human, processes, timestep)

df <-   data.frame(S = output$susceptable_counts, I = output$infected_counts, R = output$recovered_counts, time = output$time, type = "Individual",  legend = "Individual", stringsAsFactors = FALSE)

expect_true(is.data.frame(df))

})

test_that("test individual model with 10000 humans with immunity, age and location with different states", {
  # I is infected, I2 is severely infected, D is dead
  # Use hypatia::displaythemodel2(df) to plot
  pars <- hypatia::get_parameters_for_sirstochastic()
  pars[["includeimmune"]] <- TRUE
  pars[["includeage"]] <- TRUE
  pars[["includelocation"]] <- TRUE
  pars[["infection_rate"]] <- 0.3
  pars[["novariations"]] <- FALSE

  population <- pars$N
  NI <- pars$I0
  NI2 <- as.integer(pars$I0/5)
  NR <- 0
  ND <- 0
  pops <- population - NI - NR - NI2 - ND
  timestep <- pars$num/pars$dt

  S <- individual::State$new('S', pops)
  I <- individual::State$new('I', NI)
  I2 <- individual::State$new('I2', NI2)
  D <- individual::State$new('D', ND)
  R <- individual::State$new('R', NR)

  immunity <- individual::Variable$new('immunity', runif(population, 0, .1))
  rate=1/pars$average_age
  age <- individual::Variable$new('age', rexp(population, rate))
  location <- individual::Variable$new('location', runif(population, 0, .2))
  human <- individual::Individual$new('human', list(S, I, I2, D, R), variables = list(immunity, age, location))

  processes <- list(
    hypatia::individual_S_to_I_and_I2(S, I, I2, human, immunity, age, location, pars),
    hypatia::individual_I_to_R_I2_to_D(I, R, I2, D, human, immunity, age, location, pars),
    hypatia::individual_R_to_S(R, S, human, immunity, age, location, pars),
    hypatia::render_state_sizes2(S, I, R, I2, D, human)
  )

  output <- individual::simulate(human, processes, timestep , parameters = list(immunity_level = .2, age_level=0.3, location_level = 0.4))

  df <-   data.frame(S = output$susceptable_counts, I = output$infected_counts,
                     I2 = output$severelyinfected_counts, R = output$recovered_counts,
                     D = output$dead_counts, time = output$time, type = "Individual",
                     legend = "Individual", stringsAsFactors = FALSE)

  expect_true(is.data.frame(df))

})

test_that("test individual model with 10000 humans with immunity, age and location siwtched off and with different states", {
  # I is infected, I2 is severely infected, D is dead
  # Use hypatia::displaythemodel2(df) to plot

  pars <- hypatia::get_parameters_for_sirstochastic()

  population <- pars$N
  NI <- pars$I0
  NI2 <- as.integer(pars$I0/5)
  NR <- 0
  ND <- 0
  pops <- population - NI - NR - NI2 - ND
  timestep <- pars$num/pars$dt

  S <- individual::State$new('S', pops)
  I <- individual::State$new('I', NI)
  I2 <- individual::State$new('I2', NI2)
  D <- individual::State$new('D', ND)
  R <- individual::State$new('R', NR)

  immunity <- individual::Variable$new('immunity',  rep(0, pars$N))
  age  <- individual::Variable$new('age', rep(0, pars$N))
  location <- individual::Variable$new('location', rep(0, pars$N))
  human <- individual::Individual$new('human', list(S, I, I2, D, R), variables = list(immunity, age, location))

  processes <- list(
    hypatia::individual_S_to_I_and_I2(S, I, I2, human, immunity, age, location, pars),
    hypatia::individual_I_to_R_I2_to_D(I, R, I2, D, human, immunity, age, location, pars),
    hypatia::individual_R_to_S(R, S, human, immunity, age, location, pars),
    hypatia::render_state_sizes2(S, I, R, I2, D, human)
  )

  output <- individual::simulate(human, processes, timestep) #, parameters = list(immunity_level = .2, age_level=0.3, location_level = 0.4))

  df <-   data.frame(S = output$susceptable_counts, I = output$infected_counts,
                     I2 = output$severelyinfected_counts, R = output$recovered_counts,
                     D = output$dead_counts, time = output$time, type = "Individual",
                     legend = "Individual", stringsAsFactors = FALSE)

  expect_true(is.data.frame(df))

})
