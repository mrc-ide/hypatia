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


test_that("small test individual model with SQUIRE states", {
  # Use hypatia::displaythemodel3(df) to plot
  pars <- hypatia::get_parameters_for_sirstochastic()

  population <- squire:::get_population("Afghanistan", simple_SEIR = FALSE)

  parssquire <- hypatia::parameters_explicit_SEIR( population = population$n,
                                                   dt = 1,
                                                   R0 = 2,
                                                   time_period = 1000,
                                                   contact_matrix_set=squire::contact_matrices[[1]])

  newpopulation <- population$n[2]
  NI <- pars$I0
  NE1 <- 1
  NE2 <- 2
  NR <- 0
  NImild <- 0
  NICase1 <- 0
  pops <- newpopulation - NI - NE1 - NE2 - NR
  timestep <- 100

  S <- individual::State$new('S', pops)
  I <- individual::State$new('I', NI)
  E1 <- individual::State$new('E1', NE1)
  E2 <- individual::State$new('E2', NE2)
  Imild <- individual::State$new('Imild', NR)
  ICase1 <- individual::State$new('ICase1', NR)
  ICase2 <- individual::State$new('ICase2', NR)

  immunity <- individual::Variable$new('immunity',  rep(0, newpopulation))
  age  <- individual::Variable$new('age', rep(0, newpopulation))
  location <- individual::Variable$new('location', rep(0, newpopulation))
  human <- individual::Individual$new('human', list(S, I, E1, E2), variables = list(immunity, age, location))


  pgamma_E <- 1 - exp(-1.0*(parssquire$gamma_E * parssquire$dt))

  lambda <- 0.0 #Set FOI to 0.0
  problambda <- 0.0 #Set to 0.0

  inf <-  hypatia::infection(IMild, ICase1, Icase2, parssquire$dt, lambda, problambda)

  processes <- list(
    individual::fixed_probability_state_change_process('human', S$name, E1$name, inf$problambda),
    individual::fixed_probability_state_change_process('human', E1$name, E2$name, pgamma_E),
    individual::fixed_probability_state_change_process('human', E2$name, I$name,  pgamma_E),
    hypatia::render_state_sizes4(S, E1, E2, I,human)
  )

  output <- individual::simulate(human, processes, timestep)

  df <- data.frame(S = output$S, E1 = output$E1, E2 = output$E2, I = output$I,
                   time = output$time, type = "Individual",
                   legend = "Individual", stringsAsFactors = FALSE)

  expect_true(is.data.frame(df))

})


test_that("test individual model with SQUIRE states", {
  # Use hypatia::displaythemodel3(df) to plot
  warnings()
  pars <- hypatia::get_parameters_for_sirstochastic()

  population <- squire:::get_population("Afghanistan", simple_SEIR = FALSE)

  parssquire <- hypatia::parameters_explicit_SEIR( population = population$n,
                                                   dt = 1,
                                                   R0 = 2,
                                                   time_period = 1000,
                                                   contact_matrix_set=squire::contact_matrices[[1]])

  newpopulation <- population$n[2]
  NI <- pars$I0
  NE1 <- 1
  NE2 <- 2
  NR <- 0
  NImild <- 0
  NICase1 <- 0
  pops <- newpopulation - NI - NE1 - NE2 - NR
  timestep <- 100

  S <- individual::State$new('S', pops)
  I <- individual::State$new('I', NI)
  E1 <- individual::State$new('E1', NE1)
  E2 <- individual::State$new('E2', NE2)
  Imild <- individual::State$new('Imild', NR)
  ICase1 <- individual::State$new('ICase1', NR)
  ICase2 <- individual::State$new('ICase2', NR)
  Hosp <- individual::State$new('Hosp', NR)
  # cum_hosp_inc <- individual::State$new('cum_hosp_inc', NR)
  # cum_ICU_inc <- individual::State$new('cum_ICU_inc', NR)
  IOxGetLive1 <- individual::State$new('IOxGetLive1', NR)
  IOxGetLive2 <- individual::State$new('IOxGetLive2', NR)
  IOxGetDie1 <- individual::State$new('IOxGetDie1', NR)
  IOxGetDie2 <- individual::State$new('IOxGetDie2', NR)
  IOxNotGetLive1 <- individual::State$new('IOxNotGetLive1', NR)
  IOxNotGetLive2 <- individual::State$new('IOxNotGetLive2', NR)
  IOxNotGetDie1 <- individual::State$new('IOxNotGetDie1', NR)
  IOxNotGetDie2 <- individual::State$new('IOxNotGetDie2', NR)
  IMVGetLive1 <- individual::State$new('IMVGetLive1', NR)
  IMVGetLive2 <- individual::State$new('IMVGetLive2', NR)
  IMVGetDie1 <- individual::State$new('IMVGetDie1', NR)
  IMVGetDie2 <- individual::State$new('IMVGetDie2', NR)
  IMVNotGetLive1 <- individual::State$new('IMVNotGetLive1', NR)
  IMVNotGetLive2 <- individual::State$new('IMVNotGetLive2', NR)
  IMVNotGetDie1 <- individual::State$new('IMVNotGetDie1', NR)
  IMVNotGetDie2 <- individual::State$new('IMVNotGetDie2', NR)
  IRec1 <- individual::State$new('IRec1', NR)
  IRec2 <- individual::State$new('IRec2', NR)
  R <- individual::State$new('R', NR)
  D <- individual::State$new('D', NR)
  D_get <- individual::State$new('D_get', NR)
  D_not_get <- individual::State$new('D_not_get', NR)

  immunity <- individual::Variable$new('immunity',  rep(0, newpopulation))
  age  <- individual::Variable$new('age', rep(0, newpopulation))
  location <- individual::Variable$new('location', rep(0, newpopulation))
  human <- individual::Individual$new('human', list(S, I, E1, E2, ICase1, ICase2, Hosp, IOxGetLive1, IOxGetLive2, IOxNotGetLive1, IOxNotGetLive2, IOxGetDie1, IOxGetDie2,
                                                    IOxNotGetDie1, IOxNotGetDie2, IMVGetLive1, IMVGetLive2, IMVNotGetLive1, IMVNotGetLive2, IMVGetDie1, IMVGetDie2,
                                                    IMVNotGetDie1, IMVNotGetDie2, IRec1, IRec2, R, D), variables = list(immunity, age, location))


  pgamma_E <- 1 - exp(-1.0*(parssquire$gamma_E * parssquire$dt))
  pgamma_ICase <- 1 - exp(-1.0*(parssquire$gamma_ICase * parssquire$dt))
  pgamma_get_ox_survive <- 1 - exp(-1.0*(parssquire$gamma_get_ox_survive * parssquire$dt))
  pgamma_not_get_ox_survive <- 1 - exp(-1.0*(parssquire$gamma_not_get_ox_survive * parssquire$dt))
  pgamma_get_ox_die <- 1 - exp(-1.0*(parssquire$gamma_get_ox_die * parssquire$dt))
  pgamma_not_get_ox_die <- 1 - exp(-1.0*(parssquire$gamma_not_get_ox_die * parssquire$dt))
  pgamma_get_mv_survive <- 1 - exp(-1.0*(parssquire$gamma_get_mv_survive * parssquire$dt))
  pgamma_not_get_mv_survive <- 1 - exp(-1.0*(parssquire$gamma_not_get_mv_survive * parssquire$dt))
  pgamma_get_mv_die <- 1 - exp(-1.0*(parssquire$gamma_get_mv_die * parssquire$dt))
  pgamma_not_get_mv_die <-  1 - exp(-1.0*(parssquire$gamma_not_get_mv_die * parssquire$dt))
  pgamma_rec <- 1 - exp(-1.0*(parssquire$gamma_rec * parssquire$dt))

  lambda <- 0.0 #Set FOI to 0.0
  problambda <- 0.0 #Set to 0.0

  inf <-  hypatia::infection(IMild, ICase, Icase2, parssquire$dt, lambda, problambda)

  processes <- list(
    individual::fixed_probability_state_change_process('human', S$name, E1$name, inf$problambda),
    individual::fixed_probability_state_change_process('human', E1$name, E2$name, pgamma_E),
    individual::fixed_probability_state_change_process('human', E2$name, I$name,  pgamma_E),
    individual::fixed_probability_state_change_process('human', E2$name, ICase1$name, parssquire$prob_hosp[2]),
    #E2_to_IMild
    individual::fixed_probability_state_change_process('human', ICase1$name,ICase2$name, pgamma_ICase),
    individual::fixed_probability_state_change_process('human', ICase2$name,Hosp$name, pgamma_ICase),
    individual::fixed_probability_state_change_process('human', IOxGetLive1$name,IOxGetLive2$name, pgamma_get_ox_survive),
    individual::fixed_probability_state_change_process('human', IOxGetLive2$name,R$name, pgamma_get_ox_survive),
    individual::fixed_probability_state_change_process('human', IOxNotGetLive1$name,IOxNotGetLive2$name, pgamma_not_get_ox_survive),
    individual::fixed_probability_state_change_process('human', IOxNotGetLive2$name,R$name, pgamma_not_get_ox_survive),
    individual::fixed_probability_state_change_process('human', IOxGetDie1$name,IOxGetDie2$name, pgamma_get_ox_die),
    individual::fixed_probability_state_change_process('human', IOxGetDie2$name, D$name, pgamma_get_ox_die),
    individual::fixed_probability_state_change_process('human', IOxNotGetDie1$name, IOxNotGetDie2$name, pgamma_not_get_ox_die),
    individual::fixed_probability_state_change_process('human', IOxNotGetDie2$name, D$name, pgamma_not_get_ox_die),
    individual::fixed_probability_state_change_process('human', IMVGetLive1$name, IMVGetLive2$name, pgamma_get_mv_survive),
    individual::fixed_probability_state_change_process('human', IMVGetLive2$name, IRec1$name, pgamma_get_mv_survive),
    individual::fixed_probability_state_change_process('human', IMVNotGetLive1$name, IMVNotGetLive2$name, pgamma_not_get_mv_survive),
    individual::fixed_probability_state_change_process('human', IMVNotGetLive2$name, R$name, pgamma_not_get_mv_survive),
    individual::fixed_probability_state_change_process('human', IMVGetDie1$name, IMVGetDie2$name, pgamma_get_mv_die),
    individual::fixed_probability_state_change_process('human', IMVGetDie2$name, D$name,  pgamma_get_mv_die),
    individual::fixed_probability_state_change_process('human', IMVNotGetDie1$name, IMVNotGetDie2$name, pgamma_not_get_mv_die),
    individual::fixed_probability_state_change_process('human', IMVNotGetDie2$name,D$name, pgamma_not_get_mv_die),
    individual::fixed_probability_state_change_process('human', IRec1$name, IRec2$name, pgamma_rec),
    individual::fixed_probability_state_change_process('human', IRec2$name, R$name, pgamma_rec),

    hypatia::render_state_sizes3(S, E1, E2, I, ICase1, ICase2, Hosp, IOxGetLive1, IOxGetLive2, IOxNotGetLive1, IOxNotGetLive2, IOxGetDie1, IOxGetDie2,
                                 IOxNotGetDie1,IOxNotGetDie2,IMVGetLive1,IMVGetLive2,IMVNotGetLive1,IMVNotGetLive2,IMVGetDie1,IMVGetDie2,
                                 IMVNotGetDie1,IMVNotGetDie2,IRec1,IRec2,R,D,human)
  )

  output <- individual::simulate(human, processes, timestep)

  df <- data.frame(S = output$S, E1 = output$E1, E2 = output$E2, I = output$I, ICase1 = output$ICase1, ICase2 = output$ICase2, Hosp = output$Hosp, IOxGetLive1 = output$IOxGetLive1,
                     IOxGetLive2 = output$IOxGetLive2, IOxNotGetLive1 = output$IOxNotGetLive1, IOxNotGetLive2 = output$IOxNotGetLive2,
                     IOxGetDie1 = output$IOxGetDie1, IOxGetDie2 = output$IOxGetDie2,
                     IOxNotGetDie1 = output$IOxNotGetDie1, IOxNotGetDie2 = output$IOxNotGetDie2, IMVGetLive1 = output$IMVGetLive1,
                     IMVGetLive2 = output$IMVGetLive2, IMVNotGetLive1 = output$IMVNotGetLive1, IMVNotGetLive2 = output$IMVNotGetLive2,
                     IMVGetDie1 = output$IMVNotGetLive2, IMVGetDie2 = output$IMVGetDie2,
                     IMVNotGetDie1 = output$IMVNotGetDie1, IMVNotGetDie2 = output$IMVNotGetDie2, IRec1 = output$IRec1, IRec2= output$IRec2,
                     R = output$R, D = output$D, time = output$time, type = "Individual",
                     legend = "Individual", stringsAsFactors = FALSE)

  hypatia::displaythemodel3(df)

  expect_true(is.data.frame(df))

})
