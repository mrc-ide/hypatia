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

test_that("test individual model with 10000 humans with immunity, age and location effects switched off", {

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

#TEST not working - I2 not being calculated
# test_that("test individual model with 10000 humans with immunity, age and location with different states", {
#   # I is infected, I2 is severely infected, D is dead
#   # Use hypatia::displaythemodel2(df) to plot
#   pars <- hypatia::get_parameters_for_sirstochastic()
#   pars[["includeimmune"]] <- TRUE
#   pars[["includeage"]] <- TRUE
#   pars[["includelocation"]] <- TRUE
#   pars[["infection_rate"]] <- 0.3
#   pars[["novariations"]] <- FALSE
#
#   population <- pars$N
#   NI <- pars$I0
#   NI2 <- as.integer(pars$I0/5)
#   NR <- 0
#   ND <- 0
#   pops <- population - NI - NR - NI2 - ND
#   timestep <- pars$num/pars$dt
#
#   S <- individual::State$new('S', pops)
#   I <- individual::State$new('I', NI)
#   I2 <- individual::State$new('I2', NI2)
#   D <- individual::State$new('D', ND)
#   R <- individual::State$new('R', NR)
#
#   immunity <- individual::Variable$new('immunity', runif(population, 0, .1))
#   rate=1/pars$average_age
#   age <- individual::Variable$new('age', rexp(population, rate))
#   location <- individual::Variable$new('location', runif(population, 0, .2))
#   human <- individual::Individual$new('human', list(S, I, I2, D, R), variables = list(immunity, age, location))
#
#   processes <- list(
#     hypatia::individual_S_to_I_and_I2(S, I, I2, human, immunity, age, location, pars),
#     hypatia::individual_I_to_R_I2_to_D(I, R, I2, D, human, immunity, age, location, pars),
#     hypatia::individual_R_to_S(R, S, human, immunity, age, location, pars),
#     hypatia::render_state_sizes2(S, I, R, I2, D, human)
#   )
#
#   output <- individual::simulate(human, processes, timestep , parameters = list(immunity_level = .2, age_level=0.3, location_level = 0.4))
#
#   df <-   data.frame(S = output$susceptable_counts, I = output$infected_counts,
#                      I2 = output$severelyinfected_counts, R = output$recovered_counts,
#                      D = output$dead_counts, time = output$time, type = "Individual",
#                      legend = "Individual", stringsAsFactors = FALSE)
#
#   expect_true(is.data.frame(df))
#
# })

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


test_that("test individual model with SQUIRE states and probabilities for 2nd age group, 5-9", {
  # Use hypatia::displaythemodel3(df) to plot
  warnings()
  pars <- hypatia::get_parameters_for_sirstochastic()

  population <- squire:::get_population("Afghanistan", simple_SEIR = FALSE)

  parssquire <- hypatia::parameters_explicit_SEIR( population = population$n,
                                                   dt = 1,
                                                   R0 = 2,
                                                   tt_contact_matrix = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                                                   time_period = 1000,
                                                   contact_matrix_set=squire::contact_matrices[[1]])
  NR <- 0
  newpopulation <- population$n[2]
  timestep <- 100
  index <- 2

  ###To incorporate
  # n_IMild_R[] <- rbinom(IMild[i], p_IMild_R) # Number of mild infections recovering
  # n_ICase1_ICase2[] <- rbinom(ICase1[i], p_ICase1_ICase2) # Number progressing through the onset but not hospitalised compartment
  # n_ICase2_Hosp[] <- rbinom(ICase2[i], p_ICase2_Hosp) # Number progressing to requiring hospitalisation


  S <- individual::State$new('S', parssquire$S_0[index])
  E1 <- individual::State$new('E1', parssquire$E1_0[index])
  E2 <- individual::State$new('E2', parssquire$E2_0[index])
  IMild <- individual::State$new('IMild', parssquire$IMild_0[index])
  ICase1 <- individual::State$new('ICase1', parssquire$ICase1_0[index])
  ICase2 <- individual::State$new('ICase2', parssquire$ICase2_0[index])
  cum_hosp_inc <- individual::State$new('cum_hosp_inc', NR)
  # cum_ICU_inc <- individual::State$new('cum_ICU_inc', NR)
  IOxGetLive1 <- individual::State$new('IOxGetLive1', parssquire$IOxGetLive1_0[index])
  IOxGetLive2 <- individual::State$new('IOxGetLive2', parssquire$IOxGetLive2_0[index])
  IOxGetDie1 <- individual::State$new('IOxGetDie1', parssquire$IOxGetDie1_0[index])
  IOxGetDie2 <- individual::State$new('IOxGetDie2', parssquire$IOxGetDie2_0[index])
  IOxNotGetLive1 <- individual::State$new('IOxNotGetLive1', parssquire$IOxNotGetLive1_0[index])
  IOxNotGetLive2 <- individual::State$new('IOxNotGetLive2', parssquire$IOxNotGetLive2_0[index])
  IOxNotGetDie1 <- individual::State$new('IOxNotGetDie1', parssquire$IOxNotGetDie1_0[index])
  IOxNotGetDie2 <- individual::State$new('IOxNotGetDie2', parssquire$IOxNotGetDie2_0[index])
  IMVGetLive1 <- individual::State$new('IMVGetLive1', parssquire$IMVGetLive1_0[index])
  IMVGetLive2 <- individual::State$new('IMVGetLive2', parssquire$IMVGetLive2_0[index])
  IMVGetDie1 <- individual::State$new('IMVGetDie1', parssquire$IMVGetDie1_0[index])
  IMVGetDie2 <- individual::State$new('IMVGetDie2', parssquire$IMVGetDie2_0[index])
  IMVNotGetLive1 <- individual::State$new('IMVNotGetLive1', parssquire$IMVNotGetLive1_0[index])
  IMVNotGetLive2 <- individual::State$new('IMVNotGetLive2', parssquire$IMVNotGetLive2_0[index])
  IMVNotGetDie1 <- individual::State$new('IMVNotGetDie1', parssquire$IMVNotGetDie1_0[index])
  IMVNotGetDie2 <- individual::State$new('IMVNotGetDie2', parssquire$IMVNotGetDie2_0[index])
  IRec1 <- individual::State$new('IRec1', parssquire$IRec1_0[index])
  IRec2 <- individual::State$new('IRec2', parssquire$IRec2_0[index])
  R <- individual::State$new('R', parssquire$R_0[index])
  D <- individual::State$new('D', parssquire$D_0[index])
  # D_get <- individual::State$new('D_get', NR)
  # D_not_get <- individual::State$new('D_not_get', NR)

  # immunity <- individual::Variable$new('immunity',  rep(0, population$n[2]))
  # age  <- individual::Variable$new('age', rep(0, population$n[2]))
  # location <- individual::Variable$new('location', rep(0, population$n[2]))
  human <- individual::Individual$new('human', list(S, I, E1, E2, IMild, ICase1, ICase2, cum_hosp_inc, IOxGetLive1, IOxGetLive2, IOxNotGetLive1, IOxNotGetLive2, IOxGetDie1, IOxGetDie2,
                                                    IOxNotGetDie1, IOxNotGetDie2, IMVGetLive1, IMVGetLive2, IMVNotGetLive1, IMVNotGetLive2, IMVGetDie1, IMVGetDie2,
                                                    IMVNotGetDie1, IMVNotGetDie2, IRec1, IRec2, R, D))#, variables = list(immunity, age, location))

  problamda <- NULL
  lambda <- NULL

  processes <- list(
    hypatia::SEIRexplicitparameters2(IMild, ICase1, ICase2, cum_hosp_inc,  parssquire$tt_matrix, parssquire$mix_mat_set[1,index,], parssquire$tt_beta, parssquire$beta_set, index, 17, lambda, problambda),
    individual::fixed_probability_state_change_process('human', S$name, E1$name, 0.1),
    individual::fixed_probability_state_change_process('human', E1$name, E2$name, parssquire$pgamma_E),
    hypatia::E2_IMild(human, IMild, E2, ICase1, parssquire$pgamma_E, parssquire$prob_hosp[index]),
    individual::fixed_probability_state_change_process('human', E2$name, ICase1$name, parssquire$prob_hosp[index]),
    individual::fixed_probability_state_change_process('human', ICase1$name,ICase2$name, parssquire$pgamma_ICase),
    individual::fixed_probability_state_change_process('human', ICase2$name,cum_hosp_inc$name, parssquire$pgamma_ICase),
    individual::fixed_probability_state_change_process('human', IOxGetLive1$name,IOxGetLive2$name, parssquire$pgamma_get_ox_survive),
    individual::fixed_probability_state_change_process('human', IOxGetLive2$name,R$name, parssquire$pgamma_get_ox_survive),
    individual::fixed_probability_state_change_process('human', IOxNotGetLive1$name,IOxNotGetLive2$name, parssquire$pgamma_not_get_ox_survive),
    individual::fixed_probability_state_change_process('human', IOxNotGetLive2$name,R$name, parssquire$pgamma_not_get_ox_survive),
    individual::fixed_probability_state_change_process('human', IOxGetDie1$name,IOxGetDie2$name, parssquire$pgamma_get_ox_die),
    individual::fixed_probability_state_change_process('human', IOxGetDie2$name, D$name, parssquire$pgamma_get_ox_die),
    individual::fixed_probability_state_change_process('human', IOxNotGetDie1$name, IOxNotGetDie2$name, parssquire$pgamma_not_get_ox_die),
    individual::fixed_probability_state_change_process('human', IOxNotGetDie2$name, D$name, parssquire$pgamma_not_get_ox_die),
    individual::fixed_probability_state_change_process('human', IMVGetLive1$name, IMVGetLive2$name, parssquire$pgamma_get_mv_survive),
    individual::fixed_probability_state_change_process('human', IMVGetLive2$name, IRec1$name, parssquire$pgamma_get_mv_survive),
    individual::fixed_probability_state_change_process('human', IMVNotGetLive1$name, IMVNotGetLive2$name, parssquire$pgamma_not_get_mv_survive),
    individual::fixed_probability_state_change_process('human', IMVNotGetLive2$name, R$name, parssquire$pgamma_not_get_mv_survive),
    individual::fixed_probability_state_change_process('human', IMVGetDie1$name, IMVGetDie2$name, parssquire$pgamma_get_mv_die),
    individual::fixed_probability_state_change_process('human', IMVGetDie2$name, D$name,  parssquire$pgamma_get_mv_die),
    individual::fixed_probability_state_change_process('human', IMVNotGetDie1$name, IMVNotGetDie2$name, parssquire$pgamma_not_get_mv_die),
    individual::fixed_probability_state_change_process('human', IMVNotGetDie2$name,D$name, parssquire$pgamma_not_get_mv_die),
    individual::fixed_probability_state_change_process('human', IRec1$name, IRec2$name, parssquire$pgamma_rec),
    individual::fixed_probability_state_change_process('human', IRec2$name, R$name, parssquire$pgamma_rec),

    hypatia::render_state_sizes3(S, E1, E2, I, IMild, ICase1, ICase2, cum_hosp_inc, IOxGetLive1, IOxGetLive2, IOxNotGetLive1, IOxNotGetLive2, IOxGetDie1, IOxGetDie2,
                                 IOxNotGetDie1,IOxNotGetDie2,IMVGetLive1,IMVGetLive2,IMVNotGetLive1,IMVNotGetLive2,IMVGetDie1,IMVGetDie2,
                                 IMVNotGetDie1,IMVNotGetDie2,IRec1,IRec2,R,D,human)
  )

  output <- individual::simulate(human, processes, timestep)

  df <- data.frame(S = output$S, E1 = output$E1, E2 = output$E2, I = output$I, IMild = output$IMild, ICase1 = output$ICase1, ICase2 = output$ICase2, cum_hosp_inc = output$cum_hosp_inc, IOxGetLive1 = output$IOxGetLive1,
                     IOxGetLive2 = output$IOxGetLive2, IOxNotGetLive1 = output$IOxNotGetLive1, IOxNotGetLive2 = output$IOxNotGetLive2,
                     IOxGetDie1 = output$IOxGetDie1, IOxGetDie2 = output$IOxGetDie2,
                     IOxNotGetDie1 = output$IOxNotGetDie1, IOxNotGetDie2 = output$IOxNotGetDie2, IMVGetLive1 = output$IMVGetLive1,
                     IMVGetLive2 = output$IMVGetLive2, IMVNotGetLive1 = output$IMVNotGetLive1, IMVNotGetLive2 = output$IMVNotGetLive2,
                     IMVGetDie1 = output$IMVNotGetLive2, IMVGetDie2 = output$IMVGetDie2,
                     IMVNotGetDie1 = output$IMVNotGetDie1, IMVNotGetDie2 = output$IMVNotGetDie2, IRec1 = output$IRec1, IRec2= output$IRec2,
                     R = output$R, D = output$D, time = output$time, type = "Individual",
                     legend = "Individual", stringsAsFactors = FALSE)

  hypatia:::displaythemodel3(df)

  expect_true(is.data.frame(df))

})
