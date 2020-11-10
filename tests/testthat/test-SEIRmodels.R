test_that("check that Validated_state_update is working as expected", {

  human <- mockery::mock()
  S <- mockery::mock()

  api <- list(
    queue_state_update = mockery::mock()
  )

  expect_error(hypatia::Validated_state_update(api, human, S, 10001, 10000), "*")
  expect_error(hypatia::Validated_state_update(api, human, S, 933.5, 10000), "*")
  expect_error(hypatia::Validated_state_update(api, human, S, -10, 10000), "*")
  expect_error(hypatia::Validated_state_update(api, human, S, NA, 10000), "*")
})

test_that("test individual model with 10000 humans with immunity, age and
          location effects", {
  # Use hypatia::displaythemodel to plot

  pars <- hypatia::Get_parameters_for_sirstochastic()

  population <- pars$N
  NI <- pars$I0
  NR <- 2
  pops <- population - NI - NR
  timestep <- pars$num/pars$dt

  S <- individual::State$new("S", pops)
  I <- individual::State$new("I", NI)
  R <- individual::State$new("R", NR)

  immunity <- individual::Variable$new("immunity", rep(0, pars$N))
  age <- individual::Variable$new("age", rep(0, pars$N))
  location <- individual::Variable$new("location", rep(0, pars$N))
  human <- individual::Individual$new("human", list(S, I, R),
                                      variables = list(immunity, age, location))

  processes <- list(
    hypatia::Individual_R_to_S_2(S, I, human, immunity, age, location, pars),
    hypatia::Individual_I_to_R_2(I, R, human, immunity, age, location, pars),
    hypatia::Individual_R_to_S_2(S, R, human, immunity, age, location, pars),
    hypatia::Render_state_sizes_2(S, I, R, human)
  )

  output <- individual::simulate(human, processes, timestep,
                                 parameters = list(immunity_level = .2,
                                 age_level = 0.3, location_level = 0.4))

  df <-   data.frame(S = output$susceptable_counts, I = output$infected_counts,
                     R = output$recovered_counts,
                     time = output$time, type = "Individual",
                     legend = "Individual", stringsAsFactors = FALSE)

  expect_true(is.data.frame(df))

})

test_that("test individual model with 10000 humans with immunity, age and
          location effects", {

  # Use hypatia::displaythemodel to plot
  pars <- hypatia::Get_parameters_for_sirstochastic()
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

  immunity <- individual::Variable$new("immunity", runif(population, 0, .1))
  rate = 1 / pars$average_age
  age <- individual::Variable$new("age", rexp(pars$N, rate))
  location <- individual::Variable$new("location", runif(population, 0, .2))
  human <- individual::Individual$new("human", list(S, I, R),
                                      variables = list(immunity, age, location))

  processes <- list(
    hypatia::Individual_S_to_I_2(S, I, human, immunity, age, location, pars),
    hypatia::Individual_I_to_R_2(I, R, human, immunity, age, location, pars),
    hypatia::Individual_R_to_S_2(S, R, human, immunity, age, location, pars),
    hypatia::Render_state_sizes_2(S, I, R, human)
  )

  output <- individual::simulate(human, processes, timestep,
                                 parameters = list(immunity_level = .2,
                                                   age_level = 0.3,
                                                   location_level = 0.4))

  df <-   data.frame(S = output$susceptable_counts, I = output$infected_counts,
                     R = output$recovered_counts, time = output$time,
                     type = "Individual",  legend = "Individual",
                     stringsAsFactors = FALSE)

  expect_true(is.data.frame(df))

})

test_that("test individual model with 10000 humans with immunity, age and
          location effects", {
  # Use hypatia::displaythemodel to plot

  pars <- hypatia::Get_parameters_for_sirstochastic()
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

  immunity <- individual::Variable$new("immunity", runif(population, 0, .1))
  rate = 1 / pars$average_age
  age <- individual::Variable$new("age", rexp(pars$N, rate))
  location <- individual::Variable$new("location", runif(population, 0, .2))
  human <- individual::Individual$new("human", list(S, I, R),
                                      variables = list(immunity, age, location))

  processes <- list(
    hypatia::Individual_S_to_I_2(S, I, human, immunity, age, location, pars),
    hypatia::Individual_I_to_R_2(I, R, human, immunity, age, location, pars),
    hypatia::Individual_R_to_S_2(S, R, human, immunity, age, location, pars),
    hypatia::Render_state_sizes_2(S, I, R, human)
  )

  output <- individual::simulate(human, processes, timestep,
                                 parameters = list(immunity_level = .2,
                                 age_level = 0.3, location_level = 0.4))

  df <-   data.frame(S = output$susceptable_counts, I = output$infected_counts,
                     R = output$recovered_counts, time = output$time,
                     type = "Individual",  legend = "Individual",
                     stringsAsFactors = FALSE)

  expect_true(is.data.frame(df))

})

test_that("test individual model with 10000 humans with immunity, age and
          location effects switched off", {

# Use hypatia::displaythemodel to plot

pars <- hypatia::Get_parameters_for_sirstochastic()

population <- pars$N
NI <- pars$I0
NR <- 2
pops <- population - NI - NR
timestep <- pars$num/pars$dt

S <- individual::State$new('S', pops)
I <- individual::State$new('I', NI)
R <- individual::State$new('R', NR)

immunity <- individual::Variable$new("immunity",  rep(0, pars$N))
age  <- individual::Variable$new("age", rep(0, pars$N))
location <- individual::Variable$new("location", rep(0, pars$N))
human <- individual::Individual$new("human", list(S, I, R),
                                    variables = list(immunity, age, location))

processes <- list(
  hypatia::Individual_S_to_I_2(S, I, human, immunity, age, location, pars),
  hypatia::Individual_I_to_R_2(I, R, human, immunity, age, location, pars),
  hypatia::Individual_R_to_S_2(S, R, human, immunity, age, location, pars),
  hypatia::Render_state_sizes_2(S, I, R, human)
)

output <- individual::simulate(human, processes, timestep)

df <-   data.frame(S = output$susceptable_counts, I = output$infected_counts,
                   R = output$recovered_counts, time = output$time,
                   type = "Individual", legend = "Individual",
                   stringsAsFactors = FALSE)

expect_true(is.data.frame(df))

})

# #TEST not working - I2 not being calculated
# test_that("test individual model with 10000 humans with immunity, age and
#location with different states", {
# #   # I is infected, I2 is severely infected, D is dead
# #   # Use hypatia::displaythemodel2(df) to plot
#
#   pars <- hypatia::Get_parameters_for_sirstochastic()
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
#   S <- individual::State$new("S", pops)
#   I <- individual::State$new("I", NI)
#   I2 <- individual::State$new("I2", NI2)
#   D <- individual::State$new("D", ND)
#   R <- individual::State$new("R", NR)
#
#   immunity <- individual::Variable$new("immunity", runif(population, 0, .1))
#   rate <- 1 / pars$average_age
#   age <- individual::Variable$new("age", rexp(population, rate))
#   location <- individual::Variable$new("location", runif(population, 0, .2))
#   human <- individual::Individual$new("human", list(S, I, I2, D, R), variables
# = list(immunity, age, location))
#
#   processes <- list(
#     hypatia::Individual_S_to_I_and_I2(S, I, I2, human, immunity, age, location,
# pars),
#     # hypatia::Individual_I_to_R_I2_to_D(I, R, I2, D, human, immunity, age,
# location, pars),
#     # hypatia::Individual_R_to_S_2(R, S, human, immunity, age, location, pars),
#     hypatia::Render_state_sizes2(S, I, R, I2, D, human)
#   )
#
#   output <- individual::simulate(human, processes, timestep , parameters
#= list(immunity_level = .2, age_level = 0.3, location_level = 0.4))
#
#   df <- data.frame(S = output$susceptable_counts, I = output$infected_counts,
#                      I2 = output$severelyinfected_counts,
#R = output$recovered_counts,
#                      D = output$dead_counts, time = output$time,
#type = "Individual",
#                      legend = "Individual", stringsAsFactors = FALSE)
#
#   expect_true(is.data.frame(df))
#
# })

# test_that("test individual model with 10000 humans with immunity, age and
#location siwtched off and with different states", {
#   # I is infected, I2 is severely infected, D is dead
#   # Use hypatia::displaythemodel2(df) to plot
#
#   pars <- hypatia::Get_parameters_for_sirstochastic()
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
#   immunity <- individual::Variable$new('immunity',  rep(0, pars$N))
#   age  <- individual::Variable$new('age', rep(0, pars$N))
#   location <- individual::Variable$new('location', rep(0, pars$N))
#   human <- individual::Individual$new("human", list(S, I, I2, D, R),
#variables = list(immunity, age, location))
#
#   processes <- list(
#     hypatia::Individual_S_to_I_and_I2(S, I, I2, human, immunity, age,
#location, pars),
#     hypatia::Individual_I_to_R_I2_to_D(I, R, I2, D, human, immunity, age,
#location, pars),
#     hypatia::Individual_R_to_S_2(R, S, human, immunity, age, location, pars),
#     hypatia::Render_state_sizes2(S, I, R, I2, D, human)
#   )
#
#   output <- individual::simulate(human, processes, timestep)
#   #, parameters = list(immunity_level = .2, age_level = 0.3,
#location_level = 0.4))
#
#   df <- data.frame(S = output$susceptable_counts, I = output$infected_counts,
#                      I2 = output$severelyinfected_counts,
#R = output$recovered_counts,
#                      D = output$dead_counts, time = output$time,
#type = "Individual",
#                      legend = "Individual", stringsAsFactors = FALSE)
#
#   expect_true(is.data.frame(df))
#
# })


test_that("test individual model with SQUIRE states and probabilities for 2nd
          age group, 5-9", {
  # Use hypatia::displaythemodel3(df) to plot
  warnings()
  pars <- hypatia::Get_parameters_for_sirstochastic()

  population <- squire::get_population("Afghanistan", simple_SEIR = FALSE)

  psq <- hypatia::Parameters_explicit_SEIR(
    population = population$n,
    dt = 1,
    R0 = 2,
    tt_contact_matrix = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    time_period = 1000,
    contact_matrix_set = squire::contact_matrices[[1]])

  NR <- 0
  newpopulation <- population$n[2]
  timestep <- 100
  ind <- 2

  S <- individual::State$new("S", psq$S_0[ind])
  E1 <- individual::State$new("E1", psq$E1_0[ind])
  E2 <- individual::State$new("E2", psq$E2_0[ind])
  IMild <- individual::State$new("IMild", psq$IMild_0[ind])
  ICase1 <- individual::State$new("ICase1", psq$ICase1_0[ind])
  ICase2 <- individual::State$new("ICase2", psq$ICase2_0[ind])
  cum_hosp_inc <- individual::State$new("cum_hosp_inc", NR)
  # cum_ICU_inc <- individual::State$new("cum_ICU_inc", NR)
  IOxGetLive1 <- individual::State$new("IOxGetLive1", psq$IOxGetLive1_0[ind])
  IOxGetLive2 <- individual::State$new("IOxGetLive2", psq$IOxGetLive2_0[ind])
  IOxGetDie1 <- individual::State$new("IOxGetDie1", psq$IOxGetDie1_0[ind])
  IOxGetDie2 <- individual::State$new("IOxGetDie2", psq$IOxGetDie2_0[ind])
  IOxNotGetLive1 <- individual::State$new("IOxNotGetLive1",
                                          psq$IOxNotGetLive1_0[ind])
  IOxNotGetLive2 <- individual::State$new("IOxNotGetLive2",
                                          psq$IOxNotGetLive2_0[ind])
  IOxNotGetDie1 <- individual::State$new("IOxNotGetDie1",
                                         psq$IOxNotGetDie1_0[ind])
  IOxNotGetDie2 <- individual::State$new("IOxNotGetDie2",
                                         psq$IOxNotGetDie2_0[ind])
  IMVGetLive1 <- individual::State$new("IMVGetLive1", psq$IMVGetLive1_0[ind])
  IMVGetLive2 <- individual::State$new("IMVGetLive2", psq$IMVGetLive2_0[ind])
  IMVGetDie1 <- individual::State$new("IMVGetDie1", psq$IMVGetDie1_0[ind])
  IMVGetDie2 <- individual::State$new("IMVGetDie2", psq$IMVGetDie2_0[ind])
  IMVNotGetLive1 <- individual::State$new("IMVNotGetLive1",
                                          psq$IMVNotGetLive1_0[ind])
  IMVNotGetLive2 <- individual::State$new("IMVNotGetLive2",
                                          psq$IMVNotGetLive2_0[ind])
  IMVNotGetDie1 <- individual::State$new("IMVNotGetDie1",
                                         psq$IMVNotGetDie1_0[ind])
  IMVNotGetDie2 <- individual::State$new("IMVNotGetDie2",
                                         psq$IMVNotGetDie2_0[ind])
  IRec1 <- individual::State$new("IRec1", psq$IRec1_0[ind])
  IRec2 <- individual::State$new("IRec2", psq$IRec2_0[ind])
  R <- individual::State$new("R", psq$R_0[ind])
  D <- individual::State$new("D", psq$D_0[ind])

  human <- individual::Individual$new("human", list(S, E1, E2, IMild, ICase1,
                                          ICase2, cum_hosp_inc, IOxGetLive1,
                                          IOxGetLive2, IOxNotGetLive1,
                                          IOxNotGetLive2, IOxGetDie1,
                                          IOxGetDie2, IOxNotGetDie1,
                                          IOxNotGetDie2, IMVGetLive1,
                                          IMVGetLive2, IMVNotGetLive1,
                                          IMVNotGetLive2, IMVGetDie1,
                                          IMVGetDie2, IMVNotGetDie1,
                                          IMVNotGetDie2, IRec1, IRec2,
                                          R, D))


  problamda <- 0.1
  lambda <- 0.1
  beta <- 0.1

  processes <- list(
    hypatia::SEIRexplicitparameters(human, IMild, ICase1, ICase2, cum_hosp_inc,
                                    ind, population$n[2], lambda, problambda,
                                    beta, psq$mix_mat_set[1,ind,], psq$dt),
    individual::fixed_probability_state_change_process(
      "human", S$name, E1$name, 0.1),
    individual::fixed_probability_state_change_process(
      "human", E1$name, E2$name, psq$pgamma_E),
    hypatia::E2_IMild(human, IMild, E2, ICase1,
                      psq$pgamma_E, psq$prob_hosp[ind]),
    individual::fixed_probability_state_change_process(
      "human", IMild$name, R$name, psq$pgamma_IMild),
    individual::fixed_probability_state_change_process(
      "human", E2$name, ICase1$name, psq$prob_hosp[ind]),
    individual::fixed_probability_state_change_process(
      "human", ICase1$name,ICase2$name, psq$pgamma_ICase),
    individual::fixed_probability_state_change_process(
      "human", ICase2$name,cum_hosp_inc$name, psq$pgamma_ICase),
    individual::fixed_probability_state_change_process(
      "human", IOxGetLive1$name,IOxGetLive2$name, psq$pgamma_get_ox_survive),
    individual::fixed_probability_state_change_process(
      "human", IOxGetLive2$name,R$name, psq$pgamma_get_ox_survive),
    individual::fixed_probability_state_change_process(
      "human", IOxNotGetLive1$name,IOxNotGetLive2$name,
      psq$pgamma_not_get_ox_survive),
    individual::fixed_probability_state_change_process(
      "human", IOxNotGetLive2$name,R$name, psq$pgamma_not_get_ox_survive),
    individual::fixed_probability_state_change_process(
      "human", IOxGetDie1$name,IOxGetDie2$name, psq$pgamma_get_ox_die),
    individual::fixed_probability_state_change_process(
      "human", IOxGetDie2$name, D$name, psq$pgamma_get_ox_die),
    individual::fixed_probability_state_change_process(
      "human", IOxNotGetDie1$name, IOxNotGetDie2$name,
      psq$pgamma_not_get_ox_die),
    individual::fixed_probability_state_change_process(
      "human", IOxNotGetDie2$name, D$name, psq$pgamma_not_get_ox_die),
    individual::fixed_probability_state_change_process(
      "human", IMVGetLive1$name, IMVGetLive2$name, psq$pgamma_get_mv_survive),
    individual::fixed_probability_state_change_process(
      "human", IMVGetLive2$name, IRec1$name, psq$pgamma_get_mv_survive),
    individual::fixed_probability_state_change_process(
      "human", IMVNotGetLive1$name, IMVNotGetLive2$name,
      psq$pgamma_not_get_mv_survive),
    individual::fixed_probability_state_change_process(
      "human", IMVNotGetLive2$name, R$name, psq$pgamma_not_get_mv_survive),
    individual::fixed_probability_state_change_process(
      "human", IMVGetDie1$name, IMVGetDie2$name, psq$pgamma_get_mv_die),
    individual::fixed_probability_state_change_process(
      "human", IMVGetDie2$name, D$name,  psq$pgamma_get_mv_die),
    individual::fixed_probability_state_change_process(
      "human", IMVNotGetDie1$name, IMVNotGetDie2$name,
      psq$pgamma_not_get_mv_die),
    individual::fixed_probability_state_change_process(
      "human", IMVNotGetDie2$name,D$name, psq$pgamma_not_get_mv_die),
    individual::fixed_probability_state_change_process(
      "human", IRec1$name, IRec2$name, psq$pgamma_rec),
    individual::fixed_probability_state_change_process(
      "human", IRec2$name, R$name, psq$pgamma_rec),

    hypatia::Render_state_sizes3(S, E1, E2, IMild, ICase1, ICase2, cum_hosp_inc,
                                 IOxGetLive1, IOxGetLive2, IOxNotGetLive1,
                                 IOxNotGetLive2, IOxGetDie1, IOxGetDie2,
                                 IOxNotGetDie1, IOxNotGetDie2,IMVGetLive1,
                                 IMVGetLive2,IMVNotGetLive1,IMVNotGetLive2,
                                 IMVGetDie1,IMVGetDie2, IMVNotGetDie1,
                                 IMVNotGetDie2, IRec1, IRec2, R, D, human)
  )

  out <- individual::simulate(human, processes, timestep)

  df <- data.frame(S = out$S,
                   E1 = out$E1,
                   E2 = out$E2,
                   IMild = out$IMild,
                   ICase1 = out$ICase1,
                   ICase2 = out$ICase2,
                   cum_hosp_inc = out$cum_hosp_inc,
                   IOxGetLive1 = out$IOxGetLive1,
                   IOxGetLive2 = out$IOxGetLive2,
                   IOxNotGetLive1 = out$IOxNotGetLive1,
                   IOxNotGetLive2 = out$IOxNotGetLive2,
                   IOxGetDie1 = out$IOxGetDie1,
                   IOxGetDie2 = out$IOxGetDie2,
                   IOxNotGetDie1 = out$IOxNotGetDie1,
                   IOxNotGetDie2 = out$IOxNotGetDie2,
                   IMVGetLive1 = out$IMVGetLive1,
                   IMVGetLive2 = out$IMVGetLive2,
                   IMVNotGetLive1 = out$IMVNotGetLive1,
                   IMVNotGetLive2 = out$IMVNotGetLive2,
                   IMVGetDie1 = out$IMVNotGetLive2,
                   IMVGetDie2 = out$IMVGetDie2,
                   IMVNotGetDie1 = out$IMVNotGetDie1,
                   IMVNotGetDie2 = out$IMVNotGetDie2,
                   IRec1 = out$IRec1, IRec2 = out$IRec2,
                   R = out$R, D = out$D, time = out$time, type = "Individual",
                   legend = "Individual", stringsAsFactors = FALSE)

  expect_true(is.data.frame(df))

})
