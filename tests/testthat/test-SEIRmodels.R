test_that("check that Validated_state_update is working as expected", {

  human <- mockery::mock()
  S <- mockery::mock()

  api <- list(
    queue_state_update = mockery::mock()
  )

  expect_error(
    hypatia::validated_state_update(api, human, S, 10001, 10000), "*")
  expect_error(
    hypatia::validated_state_update(api, human, S, 933.5, 10000), "*")
  expect_error(hypatia::validated_state_update(api, human, S, -10, 10000), "*")
  expect_error(hypatia::validated_state_update(api, human, S, NA, 10000), "*")
})

test_that("test individual model with SQUIRE states and probabilities for 2nd
          age group, 5-9", {
  # Use hypatia::displaythemodel3(df) to plot
  warnings()
  pars <- hypatia::get_parameters_for_sirstochastic()

  population <- squire::get_population("Afghanistan", simple_SEIR = FALSE)

  dt <- 1

  psq <- squire::parameters_explicit_SEEIR(
    population = population$n,
    dt = 1,
    R0 = 2,
    tt_contact_matrix = 0,
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

  human <- individual::Individual$new(
    "human", list(S, E1, E2, IMild, ICase1,
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

  pgamma_E <- 1 - exp(-1.0 * (psq$gamma_E * dt))
  pgamma_IMild <- 1 - exp(-psq$gamma_IMild * dt)
  pgamma_ICase <- 1 - exp(-1.0 * (psq$gamma_ICase * dt))
  pgamma_get_ox_survive <- 1 - exp(-1.0 * (psq$gamma_get_ox_survive * dt))
  pgamma_not_get_ox_survive <-
    1 - exp(-1.0 * (psq$gamma_not_get_ox_survive * dt))
  pgamma_get_ox_die <- 1 - exp(-1.0 * (psq$gamma_get_ox_die * dt))
  pgamma_not_get_ox_die <- 1 - exp(-1.0 * (psq$gamma_not_get_ox_die * dt))
  pgamma_get_mv_survive <- 1 - exp(-1.0 * (psq$gamma_get_mv_survive * dt))
  pgamma_not_get_mv_survive <-
    1 - exp(-1.0 * (psq$gamma_not_get_mv_survive * dt))
  pgamma_get_mv_die <- 1 - exp(-1.0 * (psq$gamma_get_mv_die * dt))
  pgamma_not_get_mv_die  <- 1 - exp(-1.0 * (psq$gamma_not_get_mv_die * dt))
  pgamma_rec  <- 1 - exp(-1.0 * (psq$gamma_rec * dt))

  processes <- list(
    hypatia:::infection_process(human, IMild, ICase1, ICase2,
       S, E1, ind, population$n[2], psq$beta_set,
       psq$mix_mat_set[1, ind ,], psq$dt),
    individual::fixed_probability_state_change_process(
      "human", E1$name, E2$name, pgamma_E[2]),
    hypatia:::E2_IMild(human, IMild, E2, ICase1,
      pgamma_E, psq$prob_hosp[ind]),
    individual::fixed_probability_state_change_process(
      "human", IMild$name, R$name, pgamma_IMild),
    individual::fixed_probability_state_change_process(
      "human", E2$name, ICase1$name, psq$prob_hosp[ind]),
    individual::fixed_probability_state_change_process(
      "human", ICase1$name, ICase2$name, pgamma_ICase),
    individual::fixed_probability_state_change_process(
      "human", ICase2$name, cum_hosp_inc$name, pgamma_ICase),
    individual::fixed_probability_state_change_process(
      "human", IOxGetLive1$name, IOxGetLive2$name, pgamma_get_ox_survive),
    individual::fixed_probability_state_change_process(
      "human", IOxGetLive2$name, R$name, pgamma_get_ox_survive),
    individual::fixed_probability_state_change_process(
      "human", IOxNotGetLive1$name, IOxNotGetLive2$name,
      pgamma_not_get_ox_survive),
    individual::fixed_probability_state_change_process(
      "human", IOxNotGetLive2$name, R$name, pgamma_not_get_ox_survive),
    individual::fixed_probability_state_change_process(
      "human", IOxGetDie1$name, IOxGetDie2$name, pgamma_get_ox_die),
    individual::fixed_probability_state_change_process(
      "human", IOxGetDie2$name, D$name, pgamma_get_ox_die),
    individual::fixed_probability_state_change_process(
      "human", IOxNotGetDie1$name, IOxNotGetDie2$name,
      pgamma_not_get_ox_die),
    individual::fixed_probability_state_change_process(
      "human", IOxNotGetDie2$name, D$name, pgamma_not_get_ox_die),
    individual::fixed_probability_state_change_process(
      "human", IMVGetLive1$name, IMVGetLive2$name, pgamma_get_mv_survive),
    individual::fixed_probability_state_change_process(
      "human", IMVGetLive2$name, IRec1$name, pgamma_get_mv_survive),
    individual::fixed_probability_state_change_process(
      "human", IMVNotGetLive1$name, IMVNotGetLive2$name,
      pgamma_not_get_mv_survive),
    individual::fixed_probability_state_change_process(
      "human", IMVNotGetLive2$name, R$name, pgamma_not_get_mv_survive),
    individual::fixed_probability_state_change_process(
      "human", IMVGetDie1$name, IMVGetDie2$name, pgamma_get_mv_die),
    individual::fixed_probability_state_change_process(
      "human", IMVGetDie2$name, D$name, pgamma_get_mv_die),
    individual::fixed_probability_state_change_process(
      "human", IMVNotGetDie1$name, IMVNotGetDie2$name,
      pgamma_not_get_mv_die),
    individual::fixed_probability_state_change_process(
      "human", IMVNotGetDie2$name, D$name, pgamma_not_get_mv_die),
    individual::fixed_probability_state_change_process(
      "human", IRec1$name, IRec2$name, pgamma_rec),
    individual::fixed_probability_state_change_process(
      "human", IRec2$name, R$name, pgamma_rec),

    hypatia:::render_all_state_sizes(
      S, E1, E2, IMild, ICase1, ICase2, cum_hosp_inc,
      IOxGetLive1, IOxGetLive2, IOxNotGetLive1,
      IOxNotGetLive2, IOxGetDie1, IOxGetDie2,
      IOxNotGetDie1, IOxNotGetDie2, IMVGetLive1,
      IMVGetLive2, IMVNotGetLive1, IMVNotGetLive2,
      IMVGetDie1, IMVGetDie2, IMVNotGetDie1,
      IMVNotGetDie2, IRec1, IRec2, R, D, human)
  )

  out <- individual::simulate(human, processes, timestep)

  df <- data.frame(
     S = out$S,
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


test_that("repeat of previous test but using Create_states,
          Create_individuals, and Probabilities_of_states", {

# Use hypatia::displaythemodel3(df) to plot

    warnings()
    pars <- hypatia::get_parameters_for_sirstochastic()

    population <- squire::get_population("Afghanistan", simple_SEIR = FALSE)

    dt <- 1

    psq <- squire::parameters_explicit_SEEIR(
      population = population$n,
      dt = 1,
      R0 = 2,
      tt_contact_matrix = 0,
      time_period = 1000,
      contact_matrix_set = squire::contact_matrices[[1]])

    NR <- 0
    newpopulation <- population$n[2]
    timestep <- 100
    ind <- 2

    states <- create_states(psq)

    indivs <- create_individuals(states, variables)

    pstates <- probabilities_of_states(dt, psq)

    processes <- list(
      hypatia:::infection_process(indivs$human, states$IMild, states$ICase1,states$ICase2,
         states$S, states$E1, ind, population$n[2], psq$beta_set,
         psq$mix_mat_set[1, ind ,], psq$dt),
      individual::fixed_probability_state_change_process(
        "human", states$E1$name, states$E2$name, pstates$pgamma_E[2]),
      hypatia:::E2_IMild(indivs$human, states$IMild, states$E2, states$ICase1,
        pstates$pgamma_E, psq$prob_hosp[ind]),
      individual::fixed_probability_state_change_process(
        "human", states$IMild$name, states$R$name, pstates$pgamma_IMild),
      individual::fixed_probability_state_change_process(
        "human", states$E2$name, states$ICase1$name, psq$prob_hosp[ind]),
      individual::fixed_probability_state_change_process(
        "human", states$ICase1$name, states$ICase2$name,
        pstates$pgamma_ICase),
      individual::fixed_probability_state_change_process(
        "human", states$ICase2$name, states$cum_hosp_inc$name,
        pstates$pgamma_ICase),
      individual::fixed_probability_state_change_process(
        "human", states$IOxGetLive1$name, states$IOxGetLive2$name,
        pstates$pgamma_get_ox_survive),
      individual::fixed_probability_state_change_process(
        "human", states$IOxGetLive2$name, states$R$name,
        pstates$pgamma_get_ox_survive),
      individual::fixed_probability_state_change_process(
        "human", states$IOxNotGetLive1$name, states$IOxNotGetLive2$name,
        pstates$pgamma_not_get_ox_survive),
      individual::fixed_probability_state_change_process(
        "human", states$IOxNotGetLive2$name, states$R$name,
        pstates$pgamma_not_get_ox_survive),
      individual::fixed_probability_state_change_process(
        "human", states$IOxGetDie1$name, states$IOxGetDie2$name,
        pstates$pgamma_get_ox_die),
      individual::fixed_probability_state_change_process(
        "human", states$IOxGetDie2$name, states$D$name,
        pstates$pgamma_get_ox_die),
      individual::fixed_probability_state_change_process(
        "human", states$IOxNotGetDie1$name, states$IOxNotGetDie2$name,
        pstates$pgamma_not_get_ox_die),
      individual::fixed_probability_state_change_process(
        "human", states$IOxNotGetDie2$name, states$D$name,
        pstates$pgamma_not_get_ox_die),
      individual::fixed_probability_state_change_process(
        "human", states$IMVGetLive1$name, states$IMVGetLive2$name,
        pstates$pgamma_get_mv_survive),
      individual::fixed_probability_state_change_process(
        "human", states$IMVGetLive2$name, states$IRec1$name,
        pstates$pgamma_get_mv_survive),
      individual::fixed_probability_state_change_process(
        "human", states$IMVNotGetLive1$name, states$IMVNotGetLive2$name,
        pstates$pgamma_not_get_mv_survive),
      individual::fixed_probability_state_change_process(
        "human", states$IMVNotGetLive1$name, states$IMVNotGetLive2$name,
        pstates$pgamma_not_get_mv_survive),
      individual::fixed_probability_state_change_process(
        "human", states$IMVGetDie1$name, states$IMVGetDie2$name,
        pstates$pgamma_get_mv_die),
      individual::fixed_probability_state_change_process(
        "human", states$IMVGetDie2$name, states$D$name,
        pstates$pgamma_get_mv_die),
      individual::fixed_probability_state_change_process(
        "human", states$IMVNotGetDie1$name, states$IMVNotGetDie2$name,
        pstates$pgamma_not_get_mv_die),
      individual::fixed_probability_state_change_process(
        "human", states$IMVNotGetDie2$name, states$D$name,
        pstates$pgamma_not_get_mv_die),
      individual::fixed_probability_state_change_process(
        "human", states$IRec1$name, states$IRec2$name, pstates$pgamma_rec),
      individual::fixed_probability_state_change_process(
        "human", states$IRec2$name, states$R$name, pstates$pgamma_rec),

      hypatia:::render_all_state_sizes(
        states$S, states$E1, states$E2, states$IMild, states$ICase1,
        states$ICase2, states$cum_hosp_inc,
        states$IOxGetLive1, states$IOxGetLive2, states$IOxNotGetLive1,
        states$IOxNotGetLive2, states$IOxGetDie1, states$IOxGetDie2,
        states$IOxNotGetDie1, states$IOxNotGetDie2, states$IMVGetLive1,
        states$IMVGetLive2, states$IMVNotGetLive1, states$IMVNotGetLive2,
        states$IMVGetDie1, states$IMVGetDie2, states$IMVNotGetDie1,
        states$IMVNotGetDie2, states$IRec1, states$IRec2, states$R,
        states$D, indivs$human)
    )

    out <- individual::simulate(indivs$human, processes, timestep)

    df <- data.frame(
         S = out$S,
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
         R = out$R, D = out$D, time = out$time,
         type = "Individual",
         legend = "Individual", stringsAsFactors = FALSE)

    expect_true(is.data.frame(df))

})
