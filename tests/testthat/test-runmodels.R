test_that("SQUIRE SEEIR model runs", {

pop = squire::get_population("Afghanistan", simple_SEIR = FALSE)

set.seed(123)
r1 <- squire::run_explicit_SEEIR_model(population = pop$n,
                               dt = 1,
                               R0 = 2,
                               time_period = 1000,
                               replicates = 10,
                               contact_matrix_set=squire::contact_matrices[[1]])
expect_type(r1$output, "double")

o1 <- squire::format_output(r1)
expect_true(sum(dplyr::filter(o1, t == min(t), replicate == 1)$y) == sum(pop$n))

uc <- unique(o1$compartment)

for(i in seq_along(uc)){
  expect_equal(sum(o1$compartment == uc[i]), 1000 * 10)
}

set.seed(123)
# Multiple R0s
r2 <- squire::run_explicit_SEEIR_model(population = pop$n,
                               dt = 1,
                               R0 = c(2,2),
                               tt_R0 = c(0, 10),
                               time_period = 1000,
                               replicates = 10,
                               contact_matrix_set=squire::contact_matrices[[1]])
expect_identical(r1$output, r2$output)
})

#test_that("Hypatia SEIR model runs with no immunity, age or location issues", {

  # pop = squire::get_population("Afghanistan", simple_SEIR = FALSE)
  # pars <- hypatia::get_parameters_for_sirstochastic()
  #
  # set.seed(123)
  #
  # immunity <- individual::Variable$new('immunity', rep(0, pars$N))
  # rate=1/pars$average_age
  # age <- individual::Variable$new('age', rexp(pars$N, rate))
  # location <- individual::Variable$new('location', rep(0, pars$N))
  # human <- individual::Individual$new('human', list(S, I, R), variables = list(immunity, age, location))
  #
  # processes <- list(
  #   SEIRexplicitparameters(S, E1, E2, IMild, ICase1, ICase2, cum_hosp_inc, cum_ICU_inc, IOxGetLive1, IOxGetLive2,
  #                          IOxGetDie1, IOxGetDie2, IOxNotGetLive1, IOxNotGetLive2, IOxNotGetDie1, IOxNotGetDie2,
  #                          IMVGetLive1, IMVGetLive2, IMVGetDie1, IMVGetDie2, IMVNotGetLive1, IMVNotGetLive2,
  #                          IMVNotGetDie1, IMVNotGetDie2, IRec1, IRec2, R, D, D_get, D_not_get, i, j),
  #   render_state_sizes(S, I, R, human)
  # )
  #
  # output <- individual::simulate(human, processes, timestep, parameters = list(age_level=0.3))
  #
  # df <-   data.frame(S = output$susceptable_counts, I = output$infected_counts, R = output$recovered_counts, time = output$time, type = "Individual",  legend = "Individual", stringsAsFactors = FALSE)
  #
  # expect_true(is.data.frame(df))

#})
