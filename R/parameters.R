#' @title Default parameters for SIR model
#'
#' @param overrides uses a named parameter list instead of defaults
#' Parameters are defined below
#'
#' * `pars` list of parameters
#' Compartmental
#' * `beta` contact rate
#' * `nu rate` of recovery
#' * `mu rate` of death
#' * `prop_immune` proportion of people who are immune
#' * `N number` of people being investigated
#' * `num` used for countlim/num time points
#' * `I0` initial number of infected people
#' * `dt` time step
#' * `I0_at_steady_state` boolean value
#' * `n_deaths_S` number of deaths at susceptible stage
#' * `n_infections_S` number of infections at susceptible stage
#' * `n_deaths_I` number of deaths at infected stage
#' * `n_recoveries_I` number of recoveries at infected stage
#' * `n_deaths_R` number of deaths at recovered stage
#' * `n_births` number of births
#' Individual only
#' * `novariations`TRUE if no age, location, immunity effects
#' * `average_age` average age for population
#' * `includeage` TRUE if age used
#' * `includebirth` TRUE if immunity used
#' * `includeimmune` TRUE if immunity used
#' * `includelocation` TRUE if location used
#' * `infection_rate` rate of infection
#' * `severe_infection_rate` rate of infection
#' * `recovery_rate` rate of recovery
#' * `age_rate` rate of age effect
#' * `location_rate` rate of location effect
#'
#' @return list
#' @export
Get_parameters_for_sirstochastic <- function(overrides = list()) {

  pars <- sir_model_parameters_defaults()

  # Override pars with any client specified ones
  if (!is.list(overrides) && !is.null(overrides)) {
    stop("overrides must be a list")
  }

  for (name in names(overrides)) {
    if (!(name %in% names(pars))) {
      stop(paste("unknown parameter", name, sep = " " ))
    }
    pars[[name]] <- overrides[[name]]
  }

  if (pars$beta < 0) {
    stop("'beta' must be positive")
  }

  if (pars$nu < 0) {
    stop("'nu' must be positive")
  }

  if (pars$mu < 0) {
    stop("'mu' must be positive")
  }

  if (pars$prop_immune > 0 || pars$prop_immune < 0) {
    stop("'prop_immune' must be between 0 and 1 (inclusive)")
  }

  if (pars$N <= 0) {
    stop("'N' must be positive")
  }

  if (pars$num <= 0) {
    stop("'num' must be positive")
  }

  if (pars$I0 > pars$N || pars$I0 < 0) {
    stop("'I0' must be positive and never greater than N")
  }

  if (pars$dt <= 0) {
    stop("'dt' must be positive and greater than 0")
  }

  if (pars$n_deaths_S < 0) {
    stop("'n_deaths_S' must be positive and greater than or equal to 0")
  }

  if (pars$n_infections_S < 0) {
    stop("'n_infections_S' must be positive and greater than or equal to 0")
  }

  if (pars$n_deaths_I < 0) {
    stop("'n_deaths_I' must be positive and greater than or equal to 0")
  }

  if (pars$n_recoveries_I < 0) {
    stop("'n_recoveries_I' must be positive and greater than or equal to 0")
  }

  if (pars$n_deaths_R < 0) {
    stop("'n_deaths_R' must be positive and greater than or equal to 0")
  }

  if (pars$n_births < 0) {
    stop("'n_births' must be positive and greater than or equal to 0")
  }

  if (pars$average_age <= 0) {
    stop("'average_age' must be positive and greater than 0")
  }

  if (pars$infection_rate < 0) {
    stop("'infection_rate' must be positive or equal to 0")
  }

  if (pars$severe_infection_rate < 0) {
    stop("'severe_infection_rate' must be positive or equal to 0")
  }

  if (pars$recovery_rate < 0) {
    stop("'recovery_rate' must be positive or equal to 0")
  }

  if (pars$age_rate < 0) {
    stop("'age_rate' must be positive or equal to 0")
  }

  if (pars$location_rate < 0) {
    stop("'location_rate' must be positive or equal to 0")
  }

  pars

}

sir_model_parameters_defaults <- function() {

  pars <- list(
    # Compartmental
    beta = 0.5,
    nu = 0.3,
    mu = 0.001,
    prop_immune = 0,
    I0_at_steady_state = FALSE,
    N = 10000,
    num = 100,
    I0 = 5,
    dt = 0.01,
    n_deaths_S = 0,
    n_infections_S = 0,
    n_deaths_I = 0,
    n_recoveries_I = 0,
    n_deaths_R = 0,
    n_births = 0,
    # individual only
    novariations = TRUE,
    average_age = 20,
    includeage = FALSE,
    includebirth = FALSE,
    includeimmune = FALSE,
    includelocation = FALSE,
    infection_rate = 0.4,
    severe_infection_rate = 0.1,
    recovery_rate = 0.8,
    age_rate = 0.2,
    location_rate = 0.2)

  pars
}

Default_probs <- function() {
  prob_hosp <- c(
    0.000744192, 0.000634166, 0.001171109, 0.002394593, 0.005346437,
    0.010289885, 0.016234604, 0.023349169, 0.028944623, 0.038607042,
    0.057734879, 0.072422135, 0.101602458, 0.116979814, 0.146099064,
    0.176634654 ,0.180000000)
  list(
    prob_hosp = prob_hosp,
    prob_severe = c(
      0.05022296,	0.05022296,	0.05022296,	0.05022296,	0.05022296,
      0.05022296,	0.05022296,	0.053214942, 0.05974426,	0.074602879,
      0.103612417, 0.149427991, 0.223777304, 0.306985918,
      0.385779555, 0.461217861, 0.709444444),
    prob_non_severe_death_treat = c(
      0.0125702,	0.0125702,	0.0125702,	0.0125702,
      0.0125702,	0.0125702,	0.0125702,	0.013361147,
      0.015104687,	0.019164124,	0.027477519,	0.041762108,
      0.068531658,	0.105302319,	0.149305732,	0.20349534,	0.5804312),
    prob_non_severe_death_no_treat = rep(0.6, length(prob_hosp)),
    prob_severe_death_treat = rep(0.5, length(prob_hosp)),
    prob_severe_death_no_treat = rep(0.95, length(prob_hosp)),
    p_dist = rep(1, length(prob_hosp))
  )
}
