# -----------------------------------------------------------------------------
#' @title Parmaters for explicit SEIR model
#'
#' @details All durations are in days.
#'
#' @param population Population vector (for each age group). Default = NULL,
#'   which will cause population to be sourced from \code{country}
#' @param country Character for country beign simulated. WIll be used to
#'   generate \code{population} and \code{contact_matrix_set} if
#'   unprovided. Either \code{country} or \code{population} and
#'   \code{contact_matrix_set} must be provided.
#' @param contact_matrix_set Contact matrices used in simulation. Default =
#'   NULL, which will generate this based on the \code{country}.
#' @param tt_contact_matrix Time change points for matrix change. Default = 0
#' @param R0 Basic Reproduction Number. Default = 3
#' @param tt_R0 Change time points for R0. Default = 0
#' @param beta_set Alternative parameterisation via beta rather than R0.
#'   Default = NULL, which causes beta to be estimated from R0
#' @param time_period Length of simulation. Default = 365
#' @param dt Time Step. Default = 0.1
#' @param init Data.frame of initial conditions. Default = NULL
#' @param seeding_cases Initial number of cases seeding the epidemic
#' @param prob_hosp probability of hospitalisation by age.
#'   Default = c(0.001127564, 0.000960857, 0.001774408, 0.003628171,
#'   0.008100662, 0.015590734, 0.024597885, 0.035377529,
#'   0.04385549, 0.058495518, 0.08747709, 0.109730508,
#'   0.153943118, 0.177242143, 0.221362219, 0.267628264)
#' @param prob_severe Probability of developing severe symptoms by age.
#'   Default = c(3.73755e-05, 3.18497e-05, 5.88166e-05, 0.000120264,
#'   0.000268514, 0.000516788, 0.00081535, 0.001242525,
#'   0.001729275, 0.002880196, 0.00598205, 0.010821894,
#'   0.022736324, 0.035911156, 0.056362032, 0.081467057)
#' @param prob_non_severe_death_treatment Probability of death from non severe
#'   treated infection.
#'   Default = c(0.0125702, 0.0125702, 0.0125702, 0.0125702,
#'   0.0125702, 0.0125702, 0.0125702, 0.013361147,
#'   0.015104687, 0.019164124, 0.027477519, 0.041762108,
#'   0.068531658, 0.105302319, 0.149305732, 0.20349534)
#' @param prob_severe_death_treatment Probability of death from severe infection
#'   that is treated. Default = rep(0.5, 16)
#' @param prob_non_severe_death_no_treatment Probability of death in non severe
#'   hospital inections that aren't treated
#' @param prob_severe_death_no_treatment Probability of death from severe infection
#'   that is not treated. Default = rep(0.95, 16)
#' @param p_dist Preferentiality of age group receiving treatment relative to
#'   other age groups when demand exceeds healthcare capacity.
#' @param dur_E Mean duration of incubation period (days). Default = 4.6
#' @param dur_IMild Mean duration of mild infection (days). Default = 2.1
#' @param dur_ICase Mean duration from symptom onset to hospitil admission (days).
#'   Default = 4.5
#' @param dur_get_ox_survive Mean duration of oxygen given survive. Default = 5
#' @param dur_get_ox_die Mean duration of oxygen given death. Default = 5
#' @param dur_not_get_ox_survive Mean duration without oxygen given survive.
#'   Default = 5
#' @param dur_not_get_ox_die Mean duration without  oxygen given death.
#'  Default = 5
#' @param dur_get_mv_survive Mean duration of ventilation given survive.
#'   Default = 7.3
#' @param dur_get_mv_die Mean duration of ventilation given death. Default = 6
#' @param dur_not_get_mv_survive Mean duration without ventilation given
#'   survive. Default = 7.3
#' @param dur_not_get_mv_die Mean duration without ventilation given
#'   death. Default = 1
#' @param dur_rec Duration of recovery after coming off ventilation. Default = 2
#' @param hosp_bed_capacity General bed capacity. Can be single number or vector if capacity time-varies.
#' @param ICU_bed_capacity ICU bed capacity. Can be single number or vector if capacity time-varies.
#' @param tt_hosp_beds Times at which hospital bed capacity changes (Default = 0 = doesn't change)
#' @param tt_ICU_beds Times at which ICU bed capacity changes (Default = 0 = doesn't change)
#'
#' @return Paramater List
#' @export
#'
parameters_explicit_SEIR <- function(

  # demography
  country = NULL,
  population = NULL,
  tt_contact_matrix = 0,
  contact_matrix_set = NULL,

  # transmission
  R0 = 3,
  tt_R0 = 0,
  beta_set = NULL,

  # initial state, duration, reps
  time_period = 365,
  dt = 0.1,
  init = NULL,
  seeding_cases = NULL,

  # parameters
  # probabilities
  # probabilities
  prob_hosp = squire:::probs$prob_hosp,
  prob_severe = squire:::probs$prob_severe,
  prob_non_severe_death_treatment = squire:::probs$prob_non_severe_death_treatment,
  prob_non_severe_death_no_treatment = squire:::probs$prob_non_severe_death_no_treatment,
  prob_severe_death_treatment = squire:::probs$prob_severe_death_treatment,
  prob_severe_death_no_treatment = squire:::probs$prob_severe_death_no_treatment,
  p_dist = squire:::probs$p_dist,

  # durations
  dur_E  = 4.6,
  dur_IMild = 2.1,
  dur_ICase = 4.5,

  dur_get_ox_survive = 9.5,
  dur_get_ox_die = 7.6,
  dur_not_get_ox_survive = 9.5*0.5,
  dur_not_get_ox_die = 7.6*0.5,

  dur_get_mv_survive = 11.3,
  dur_get_mv_die = 10.1,
  dur_not_get_mv_survive = 11.3*0.5,
  dur_not_get_mv_die = 1,

  dur_rec = 3.4,

  # health system capacity
  hosp_bed_capacity = NULL,
  ICU_bed_capacity = NULL,
  tt_hosp_beds = 0,
  tt_ICU_beds = 0

) {

  # Handle country population args
  cpm <- squire:::parse_country_population_mixing_matrix(country = country,
                                                population = population,
                                                contact_matrix_set = contact_matrix_set)
  country <- cpm$country
  population <- cpm$population
  contact_matrix_set <- cpm$contact_matrix_set

  # Standardise contact matrix set
  if (is.matrix(contact_matrix_set)) {
    contact_matrix_set <- list(contact_matrix_set)
  }

  # populate contact matrix set if not provided
  if (length(contact_matrix_set) == 1) {
    baseline <- contact_matrix_set[[1]]
    contact_matrix_set <- vector("list", length(tt_contact_matrix))
    for(i in seq_along(tt_contact_matrix)) {
      contact_matrix_set[[i]] <- baseline
    }
  }


  # populate hospital and ICU bed capacity if not provided
  if (is.null(hosp_bed_capacity)) {
    if (!is.null(country)) {
      beds <- squire::get_healthcare_capacity(country)
      hosp_beds <- beds$hosp_beds
      hosp_bed_capacity <- rep(round(hosp_beds * sum(population)/1000), length(tt_hosp_beds))
    } else {
      hosp_bed_capacity <- round(5 * sum(population)/1000)
    }
  }
  if (is.null(ICU_bed_capacity)) {
    if (!is.null(country)) {
      beds <- squire::get_healthcare_capacity(country)
      ICU_beds <- beds$ICU_beds
      ICU_bed_capacity <- rep(round(ICU_beds * sum(population)/1000), length(tt_ICU_beds))
    } else {
      ICU_bed_capacity <- round(3 * hosp_bed_capacity/100)
    }
  }

  # Initial state and matrix formatting
  # ----------------------------------------------------------------------------

  # Initialise initial conditions
  if (!is.null(seeding_cases)) {
    assert_int(seeding_cases)
    mod_init <- squire:::init_check_explicit(init, population, seeding_cases)
  } else {
    mod_init <- squire:::init_check_explicit(init, population)
  }

  # Convert contact matrices to input matrices
  matrices_set <- squire:::matrix_set_explicit(contact_matrix_set, population)

  # Input checks
  # ----------------------------------------------------------------------------
  mc <- squire:::matrix_check(population[-1], contact_matrix_set)
  stopifnot(length(R0) == length(tt_R0))
  stopifnot(length(contact_matrix_set) == length(tt_contact_matrix))
  stopifnot(length(hosp_bed_capacity) == length(tt_hosp_beds))
  stopifnot(length(ICU_bed_capacity) == length(tt_ICU_beds))
  tc <- lapply(list(tt_R0/dt, tt_contact_matrix/dt), squire:::check_time_change, time_period/dt)
  tc2 <- lapply(list(tt_hosp_beds/dt, tt_ICU_beds/dt), squire:::check_time_change, time_period/dt)

  squire:::assert_pos(dt)
  squire:::assert_pos(dur_E)
  squire:::assert_pos(dur_IMild)
  squire:::assert_pos(dur_ICase)
  squire:::assert_pos(dur_get_ox_survive)
  squire:::assert_pos(dur_get_ox_die)
  squire:::assert_pos(dur_not_get_ox_survive)
  squire:::assert_pos(dur_not_get_ox_die)
  squire:::assert_pos(dur_get_mv_survive)
  squire:::assert_pos(dur_get_mv_die)
  squire:::assert_pos(dur_not_get_mv_survive)
  squire:::assert_pos(dur_not_get_mv_die)
  squire:::assert_pos(time_period)
  squire:::assert_pos(hosp_bed_capacity)
  squire:::assert_pos(ICU_bed_capacity)

  squire:::assert_length(prob_hosp, length(population))
  squire:::assert_length(prob_severe, length(population))
  squire:::assert_length(prob_non_severe_death_treatment, length(population))
  squire:::assert_length(prob_non_severe_death_no_treatment, length(population))
  squire:::assert_length(prob_severe_death_treatment, length(population))
  squire:::assert_length(prob_severe_death_no_treatment, length(population))
  squire:::assert_length(p_dist, length(population))

  squire:::assert_numeric(prob_hosp, length(population))
  squire:::assert_numeric(prob_severe, length(population))
  squire:::assert_numeric(prob_non_severe_death_treatment, length(population))
  squire:::assert_numeric(prob_non_severe_death_no_treatment, length(population))
  squire:::assert_numeric(prob_severe_death_treatment, length(population))
  squire:::assert_numeric(prob_severe_death_no_treatment, length(population))
  squire:::assert_numeric(p_dist, length(population))

  squire:::assert_leq(prob_hosp, 1)
  squire:::assert_leq(prob_severe, 1)
  squire:::assert_leq(prob_non_severe_death_treatment, 1)
  squire:::assert_leq(prob_non_severe_death_no_treatment, 1)
  squire:::assert_leq(prob_severe_death_treatment, 1)
  squire:::assert_leq(prob_severe_death_no_treatment, 1)
  squire:::assert_leq(p_dist, 1)

  squire:::assert_greq(prob_hosp, 0)
  squire:::assert_greq(prob_severe, 0)
  squire:::assert_greq(prob_non_severe_death_treatment, 0)
  squire:::assert_greq(prob_non_severe_death_no_treatment, 0)
  squire:::assert_greq(prob_severe_death_treatment, 0)
  squire:::assert_greq(prob_severe_death_no_treatment, 0)
  squire:::assert_greq(p_dist, 0)


  # Convert and Generate Parameters As Required
  # ----------------------------------------------------------------------------

  # durations
  gamma_E = 2 * 1/dur_E
  gamma_IMild = 1/dur_IMild
  gamma_ICase = 2 * 1/dur_ICase
  gamma_get_ox_survive = 2 * 1/dur_get_ox_survive
  gamma_get_ox_die = 2 * 1/dur_get_ox_die
  gamma_not_get_ox_survive = 2 * 1/dur_not_get_ox_survive
  gamma_not_get_ox_die = 2 * 1/dur_not_get_ox_die
  gamma_get_mv_survive = 2 * 1/dur_get_mv_survive
  gamma_get_mv_die = 2 * 1/dur_get_mv_die
  gamma_not_get_mv_survive = 2 * 1/dur_not_get_mv_survive
  gamma_not_get_mv_die = 2 * 1/dur_not_get_mv_die
  gamma_rec = 2 * 1/dur_rec

  # probabilities
  pgamma_E <- 1 - exp(-1.0*(gamma_E * dt))
  pgamma_IMild <- 1 - exp(-gamma_IMild * dt)
  pgamma_ICase <- 1 - exp(-1.0*(gamma_ICase * dt))
  pgamma_get_ox_survive <- 1 - exp(-1.0*(gamma_get_ox_survive * dt))
  pgamma_not_get_ox_survive <- 1 - exp(-1.0*(gamma_not_get_ox_survive * dt))
  pgamma_get_ox_die <- 1 - exp(-1.0*(gamma_get_ox_die * dt))
  pgamma_not_get_ox_die <- 1 - exp(-1.0*(gamma_not_get_ox_die * dt))
  pgamma_get_mv_survive <- 1 - exp(-1.0*(gamma_get_mv_survive * dt))
  pgamma_not_get_mv_survive <- 1 - exp(-1.0*(gamma_not_get_mv_survive * dt))
  pgamma_get_mv_die <- 1 - exp(-1.0*(gamma_get_mv_die * dt))
  pgamma_not_get_mv_die  <- 1 - exp(-1.0*( gamma_not_get_mv_die * dt))
  pgamma_rec  <- 1 - exp(-1.0*( gamma_rec * dt))

  if (is.null(beta_set)) {
    baseline_matrix <- squire:::process_contact_matrix_scaled_age(contact_matrix_set[[1]], population)
    beta_set <- squire:::beta_est_explicit(dur_IMild = dur_IMild,
                                  dur_ICase = dur_ICase,
                                  prob_hosp = prob_hosp,
                                  mixing_matrix = baseline_matrix,
                                  R0 = R0)
  }

  # normalise to sum to 1
  p_dist <- p_dist/mean(p_dist)

  # Collate Parameters Into List
  pars <- list(N_age = length(population),
               S_0 = mod_init$S,
               E1_0 = mod_init$E1,
               E2_0 = mod_init$E2,
               IMild_0 = mod_init$IMild,
               ICase1_0 = mod_init$ICase1,
               ICase2_0 = mod_init$ICase2,
               IOxGetLive1_0 = mod_init$IOxGetLive1,
               IOxGetLive2_0 = mod_init$IOxGetLive2,
               IOxGetDie1_0 = mod_init$IOxGetDie1,
               IOxGetDie2_0 = mod_init$IOxGetDie2,
               IOxNotGetLive1_0 = mod_init$IOxNotGetLive1,
               IOxNotGetLive2_0 = mod_init$IOxNotGetLive2,
               IOxNotGetDie1_0 = mod_init$IOxNotGetDie1,
               IOxNotGetDie2_0 = mod_init$IOxNotGetDie2,
               IMVGetLive1_0 = mod_init$IMVGetLive1,
               IMVGetLive2_0 = mod_init$IMVGetLive2,
               IMVGetDie1_0 = mod_init$IMVGetDie1,
               IMVGetDie2_0 = mod_init$IMVGetDie2,
               IMVNotGetLive1_0 = mod_init$IMVNotGetLive1,
               IMVNotGetLive2_0 = mod_init$IMVNotGetLive2,
               IMVNotGetDie1_0 = mod_init$IMVNotGetDie1,
               IMVNotGetDie2_0 = mod_init$IMVNotGetDie2,
               IRec1_0 = mod_init$IRec1,
               IRec2_0 = mod_init$IRec2,
               R_0 = mod_init$R,
               D_0 = mod_init$D,
               gamma_E = gamma_E,
               gamma_IMild = gamma_IMild,
               gamma_ICase = gamma_ICase,
               gamma_get_ox_survive = gamma_get_ox_survive,
               gamma_get_ox_die = gamma_get_ox_die,
               gamma_not_get_ox_survive = gamma_not_get_ox_survive,
               gamma_not_get_ox_die = gamma_not_get_ox_die,
               gamma_get_mv_survive = gamma_get_mv_survive,
               gamma_get_mv_die = gamma_get_mv_die,
               gamma_not_get_mv_survive = gamma_not_get_mv_survive,
               gamma_not_get_mv_die = gamma_not_get_mv_die,
               gamma_rec = gamma_rec,
               pgamma_E = pgamma_E,
               pgamma_ICase = pgamma_ICase,
               pgamma_IMild = pgamma_IMild,
               pgamma_get_ox_survive = pgamma_get_ox_survive,
               pgamma_not_get_ox_survive = pgamma_not_get_ox_survive,
               pgamma_get_ox_die = pgamma_get_ox_die,
               pgamma_not_get_ox_die = pgamma_not_get_ox_die,
               pgamma_get_mv_survive = pgamma_get_mv_survive,
               pgamma_not_get_mv_survive = pgamma_not_get_mv_survive,
               pgamma_get_mv_die = pgamma_get_mv_die,
               pgamma_not_get_mv_die = pgamma_not_get_mv_die,
               pgamma_rec = pgamma_rec,
               prob_hosp = prob_hosp,
               prob_severe = prob_severe,
               prob_non_severe_death_treatment = prob_non_severe_death_treatment,
               prob_non_severe_death_no_treatment = prob_non_severe_death_no_treatment,
               prob_severe_death_treatment = prob_severe_death_treatment,
               prob_severe_death_no_treatment = prob_severe_death_no_treatment,
               p_dist = p_dist,
               hosp_beds = hosp_bed_capacity,
               ICU_beds = ICU_bed_capacity,
               tt_hosp_beds = round(tt_hosp_beds/dt),
               tt_ICU_beds = round(tt_ICU_beds/dt),
               tt_matrix = round(tt_contact_matrix/dt),
               mix_mat_set = matrices_set,
               tt_beta = round(tt_R0/dt),
               beta_set = beta_set,
               dt = dt,
               population = population,
               contact_matrix_set = contact_matrix_set)

  class(pars) <- c("explicit_SEIR_parameters", "squire_parameters")

  return(pars)

}

#' @title Default parameters for SIR model
#'
#' @param overrides use a named parameter list instead of defaults
#' Parameters defined below
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
get_parameters_for_sirstochastic <- function(overrides = list()) {

  pars <- sir_model_parameters_defaults()

  # Override pars with any client specified ones
  if (!is.list(overrides) && !is.null(overrides)) {
    stop('overrides must be a list')
  }

  for (name in names(overrides)) {
    if (!(name %in% names(pars))) {
      stop(paste('unknown parameter', name, sep=' '))
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


