#' @title Define model individuals
#' @description
#' Create_individuals declares the individuals to simulate. It assigns the
#' relevant states and variables to each individual.
#'
#' @param states available states to assign
#' @param variables list of variables
#' @param events list of events
#' @param parameters list of parameters
create_individuals <- function(
  states, variables, events, parameters
) {
  human <- individual::Individual$new(
    "human",
    states = list(
      states$S,
      states$E,
      states$IMild,
      states$ICase1,
      states$ICase2,
      states$IOxGetLive1,
      states$IOxGetLive2,
      states$IOxGetDie1,
      states$IOxGetDie2,
      states$IOxNotGetLive1,
      states$IOxNotGetLive2,
      states$IOxNotGetDie1,
      states$IOxNotGetDie2,
      states$IMVGetLive1,
      states$IMVGetLive2,
      states$IMVGetDie1,
      states$IMVGetDie2,
      states$IMVNotGetLive1,
      states$IMVNotGetLive2,
      states$IMVNotGetDie1,
      states$IMVNotGetDie2,
      states$IRec1,
      states$IRec2,
      states$R,
      states$D),
    variables = variables,
    events = events
  )

  list(human = human)
}


#' @title Define model variables
#' @description
#' create_variables creates the human and mosquito variables for
#' the model. Variables are used to track real data for each individual over
#' time, they are read and updated by processes
#'
#' The human variables are defined as:
#'
#' * birth - an integer representing the timestep when this individual was born
#' * last_boosted_* - the last timestep at which this individual's immunity was
#' boosted for tracking grace periods in the boost of immunity
#' * is_severe - a binary indicator (0 or 1) for if the individual currently has
#' severe malaria
#' * ICM - Maternal immunity to clinical disease
#' * IVM - Maternal immunity to severe disease
#' * IB  - Pre-erythoctic immunity
#' * ICA  - Acquired immunity to clinical disease
#' * IVA  - Acquired immunity to severe disease
#' * ID - Acquired immunity to detectability
#' * zeta - Heterogeneity of human individuals
#' * zeta_group - Discretised heterogeneity of human individuals
#' * rtss_vaccinated - The timstep of the last rtss vaccination (-1 if there
#' haven't been any)
#' * rtss_boosted  - The timstep of the last rtss booster (-1 if there
#' haven't been any)
#' * rtss_cs - peak antibodies
#' * rtss_rho - antibody component variable
#' * rtss_ds - short-lived antibody delay variable
#' * rtss_dl - long-lived antibody delay variable
#' * zeta_group - Discretised heterogeneity of human individuals
#'
#' Mosquito variables are:
#' * variety - The variety of mosquito, either 1, 2 or 3. These are related to
#' blood meal rate parameter
#' * infectivity - The onward infectiousness to mosquitos
#' * drug - The last prescribed drug
#' * drug_time - The timestep of the last drug
#'
#' @param psq, model parameters created by `get_parameters`
#' @importFrom stats rexp rnorm
create_variables <- function(psq) {

  immunity <- individual::Variable$new('immunity',  rep(0, sum(psq$population)))
  age  <- individual::Variable$new('age', rep(0, sum(psq$population)))
  location <- individual::Variable$new('location', rep(0, sum(psq$population)))

  #
  # # Define variables
  # birth <- individual::Variable$new("birth", function(size) -initial_age)
  # last_boosted_ib <- individual::Variable$new("last_boosted_ib", function(size) { rep(-1, size) })
  # last_boosted_ica <- individual::Variable$new("last_boosted_ica", function(size) { rep(-1, size) })
  # last_boosted_iva <- individual::Variable$new("last_boosted_iva", function(size) { rep(-1, size) })
  # last_boosted_id <- individual::Variable$new("last_boosted_id", function(size) { rep(-1, size) })
  # is_severe <- individual::Variable$new(
  #   "is_severe",
  #   function(size) { rep(0, size) }
  # )
  #
  # # Maternal immunity
  # icm <- individual::Variable$new(
  #   "ICM",
  #   function(size) {
  #     first_immunity <- parameters$init_icm
  #     t <- initial_age * 365 / parameters$days_per_timestep
  #     first_immunity * exp(-(t * parameters$rm))
  #   }
  # )
  #
  # ivm <- individual::Variable$new(
  #   "IVM",
  #   function(size) {
  #     first_immunity <- parameters$init_ivm
  #     t <- initial_age * 365 / parameters$days_per_timestep
  #     first_immunity * exp(-(t * parameters$rm))
  #   }
  # )
  #
  # # Pre-erythoctic immunity
  # ib  <- individual::Variable$new(
  #   "IB",
  #   function(size) initial_immunity(parameters$init_ib, initial_age)
  # )
  # # Acquired immunity to clinical disease
  # ica <- individual::Variable$new(
  #   "ICA",
  #   function(size) initial_immunity(parameters$init_ica, initial_age)
  # )
  # # Acquired immunity to severe disease
  # iva <- individual::Variable$new(
  #   "IVA",
  #   function(size) initial_immunity(parameters$init_iva, initial_age)
  # )
  # # Acquired immunity to detectability
  # id <- individual::Variable$new(
  #   "ID",
  #   function(size) initial_immunity(parameters$init_id, initial_age)
  # )
  #
  # zeta_norm <- rnorm(parameters$human_population)
  # zeta <- individual::Variable$new(
  #   "zeta",
  #   function(n) {
  #     exp(
  #       zeta_norm * sqrt(parameters$sigma_squared) - parameters$sigma_squared/2
  #     )
  #   }
  # )
  #
  # zeta_group <- individual::Variable$new(
  #   "zeta_group",
  #   function(n) {
  #     discretise_normal(zeta_norm, parameters$n_heterogeneity_groups)
  #   }
  # )
  #
  # # Initialise infectiousness of humans -> mosquitoes
  # # NOTE: not yet supporting initialisation of infectiousness of Treated individuals
  # infectivity_values <- rep(0, parameters$human_population)
  # counts <- calculate_initial_counts(parameters)
  #
  # # Calculate the indices of individuals in each infectious state
  # diseased <- counts[[1]]:sum(counts[1:2])  # The index of individuals in the D state
  # asymptomatic <- sum(counts[1:2]):sum(counts[1:3]) # The index of individuals in the A state
  # subpatent <- sum(counts[1:3]):sum(counts[1:4]) # The index of individuals in the U state
  #
  # # Set the initial infectivity values for each individual
  # infectivity_values[diseased] <- parameters$cd
  # infectivity_values[asymptomatic] <- asymptomatic_infectivity(
  #   initial_age[asymptomatic],
  #   initial_immunity(parameters$init_id, initial_age)[asymptomatic],
  #   parameters
  # )
  # infectivity_values[subpatent] <- parameters$cu
  #
  # # Initialise the infectivity variable
  # infectivity <- individual::Variable$new("infectivity", function(n) infectivity_values)
  #
  # drug <- individual::Variable$new("drug", function(n) rep(0, n))
  # drug_time <- individual::Variable$new("drug_time", function(n) rep(-1, n))
  #
  # rtss_vaccinated <- individual::Variable$new(
  #   "rtss_vaccinated",
  #   function(n) rep(-1, n)
  # )
  #
  # rtss_boosted <- individual::Variable$new(
  #   "rtss_boosted",
  #   function(n) rep(-1, n)
  # )
  #
  # rtss_cs <- individual::Variable$new(
  #   "rtss_cs",
  #   function(n) {
  #     exp(parameters$rtss_cs[[1]] + parameters$rtss_cs[[2]] * rnorm(n))
  #   }
  # )
  # rtss_rho <- individual::Variable$new(
  #   "rtss_rho",
  #   function(n) {
  #     invlogit(parameters$rtss_rho[[1]] + parameters$rtss_rho[[2]] * rnorm(n))
  #   }
  # )
  # rtss_ds <- individual::Variable$new(
  #   "rtss_ds",
  #   function(n) {
  #     exp(parameters$rtss_ds[[1]] + parameters$rtss_ds[[2]] * rnorm(n))
  #   }
  # )
  #
  # rtss_dl <- individual::Variable$new(
  #   "rtss_dl",
  #   function(n) {
  #     exp(parameters$rtss_dl[[1]] + parameters$rtss_dl[[2]] * rnorm(n))
  #   }
  # )
  #
  # variables <- list(
  #   birth = birth,
  #   last_boosted_ib = last_boosted_ib,
  #   last_boosted_ica = last_boosted_ica,
  #   last_boosted_iva = last_boosted_iva,
  #   last_boosted_id = last_boosted_id,
  #   icm = icm,
  #   ivm = ivm,
  #   ib = ib,
  #   ica = ica,
  #   iva = iva,
  #   id = id,
  #   zeta = zeta,
  #   zeta_group = zeta_group,
  #   infectivity = infectivity,
  #   drug = drug,
  #   drug_time = drug_time,
  #   rtss_vaccinated = rtss_vaccinated,
  #   rtss_boosted = rtss_boosted,
  #   rtss_cs = rtss_cs,
  #   rtss_rho = rtss_rho,
  #   rtss_ds = rtss_ds,
  #   rtss_dl = rtss_dl,
  #   is_severe = is_severe
  # )
  #
  #
  #
  variables <- list(
    immunity,
    age,
    location
  )

  variables
}
