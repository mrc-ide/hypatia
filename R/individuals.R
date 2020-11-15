#' @title Define model states
#' @description
#' Create_states creates and initialises the human states for the model
#'
#' @title Create and initialise states
#'
#' @param psq the model parameters
#'
#' @return states
create_states <- function(psq) {

  states <- list(
    # Human states
    S = individual::State$new("S", sum(psq$S_0)),
    E1 =  individual::State$new("E1", sum(psq$E1_0)),
    E2 = individual::State$new("E2", sum(psq$E2_0)),
    IMild = individual::State$new("IMild", sum(psq$IMild_0)),
    ICase1 = individual::State$new("ICase1", sum(psq$ICase1_0)),
    ICase2 = individual::State$new("ICase2", sum(psq$ICase2_0)),
    IOxGetLive1 = individual::State$new("IOxGetLive1", sum(psq$IOxGetLive1_0)),
    IOxGetLive2 = individual::State$new("IOxGetLive2", sum(psq$IOxGetLive2_0)),
    IOxGetDie1 = individual::State$new("IOxGetDie1", sum(psq$IOxGetDie1_0)),
    IOxGetDie2 = individual::State$new("IOxGetDie2", sum(psq$IOxGetDie2_0)),
    IOxNotGetLive1 = individual::State$new("IOxNotGetLive1",
                                            sum(psq$IOxNotGetLive1_0)),
    IOxNotGetLive2 = individual::State$new("IOxNotGetLive2",
                                            sum(psq$IOxNotGetLive2_0)),
    IOxNotGetDie1 = individual::State$new("IOxNotGetDie1",
                                           sum(psq$IOxNotGetDie1_0)),
    IOxNotGetDie2 = individual::State$new("IOxNotGetDie2",
                                           sum(psq$IOxNotGetDie2_0)),
    IMVGetLive1 = individual::State$new("IMVGetLive1", sum(psq$IMVGetLive1_0)),
    IMVGetLive2 = individual::State$new("IMVGetLive2", sum(psq$IMVGetLive2_0)),
    IMVGetDie1 = individual::State$new("IMVGetDie1", sum(psq$IMVGetDie1_0)),
    IMVGetDie2 = individual::State$new("IMVGetDie2", sum(psq$IMVGetDie2_0)),
    IMVNotGetLive1 = individual::State$new("IMVNotGetLive1",
                                            sum(psq$IMVNotGetLive1_0)),
    IMVNotGetLive2 = individual::State$new("IMVNotGetLive2",
                                            sum(psq$IMVNotGetLive2_0)),
    IMVNotGetDie1 = individual::State$new("IMVNotGetDie1",
                                           sum(psq$IMVNotGetDie1_0)),
    IMVNotGetDie2 = individual::State$new("IMVNotGetDie2",
                                           sum(psq$IMVNotGetDie2_0)),
    IRec1 = individual::State$new("IRec1", sum(psq$IRec1_0)),
    IRec2 = individual::State$new("IRec2", sum(psq$IRec2_0)),
    R = individual::State$new("R", sum(psq$R_0)),
    D = individual::State$new("D", sum(psq$D_0))
  )

  states
}

#' @title Define model individuals
#' @description
#' Create_individuals declares the individuals to simulate. It assigns the
#' relevant states and variables to each individual.
#'
#' @param states available states to assign
#' @param variables available variables to assign
#' @param events available events to assign
#' @param parameters model parameters
create_individuals <- function(
  states,
  variables,
  events,
  parameters
) {
  human <- individual::Individual$new(
    "human",
    states = list(
      states$S,
      states$E1,
      states$E2,
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

    variables = list(),

    events = list()
  )

  list(human = human)
}

#' @title probabilities of the states
#'
#' @param dt time step
#' @param psq parameters
#'
#' @return states
probabilities_of_states <- function(dt, psq) {

  # probabilities
  pstates <- list(
    pgamma_E = 1 - exp(-1.0 * (psq$gamma_E * dt)),
    pgamma_IMild = 1 - exp(-psq$gamma_IMild * dt),
    pgamma_ICase = 1 - exp(-1.0 * (psq$gamma_ICase * dt)),
    pgamma_get_ox_survive = 1 - exp(-1.0 * (psq$gamma_get_ox_survive * dt)),
    pgamma_not_get_ox_survive =
      1 - exp(-1.0 * (psq$gamma_not_get_ox_survive * dt)),
    pgamma_get_ox_die = 1 - exp(-1.0 * (psq$gamma_get_ox_die * dt)),
    pgamma_not_get_ox_die = 1 - exp(-1.0 * (psq$gamma_not_get_ox_die * dt)),
    pgamma_get_mv_survive = 1 - exp(-1.0 * (psq$gamma_get_mv_survive * dt)),
    pgamma_not_get_mv_survive =
      1 - exp(-1.0 * (psq$gamma_not_get_mv_survive * dt)),
    pgamma_get_mv_die = 1 - exp(-1.0 * (psq$gamma_get_mv_die * dt)),
    pgamma_not_get_mv_die  = 1 - exp(-1.0 * (psq$gamma_not_get_mv_die * dt)),
    pgamma_rec  = 1 - exp(-1.0 * (psq$gamma_rec * dt)))

  pstates
}

#' @title Continuous age variable
#' @description Create a continuous age variable for the population
#'
#' @param pop population list
#' @param max_age maximum age to be drawn
#'
#' @return continuous age variable
#' @importFrom stats dexp
create_continuous_age_variable <- function(pop, max_age = 100) {

  # get out counntry median ages
  iso3c <- pop$iso3c[1]
  med_age <- iso3c_ages$age[iso3c_ages$iso3c == iso3c]
  
  # get the top end of the 5 year age bins
  age_bins <- c(0, as.numeric(gsub("^(\\d{1,2}).*", "\\1", pop$age_group)[-1]))
  
  # use these to work out the ages in each bin  
  r <- list()
  for(i in seq_along(age_bins)[-1]){
    r[[i-1]] <- seq(age_bins[i-1], age_bins[i]-1, 1)
  }
  r[[length(r) + 1]] <- seq(max(age_bins), max_age, 1)

  # now sample from these
  ages <- list()
  for(i in seq_len(length(pop$age_group))) {
    ages[[i]] <- sample(r[[i]], pop$n[i], replace = TRUE,
                        prob = dexp(r[[i]], 1/(med_age*365)))
  }

  ages <- unlist(ages)
  return(ages)
}

#' @title Discrete age variable
#' @description Create a discrete age variable for each of the
#' length(pop$age_group) distinct age groups
#' 
#' @inheritParams create_continuous_age_variable
#' @param pop Vector of integer ages created by 
#'   \code{\link{create_continuous_age_variable}}
#'
#' @return discrete age variable
create_discrete_age_variable <- function(ages, pop) {

  # get the top end of the 5 year age bins
  age_bins <- c(0, as.numeric(gsub("^(\\d{1,2}).*", "\\1", pop$age_group)[-1]))
  age_bins <- c(age_bins, max(ages))
  
  # put these into bins
  disc_ages <- cut(ages, age_bins, include.lowest = TRUE, right = FALSE)
  disc_ages <- pop$age_group[as.numeric(disc_ages)]
  
  return(disc_ages)
}
