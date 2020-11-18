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

  pE <- sum(psq$E1_0) + sum(psq$E2_0)

  states <- list(
    # Human states
    S = individual::State$new("S", sum(psq$S_0)),
    E =  individual::State$new("E", pE),
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
#' @param variables is a list of variables
#' @param events is a list of variables
#' @param states list of states
create_individuals <- function(
  states,
  variables,
  events
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


#' @title Create events list
#'
#' @return list of events
create_events <- function() {

  events <- list(

    # infection events
    imild_event <- individual::Event$new('IMild'),
    icase1_event <- individual::Event$new('ICase1')

  )

  events

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
#'
#' @return continuous age variable
#' @importFrom stats dexp
create_continuous_age_variable <- function(pop) {

  r <- list()
  for(i in 1:(85/5)){
    r[[i]] <- (1 + ((i-1) * 5)) : (5*i)
  }

  ages <- list()
  for(i in seq_len(length(pop$age_group))) {
    ages[[i]] <- sample(r[[i]], pop$n[i], replace = TRUE,
                        prob = dexp(r[[i]], 1/(21)))
  }

  ages <- unlist(ages)
  return(ages)
}

#' @title Discrete age variable
#' @description Create a discrete age variable for each of the
#' length(pop$age_group) distinct age groups
#'
#' @param pop population list
#'
#' @return discrete age variable
create_discrete_age_variable <- function(pop) {

  age_cont <- create_continuous_age_variable(pop)

  age_bins <- levels(cut(age_cont, breaks = c(seq(0, 80, 5), 999),
                         include.lowest = TRUE))

  # TO DO
  stop("Not yet implemented")
}
