#' @title Define model states
#' @description
#' Create_states creates the human and mosquito states for the model
#'
#' @title Create states
#'
#' @export
#'
#' @param psq the model parameters
#'
#' @return states
Create_states <- function(psq) {

  # initial_counts <- calculate_initial_counts(psq)
  states <- list(
    # Human states
    S <- individual::State$new("S", psq$S_0),
    E1 <- individual::State$new("E1", psq$E1_0),
    E2 <- individual::State$new("E2", psq$E2_0),
    IMild <- individual::State$new("IMild", psq$IMild_0),
    ICase1 <- individual::State$new("ICase1", psq$ICase1_0),
    ICase2 <- individual::State$new("ICase2", psq$ICase2_0),
    # cum_hosp_inc <- individual::State$new("cum_hosp_inc", NR),
    # cum_ICU_inc <- individual::State$new("cum_ICU_inc", NR)
    IOxGetLive1 <- individual::State$new("IOxGetLive1", psq$IOxGetLive1_0),
    IOxGetLive2 <- individual::State$new("IOxGetLive2", psq$IOxGetLive2_0),
    IOxGetDie1 <- individual::State$new("IOxGetDie1", psq$IOxGetDie1_0),
    IOxGetDie2 <- individual::State$new("IOxGetDie2", psq$IOxGetDie2_0),
    IOxNotGetLive1 <- individual::State$new("IOxNotGetLive1",
                                            psq$IOxNotGetLive1_0),
    IOxNotGetLive2 <- individual::State$new("IOxNotGetLive2",
                                            psq$IOxNotGetLive2_0),
    IOxNotGetDie1 <- individual::State$new("IOxNotGetDie1",
                                           psq$IOxNotGetDie1_0),
    IOxNotGetDie2 <- individual::State$new("IOxNotGetDie2",
                                           psq$IOxNotGetDie2_0),
    IMVGetLive1 <- individual::State$new("IMVGetLive1", psq$IMVGetLive1_0),
    IMVGetLive2 <- individual::State$new("IMVGetLive2", psq$IMVGetLive2_0),
    IMVGetDie1 <- individual::State$new("IMVGetDie1", psq$IMVGetDie1_0),
    IMVGetDie2 <- individual::State$new("IMVGetDie2", psq$IMVGetDie2_0),
    IMVNotGetLive1 <- individual::State$new("IMVNotGetLive1",
                                            psq$IMVNotGetLive1_0),
    IMVNotGetLive2 <- individual::State$new("IMVNotGetLive2",
                                            psq$IMVNotGetLive2_0),
    IMVNotGetDie1 <- individual::State$new("IMVNotGetDie1",
                                           psq$IMVNotGetDie1_0),
    IMVNotGetDie2 <- individual::State$new("IMVNotGetDie2",
                                           psq$IMVNotGetDie2_0),
    IRec1 <- individual::State$new("IRec1", psq$IRec1_0),
    IRec2 <- individual::State$new("IRec2", psq$IRec2_0),
    R <- individual::State$new("R", psq$R_0),
    D <- individual::State$new("D", psq$D_0)
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
#'
#' @export
#'
Create_individuals <- function(
  states,
  variables,
  events,
  parameters
) {
  human <- individual::Individual$new(
    'human',
    states = list(states$S, states$E1, states$E1, states$IMild, states$ICase1,
                  states$ICase2, states$IOxGetLive1, states$IOxGetLive2,
                  states$IOxGetDie1, states$IOxGetDie2, states$IOxNotGetLive1,
                  states$IOxNotGetLive2, states$IMVNotGetDie1,
                  states$IMVNotGetDie2, states$IMVNotGetDie1,
                  states$IMVNotGetDie2, states$IRec1, states$IRec2, states$R,
                  states$D),
    variables = list(
    ),
    events = c(
    )
  )

  list(human = human)
}

#' @title probabilities of the states
#'
#' @param dt time step
#'
#' @return states
#' @export
#'
#' @examples
#' Probabilities_of_states(dt)
Probabilities_of_states <- function(dt) {

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
  pstates = list(pgamma_E = 1 - exp(-1.0 * (gamma_E * dt)),
    pgamma_IMild = 1 - exp(-gamma_IMild * dt),
    pgamma_ICase = 1 - exp(-1.0 * (gamma_ICase * dt)),
    pgamma_get_ox_survive = 1 - exp(-1.0 * (gamma_get_ox_survive * dt)),
    pgamma_not_get_ox_survive = 1 - exp(-1.0 * (gamma_not_get_ox_survive * dt)),
    pgamma_get_ox_die = 1 - exp(-1.0 * (gamma_get_ox_die * dt)),
    pgamma_not_get_ox_die = 1 - exp(-1.0 * (gamma_not_get_ox_die * dt)),
    pgamma_get_mv_survive = 1 - exp(-1.0 * (gamma_get_mv_survive * dt)),
    pgamma_not_get_mv_survive = 1 - exp(-1.0 * (gamma_not_get_mv_survive * dt)),
    pgamma_get_mv_die = 1 - exp(-1.0 * (gamma_get_mv_die * dt)),
    pgamma_not_get_mv_die  = 1 - exp(-1.0 * ( gamma_not_get_mv_die * dt)),
    pgamma_rec  = 1 - exp(-1.0*( gamma_rec * dt)))

  pstates
}
