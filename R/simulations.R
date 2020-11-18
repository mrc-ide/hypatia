#' @title Run simulation
#' @description Run the simulation for a given country
#'
#' @param countryname country name
#' @param R0 R0
#' @param timestep time step
#' @param dt dt
#' @param time_period time period
#' @param tt_contact_matrix tt contact matrix
#' @param newpopulation set this to NULL to use pop$n
#' @param numberof_days number of days to run for
#'
#' @export
#'
#' @return data frame
run_simulation <- function(countryname, R0, timestep, dt, time_period,
tt_contact_matrix, newpopulation = NULL, numberof_days) {

  pop <- squire::get_population(countryname, simple_SEIR = FALSE)

  psq <- squire::parameters_explicit_SEEIR(
    population = pop$n,
    dt = dt,
    R0 = R0,
    tt_contact_matrix = tt_contact_matrix,
    time_period = time_period,
    contact_matrix_set = squire::contact_matrices[[1]])

  dt <- psq$dt

  numberof_days <- 5

  beta <- squire::beta_est_explicit(psq$dur_IMild, psq$dur_ICase,
                                    psq$prob_hosp,
                                    psq$mix_mat_set[1, , ], R0 = R0)

  if (is.null(newpopulation)) {
    newpopulation <- pop$n
  }

  states <- create_states(psq)

  events <- create_events()
  variables <- list()

  indivs <- create_individuals(states, variables, events)

  pstates <- probabilities_of_states(dt, psq)

  E_I <- NULL

  p_E_I <- pstates$pgamma_E

  processes <- create_processes(
    indivs,
    states,
    pstates,
    psq,
    newpopulation,
    beta,
    E_I,
    numberof_days)

  out <- individual::simulate(
    indivs$human,
    processes,
    timestep
  )

  df <- data.frame(
    S = out$S,
    E = out$E,
    IMild = out$IMild,
    ICase1 = out$ICase1,
    ICase2 = out$ICase2,
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

}
