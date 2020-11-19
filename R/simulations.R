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

  psq<- get_parameters(countryname, R0, timestep, dt, time_period,
                             tt_contact_matrix, newpopulation, numberof_days)

  dt <- psq$dt

  numberof_days <- 5

  beta <- squire::beta_est_explicit(psq$dur_IMild, psq$dur_ICase,
                                    psq$prob_hosp,
                                    psq$mix_mat_set[1, , ], R0 = R0)

  if (is.null(newpopulation)) {
    newpopulation <- pop$n
  }

  states <- create_states(psq)

  indivs <- create_individuals(states)

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

}
