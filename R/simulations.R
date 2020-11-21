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

#' @title Run the simulation
#' @description
#' The main entrypoint for the simulation. run_simulation puts together the
#' model components and runs the malaria simulation. This currently returns a
#' dataframe with the number of individuals in each state at each timestep
#'
#' Warning: the columns of the output dataframe is likely to change as we figure
#' out what kind of outputs we would like to report from the simulation.
#'
#' @param timesteps the number of timesteps to run the simulation for
#' @param psq a named list of parameters to use
#' @export
run_simulation2 <- function(timesteps, psq) {

  events <- create_events()
  states <- create_states(psq)
  variables <- create_variables(psq)
browser()

  individuals <- create_individuals(states, variables, events, psq)
  create_event_based_processes(individuals, states, variables, events,
                               psq)

  processes <- list(
    infection_process(individuals, states, variables, events)
  )


  individual::simulate(
    individuals = individuals,
    processes = processes,
    end_timestep = timesteps,
    parameters = parameters,
    initialisation = create_setup_process(events)
  )
}
