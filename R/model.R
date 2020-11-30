#' @title Run the simulation
#' @description
#' The main entrypoint for the simulation. run_simulation puts together the
#' model components and runs the malaria simulation. This currently returns a
#' dataframe with the number of individuals in each state at each timestep
#'
#' @param country country simulation is being run for
#' @param timesteps the number of timesteps to run the simulation for.
#' @param pop population. See [squire::get_population]
#' @param parameters parameters list.
#'   See [squire::parameters_explicit_SEEIR]
#' @param contact_matrix_set contact_matrix_set
#' @param max_age maximum age defaults to 100
#' @export
run_simulation <- function(country, timesteps = NULL, pop, parameters = NULL,
                           contact_matrix_set, max_age = 100) {

  if (is.null(parameters) && !is.null(timesteps)) {
    parameters <- squire::parameters_explicit_SEEIR(
      country = country,
      contact_matrix_set = contact_matrix_set,
      time_period = timesteps
    )
  }

  parameters <- remove_non_numerics(parameters)

  if (is.null(timesteps) && !is.null(parameters)) {
    timesteps = parameters$time_period
  }

  variables <- create_variables(pop, max_age)
  parameters <- remove_non_numerics(parameters)
  states <- create_states(parameters)
  events <- create_events()
  human <- create_human(states,
                        variables,
                        events)

  output <- individual::simulate(
    individuals = human,
    processes = create_processes(parameters,
                                 pop,
                                 max_age),
    end_timestep  = timesteps,
    parameters = parameters
  )

  output
}
