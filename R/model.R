#' @title Run the simulation
#' @description
#' The main entrypoint for the simulation. run_simulation puts together the
#' model components and runs the malaria simulation. This currently returns a
#' dataframe with the number of individuals in each state at each timestep
#'
#' @param timesteps the number of timesteps to run the simulation for.
#' @param pop population. See [squire::get_population]
#' @param parameters parameters list.
#'   See [squire::parameters_explicit_SEEIR]
#' @param processes processes
#' @export
run_simulation <- function(timesteps = NULL, pop, parameters = NULL, processes) {

  if (is.null(parameters)) {
    parameters <- squire::parameters_explicit_SEEIR(
      population = pop$n,  time_period = timesteps)
  }

  if (is.null(timesteps)) {
    timesteps = parameters$time_period
  }

  parameters <- remove_non_numerics(parameters)
  states <- create_states(parameters)
  variables <- create_variables(pop)
  events <- create_events()
  human <- create_human(states,
                        variables,
                        events)

  output <- individual::simulate(
    individuals = human,
    processes = processes,
    end_timestep  = timesteps,
    parameters = parameters
  )

  output
}
