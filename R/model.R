#' @title Run the simulation
#' @description
#' The main entrypoint for the simulation. run_simulation puts together the
#' model components and runs the malaria simulation. This currently returns a
#' dataframe with the number of individuals in each state at each timestep
#'
#' @param pop population. See [squire::get_population]
#' @param parameters parameters list.
#'   See [squire::parameters_explicit_SEEIR]
#' @export
run_simulation <- function(pop, parameters) {

  parameters <- remove_non_numerics(parameters)
  states <- create_states(parameters)
  variables <- create_variables(pop = pop)
  events <- create_events()

  human <- create_human(states = states, variables = variables, events = events)


  individual::simulate(
    individuals = human,
    processes = list(),
    end_timestep = parameters$time_period,
    parameters = parameters
  )

}
