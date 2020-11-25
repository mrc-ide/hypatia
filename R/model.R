
#' @title Run the simulation
#' @description
#' The main entrypoint for the simulation. run_simulation puts together the
#' model components and runs the malaria simulation. This currently returns a
#' dataframe with the number of individuals in each state at each timestep
#'
#' @param pop population. See [squire::get_population]
#' @param parameters parameter list
#' @param max_age maximum age defaults to 100
#' @export
run_simulation <- function(pop, parameters, max_age = 100) {
  parameters <- remove_non_numerics(parameters)
  states <- create_states(parameters)
  variables <- create_variables(pop, max_age)
  events <- create_events()
  human <- create_human(states, variables, events)
  individual::simulate(
    individuals = list(human),
    processes = create_processes(human, states, events, variables),
    end_timestep = parameters$time_period,
    parameters = parameters
  )
}
