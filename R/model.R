#' @title Run the simulation
#' @description
#' The main entrypoint for the simulation. run_simulation puts together the
#' model components and runs the malaria simulation. This currently returns a
#' dataframe with the number of individuals in each state at each timestep
#'
#' @param pop population. See [squire::get_population]
#' @param parameters parameters list.
#'   See [squire::parameters_explicit_SEEIR]
#' @param renderer renderer
#' @export
run_simulation <- function(pop, parameters, renderer) {

  parameters <- remove_non_numerics(parameters)
  states <- create_states(parameters)
  variables <- create_variables(pop)
  events <- create_events()
  human <- create_human(states,
                        variables,
                        events)
  if (is.null(renderer)) processes <- list()
  if (!is.null(renderer)) processes <- list(renderer)

  individual::simulate(
    individuals = human,
    processes = processes,
    end_timestep = parameters$time_period,
    parameters = parameters,
  )
}
