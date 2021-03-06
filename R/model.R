#' @title Run the simulation
#' @description
#' The main entrypoint for the simulation. run_simulation puts together the
#' model components and runs the malaria simulation. This currently returns a
#' dataframe with the number of individuals in each state at each timestep
#'
#' @param pop population. See [squire::get_population]
#' @param parameters model parameters
#' @export
run_simulation <- function(pop, parameters) {

  variables <- create_variables(pop, parameters)
  states <- create_states(parameters)
  events <- create_events()
  human <- create_human(states, variables, events)
  create_event_based_processes(human, states, variables, events, parameters)

  individual::simulate(
    individuals = list(human),
    processes = create_processes(
      human,
      states,
      events,
      variables,
      parameters
    ),
    end_timestep  = parameters$time_period,
    parameters = parameters,
    initialisation = create_setup_process(human, states, events, variables)
  )
}

#' @title Run the simulation with repetitions
#'
#' @param repetitions n times to run the simulation
#' @param overrides a named list of parameters to use instead of defaults
#' @return data frame for runs
#' @export
run_simulation_replicate <- function(
  repetitions,
  overrides = list()
) {

  # Currently running sequentially only

  dfs <- lapply(

    seq(repetitions),
    function(repetition) {
      df <- run_simulation(pop = overrides$pop[[repetition]],
                           parameters = overrides$parameters[[repetition]])
      df$repetition <- repetition
      df
    }

  )

  do.call("rbind", dfs)

}
