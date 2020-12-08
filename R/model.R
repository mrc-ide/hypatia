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
run_simulation <- function(pop, parameters = NULL, max_age = 100) {

  if (is.null(parameters)) {
    parameters <- get_parameters(
      iso3c = pop$iso3c[1])
  }

  parameters <- remove_non_numerics(parameters)
  variables <- create_variables(pop, max_age)

  # adjust our age variables to account for age based seeding
  variables$discrete_age$initial_values <- adjust_seeding_ages(
    initial_values = variables$discrete_age$initial_values,
    parameters = parameters
  )

  states <- create_states(parameters)
  events <- create_events()

  human <- create_human(states, variables, events)

  create_event_based_processes(human, states, variables,
                               events, parameters)

  output <- individual::simulate(
    individuals = list(human),
    processes = create_processes(human,
                                 states,
                                 events,
                                 variables,
                                 parameters),
    end_timestep  = parameters$time_period,
    parameters = parameters,
    initialisation = create_setup_process(human, states, events, variables)
  )

  output

}

#' @title Run the simulation with repetitions
#'
#' @param repetitions n times to run the simulation
#' @param overrides a named list of parameters to use instead of defaults
#' @param parallel execute runs in parallel
#' @return data frame for runs
#' @export
run_simulation_replicate <- function(
  repetitions,
  overrides = list(),
  parallel = FALSE
) {

  # Currently running sequentially only
  fapply <- lapply

  # Currently running sequentially only
  counter <- 0

  dfs <- fapply(

    seq(repetitions),
    function(repetition) {
      counter <<- counter + 1
      df <- run_simulation(pop = overrides$pop[[counter]],
                           parameters = overrides$parameters[[counter]],
                           max_age = overrides$max_age[[counter]])
      df$repetition <- repetition
      df
    }

  )

  do.call("rbind", dfs)

}
