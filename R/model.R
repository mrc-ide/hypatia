
#' @title Run the simulation
#' @description
#' The main entrypoint for the simulation. run_simulation puts together the
#' model components and runs the malaria simulation. This currently returns a
#' dataframe with the number of individuals in each state at each timestep
#'
#' @param pop population. See \code{\link[squire]{get_population}}
#' @param parameters parameter list
#' @param max_age maximum age defaults to 100
#' @export
run_simulation <- function(pop, parameters = NULL, max_age = 100) {

  if (is.null(parameters)) {
    parameters <- get_parameters(
      population = pop$n, contact_matrix_set = squire::contact_matrices[1])
  }

  variables <- create_variables(pop, max_age)
  parameters <- remove_non_numerics(parameters)

  variables <- create_variables(pop, max_age)
  states <- create_states(parameters)
  events <- create_events()
  individuals <- create_human(states,
                        variables,
                        events)

  create_event_based_processes(individuals, states, variables,
                               events, parameters)

  output <- individual::simulate(
    individuals = individuals,
    processes = create_processes(individuals,
                                 states,
                                 events,
                                 variables),
    end_timestep  = parameters$time_period,
    parameters = parameters,
    initialisation = create_setup_process(individuals, states, events,
                                          variables)
  )

  output

}
