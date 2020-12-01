
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

  parameters <- remove_non_numerics(parameters)

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
    end_timestep  = parameters$time_period,
    parameters = parameters
  )

  output

}
