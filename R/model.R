#' @title Run the simulation
#' @description
#' The main entrypoint for the simulation. run_simulation puts together the
#' model components and runs the malaria simulation. This currently returns a
#' dataframe with the number of individuals in each state at each timestep
#'
#' @param timesteps the number of timesteps to run the simulation for
#' @param pop population. See \code{\link[squire]{get_population}}
#' @param parameters parameters list. 
#'   See \code{\link[squire]{parameters_explicit_SEEIR}}
#' @param ... Other parameters for 
#'   \code{\link[squire]{parameters_explicit_SEEIR}}
#' @export
run_simulation <- function(timesteps, pop, parameters = NULL, ...) {
  events <- create_events()
  if (is.null(parameters)) {
    parameters <- squire::parameters_explicit_SEEIR(population = pop$n, ...)
  }
  parameters <- remove_non_numerics(parameters)
  states <- create_states(parameters)
  variables <- create_variables(pop = pop)
  individuals <- create_individuals(states, variables, events, parameters)
  create_event_based_processes(individuals, states, variables, events, parameters)
  output <- individual::simulate(
    individuals = individuals,
    processes = create_processes(individuals,
                                 states,
                                 events,
                                 variables),
    end_timestep = timesteps,
    parameters = parameters,
    initialisation = create_setup_process(individuals, states, events, variables)
  )
  
  return(output)
}
