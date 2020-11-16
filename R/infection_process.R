#' @title Calculating the FOI
#'
#' @description calculating the FOI and infection process
#'
#' @param individuals a list of individuals in the model
#' @param states a list of states in the model
#' @param variables list of variables in the model
#' @param events a list of events in the model
#' @importFrom stats runif
infection_process <- function(individuals, states, variables, events) {
  
  function(api) {
    
    pars <- api$get_parameters()
    
    # Generating Force of Infection
    IMild <- api$get_state(individuals$human, states$IMild)
    ICase <- api$get_state(individuals$human, states$ICase)
    inf_states <- c(IMild, ICase)
    
    # If IMild = ICase = 0, FOI = 0, i.e. no infected individuals
    if (length(inf_states) > 0) {
      
      # Group infection by age
      ages <- api$get_variable(individuals$human, variables$discrete_age, inf_states)
      inf_ages <- tabulate(ages, nbins = pars$N_age)
      
      # Calculate FoI and use to create probability for each age group
      lambda <- pars$beta * rowSums(pars$mix_mat_set[1,,] %*% diag(inf_ages))
      
      # Transition from S to E1
      susceptible <- api$get_state(individuals$human, states$S)
      ages <- api$get_variable(individuals$human, variables$discrete_age, susceptible)
      
      # FOI for each susceptible person
      lambda <- lambda[as.integer(ages)]
      prob_infection  <- 1 - exp(-lambda)
      
      # infected
      infected <- bernoulli_multi_p(length(susceptible), prob_infection)
      
      # if infections then 
      if(length(infected) > 0) {
        api$schedule(
          event = events$exposure, 
          target = susceptible[infected],
          delay = 0 # i.e. happens now
        )
      }
      
    }
  }
}




create_processes <- function(
  individuals,
  states,
  events,
  variables
) {
  
  processes <- list(
    
    infection_process(individuals, states, variables, events),
    
    individual::state_count_renderer_process(
      individuals$human$name,
      unlist(lapply(states, "[[", "name"))
    )
    
  )
  
  processes
  
}
