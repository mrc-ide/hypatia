#' @title Calculating the FOI
#'
#' @description calculating the FOI and infection process
#' # NOTE function is not being used and maybe superceded by OJs work
#' ###NOTE: # E_I not being used in E_IMild_ICase1_ICase2 for time being
#'
#' @param human, human
#' @param S Susceptible
#' @param E Infection compartments
#' @param population_size population_size
#' @param beta beta
#' @param m m
#' @param dt dt
#' @param IMild rest of the infections, which we consider to be mild and not
#'  require hospitalisation
#' @param ICase1 First of the compartments for infections that will require
#'  hospitalisation
#' @param ICase2 Second of the compartments for infections that will require
#'  hospitalisation
#' @importFrom stats runif
infection_process <- function(human, S, E, population_size, beta, m, dt, IMild,
                     ICase1, ICase2) {

  function(api) {
    newimild <- api$get_state(human, IMild)
    newicase1 <- api$get_state(human, ICase1)
    newicase2 <- api$get_state(human, ICase2)

    susceptible <- api$get_state(human, S)

    stateslist = list(newimild = newimild, newicase1 = newicase1,
                newicase2 = newicase2,
                susceptible = susceptible)

    get_3_states <- list(stateslist$newimild, stateslist$newicase1,
                         stateslist$newicase2)

    probability_of_infection <- 0
    E_I <- NULL

    bernoulli_multi_p <- function(size, p) runif(size, 0, 1) < p

    # If IMild = ICase1 = ICase2 = 0, FOI = 0, i.e. no infected individuals
    if (length(get_3_states) == 0) {
      probability_of_infection  <- 0.0
      validated_state_update(human, E, 0, sum(population_size))
    }
    else {
      # Calculate FoI and use to create probability
      lambda <- beta * length(get_3_states) * mean(m)

      if (!isEmpty(lambda)) {
        probability_of_infection  <- 1 - exp(-lambda * dt)

        #Transition from S to E
        # susceptible <- api$get_state(human, S)

        temp <- bernoulli_multi_p(length(stateslist$susceptible),
                                  probability_of_infection)

        # Do a qstate_update
        validated_state_update(human, E, temp, sum(population_size))

        temp_prob <- 0.1
        # E_I not being used in E_IMild_ICase1_ICase2 for time being
        # Get list of individuals infected
        E_I <- rbinom(length(E), 1, temp_prob)

      }
      else {
        # E_I not being used in E_IMild_ICase1_ICase2 for time being
        probability_of_infection  <- 0.0
        E_I <- NULL
        validated_state_update(human, E, 0, sum(population_size))
      }
    }

    # E_I not being used in E_IMild_ICase1_ICase2 for time being
    #return(E_I)

  }

}

isEmpty <- function(x) {
  return(length(x) == 0)
}
