#' @title Function for transition from E2 to IMild
#'
#' @param human human
#' @param IMild IMild
#' @param E2 E2
#' @param ICase1 ICase1
#' @param p_E2_I p_E2_I
#' @param prob_hosp prob_hosp
#' @param population_size total population size
#'
#' @importFrom stats rbinom
E2_IMild <- function(human, IMild, E2, ICase1, p_E2_I, prob_hosp,
                     population_size) {
  function(api) {
    E2 <- api$get_state(human, E2)
    E2_I <- rbinom(length(E2), 1, p_E2_I)
    E2_I_pos <- E2[as.logical(E2_I)]

    n_E2_ICase1 <- rbinom(length(E2_I_pos), 1, mean(prob_hosp))
    new_ICase1 <- round(E2_I_pos[as.logical(n_E2_ICase1)])
    new_IMild1 <- round(E2_I_pos[!as.logical(n_E2_ICase1)])

    if (length(new_ICase1) != 0) {
      validated_state_update(api, human, ICase1, new_ICase1, sum(population_size))
    }
    if (length(new_IMild1) != 0) {
      validated_state_update(api, human, IMild, new_IMild1, sum(population_size))
    }
  }
}

#' @title Renders the sizes for S, I, R
#' @param S S
#' @param I I
#' @param R R
#' @param human human
render_sir_state_sizes <- function(S, I, R, human) {
  function(api) {
    api$render("susceptable_counts", length(api$get_state(human, S)))
    api$render("infected_counts", length(api$get_state(human, I)))
    api$render("recovered_counts", length(api$get_state(human, R)))
  }
}

#' Render sizes for SQUIRE states - note IMild is missing for now
#'
#' @param S Susceptible
#' @param E1 First of the latent infection compartments
#' @param E2 Second of the latent infection compartments
#' @param IMild rest of the infections, which we consider to be mild and not
#'  require hospitalisation
#' @param ICase1 First of the compartments for infections that will require
#' hospitalisation
#' @param ICase2 Second of the compartments for infections that will require
#' hospitalisation
#' @param IOxGetLive1 First of the compartments for infections that will require
#' oxygen, get it, and who survive
#' @param IOxGetLive2 Second of the compartments for infections that will
#' require oxygen, get it, and who survive
#' @param IOxNotGetLive1 First of the compartments for infections that will
#' require oxygen, do NOT get it, and live
#' @param IOxNotGetLive2 Second of the compartments for infections that will
#'  require oxygen, do NOT get it, and live
#' @param IOxGetDie1 First of the compartments for infections that will
#' require oxygen, get it, and die
#' @param IOxGetDie2 Second of the compartments for infections that will
#' require oxygen, get it, and die
#' @param IOxNotGetDie1 First of the compartments for infections that will
#' require oxygen, do NOT get it, and die
#' @param IOxNotGetDie2 Second of the compartments for infections that will
#'  require oxygen, do NOT get it, and die
#' @param IMVGetLive1 First of the compartments for infections that will
#' require mechanical ventilation, get it, and who survive
#' @param IMVGetLive2 Second of the compartments for infections that will
#' require mechanical ventilation, get it, and who survive
#' @param IMVNotGetLive1 First of the compartments for infections that will
#'  require mechanical ventilation, do NOT get it, and survive
#' @param IMVNotGetLive2 Second of the compartments for infections that will
#'  require mechanical ventilation, do NOT get it, and survive
#' @param IMVGetDie1 First of the compartments for infections that will
#'  require mechanical ventilation, get it, and die
#' @param IMVGetDie2 Second of the compartments for infections that will
#'  require mechanical ventilation, get it, and die
#' @param IMVNotGetDie1 First of the compartments for infections that will
#'  require mechanical ventilation, do NOT get it, and die
#' @param IMVNotGetDie2 Second of the compartments for infections that will
#'  require mechanical ventilation, do NOT get it, and die
#' @param IRec1 First of the compartments for those recovering from ICU
#' @param IRec2 Second of the compartments for those recovering from ICU
#' @param R Recovered
#' @param D Dead
#' @param human human
render_all_state_sizes <- function(S, E1, E2, IMild, ICase1, ICase2,
                                IOxGetLive1, IOxGetLive2,
                                IOxNotGetLive1, IOxNotGetLive2, IOxGetDie1,
                                IOxGetDie2, IOxNotGetDie1, IOxNotGetDie2,
                                IMVGetLive1, IMVGetLive2, IMVNotGetLive1,
                                IMVNotGetLive2, IMVGetDie1, IMVGetDie2,
                                IMVNotGetDie1, IMVNotGetDie2, IRec1, IRec2, R,
                                D, human) {
  function(api) {
    api$render("S", length(api$get_state(human, S)))
    api$render("E1", length(api$get_state(human, E1)))
    api$render("E2", length(api$get_state(human, E2)))
    api$render("IMild", length(api$get_state(human, IMild)))
    api$render("ICase1", length(api$get_state(human, ICase1)))
    api$render("ICase2", length(api$get_state(human, ICase2)))
    api$render("IOxGetLive1", length(api$get_state(human, IOxGetLive1)))
    api$render("IOxGetLive2", length(api$get_state(human, IOxGetLive2)))
    api$render("IOxNotGetLive1", length(api$get_state(human, IOxNotGetLive1)))
    api$render("IOxNotGetLive2", length(api$get_state(human, IOxNotGetLive2)))
    api$render("IOxGetDie1", length(api$get_state(human, IOxGetDie1)))
    api$render("IOxGetDie2", length(api$get_state(human, IOxGetDie2)))
    api$render("IOxNotGetDie1", length(api$get_state(human, IOxNotGetDie1)))
    api$render("IOxNotGetDie2", length(api$get_state(human, IOxNotGetDie2)))
    api$render("IMVGetLive1", length(api$get_state(human, IMVGetLive1)))
    api$render("IMVGetLive2", length(api$get_state(human, IMVGetLive2)))
    api$render("IMVNotGetLive1", length(api$get_state(human, IMVNotGetLive1)))
    api$render("IMVNotGetLive2", length(api$get_state(human, IMVNotGetLive2)))
    api$render("IMVGetDie1", length(api$get_state(human, IMVGetDie1)))
    api$render("IMVGetDie2", length(api$get_state(human, IMVGetDie2)))
    api$render("IMVNotGetDie1", length(api$get_state(human, IMVNotGetDie1)))
    api$render("IMVNotGetDie2", length(api$get_state(human, IMVNotGetDie2)))
    api$render("IRec1", length(api$get_state(human, IRec1)))
    api$render("IRec2", length(api$get_state(human, IRec2)))
    api$render("D", length(api$get_state(human, D)))
    api$render("R", length(api$get_state(human, R)))
  }
}

#' @title Check state update is valid
#'
#' @param api api
#' @param i human etc
#' @param state S, I, R, etc
#' @param ix index
#' @param population_size population size
validated_state_update <- function(api, i, state, ix, population_size) {

  if (any(ix > population_size)) {
    stop(sprintf("Your ix %d for %s:%s is greater than the population size\n",
                 ix, i$name, state$name))
  }
  if (any(ix <= 0)) {
    stop(sprintf("Your ix %d for %s:%s is less than or equal to 0 \n",
                 ix, i$name, state$name))
  }
  if (any(is.na(ix))) {
    stop(sprintf("Your ix %d for %s:%s is not a number \n",
                 ix, i$name, state$name))
  }
  if (any(!is_integer_like(ix))) {
    stop(sprintf("Your ix %d for %s:%s is not an integer\n",
                 ix, i$name, state$name))
  }

  api$queue_state_update(i, state, ix)
}

is_integer_like <- function(x) {
  abs(x - round(x)) < sqrt(.Machine$double.eps)
}
