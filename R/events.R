#' @ Create events
#'
#' @return events
create_events <- function() {

  events = list(

    # Human infection events
    exposure = individual::Event$new('exposure'),
    mild_infection = individual::Event$new('mild_infection'),
    severe_infection = individual::Event$new('severe_infection'), # requiring hospital eventually
    hospitilisation = individual::Event$new('hospitilisation'), # either ICU or MV
    imv_get_live = individual::Event$new('imv_get_live'),
    imv_get_die = individual::Event$new('imv_get_die'),
    iox_get_live = individual::Event$new('iox_get_live'),
    iox_get_die = individual::Event$new('iox_get_die'),
    imv_not_get_live = individual::Event$new('imv_not_get_live'),
    imv_not_get_die = individual::Event$new('imv_not_get_die'),
    iox_not_get_live = individual::Event$new('iox_not_get_live'),
    iox_not_get_die = individual::Event$new('iox_not_get_die'),
    stepdown = individual::Event$new('stepdown'),
    recovery = individual::Event$new('recovery'),
    death = individual::Event$new('deaths')

    # Vaccination events

  )

  events
}

#' @title Update the state of an individual as infection events occur
#' @description Randomly moves individuals towards the later stages of disease
#' and updates their ineffectivity
#'
#' @param human the handle for the human individuals
#' @param to_state the destination disease state
create_infection_update_listener <- function(
  human,
  to_state) {
  function(api, to_move) {
    api$queue_state_update(human, to_state, to_move)
  }
}

#' @title Schedule progression of human disease at the start of the simulation
#' @description Schedules infection events using Erlang
#'
#' @param event the event to schedule
#' @param human the human handle
#' @param from_state the state this event applies to
#' @param duration the average time spent in this state
initialise_progression <- function(event, human, from_state, duration) {
  function(api, target) {
    target <- api$get_state(human, from_state)
    api$schedule(event, target, r_erlang(length(target), duration))
  }
}

#' @title Modelling the progression of the human disease
#' @description schedules follow up infection events
#'
#' @param event the event to schedule
#' @param duration the average time spent in this state
#' @param shift number of days to increase scheduled event by
#' @param func function to use for drawing progression.
#'   Default = \code{\link{r_erlang}}
create_progression_listener <- function(event, duration, shift = 0, func = r_erlang) {
  function(api, target) {
    api$schedule(event, target, func(length(target), duration) + shift)
  }
}

#' @title Modelling the progression to either IMild or ICase
#' @description Age dependent outcome of exposure
#'
#' @param human the handle for the human individuals
#' @param states the available human states
#' @param events a list of events in the model
#' @param variables the available human variables
#' @param parameters model parameters
create_exposure_update_listener <- function(
  human,
  states,
  events,
  variables,
  parameters) {
  function(api, to_move) {
    disc_ages <- api$get_variable(human, variables$discrete_age, to_move)
    prob_hosp <- parameters$prob_hosp[as.integer(disc_ages)]
    hosp <- bernoulli_multi_p(length(to_move), prob_hosp)

    if(sum(hosp) > 0) {
      api$schedule(
        event = events$severe_infection,
        target = to_move[hosp],
        delay = r_erlang(length(to_move[hosp]), parameters$dur_E) + 1
      )
    }
    if(sum(!hosp) > 0){
      api$schedule(
        event = events$mild_infection,
        target = to_move[!hosp],
        delay = r_erlang(length(to_move[!hosp]), parameters$dur_E) + 1
      )
    }
  }
}


#' @title Modelling the progression to either IMv or IOx
#' @description Age dependent outcome of exposure
#'
#' @param human the handle for the human individuals
#' @param states the available human states
#' @param events a list of events in the model
#' @param variables the available human variables
#' @param parameters model parameters
create_hospitilisation_update_listener <- function(
  human,
  states,
  variables,
  parameters,
  events) {
  function(api, to_move) {

    hospitilisation_flow_process(
      api,
      to_move,
      human,
      states,
      variables,
      parameters,
      events
    )

  }
}
