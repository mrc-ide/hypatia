#' @title Define event based processes at initialisation
#' @description Defines processes for events for states at initialisation
#'
#' @param individuals a list of individuals in the model
#' @param states a list of states in the model
#' @param events a list of events in the model
#' @param variables list of variables in the model
create_setup_process <- function(
   individuals,
   states,
   events,
   variables
) {
   function(api) {
      parameters <- api$get_parameters()
      exposed <- api$get_state(individuals$human, states$E)
      age <- api$get_variable(individuals$human, variables$discrete_age, exposed)
      prob_hosp <- parameters$prob_hosp[as.integer(age)]
      hosp <- bernoulli_multi_p(length(exposed), prob_hosp)

      if(sum(hosp) > 0) {
         api$schedule(
            event = events$severe_infection,
            target = exposed[hosp],
            delay = r_erlang(length(exposed[hosp]), parameters$dur_E)
         )
      }
      if(sum(!hosp) > 0) {
         api$schedule(
            event = events$mild_infection,
            target = exposed[!hosp],
            delay = r_erlang(length(exposed[!hosp]), parameters$dur_E)
         )
      }
   }
}

#' @title Define event based processes
#' @description defines processes for events that can be scheduled in the future
#'
#' @param individuals a list of individuals in the model
#' @param states a list of states in the model
#' @param variables list of variables in the model
#' @param events a list of events in the model
#' @param parameters the model parameters
create_event_based_processes <- function(
   individuals,
   states,
   variables,
   events,
   parameters
) {

   # STATE UPDATES
   # These events cause the infection state to change at the end of the timestep
   # ---------------------

   # Exposure events
   events$exposure$add_listener(
      create_infection_update_listener(
         individuals$human,
         states$E
      )
   )

   # IMild events
   events$mild_infection$add_listener(
      create_infection_update_listener(
         individuals$human,
         states$IMild
      )
   )

   # ICase events
   events$severe_infection$add_listener(
      create_infection_update_listener(
         individuals$human,
         states$ICase
      )
   )

   # IMV events
   events$imv_get_live$add_listener(
      create_infection_update_listener(
         individuals$human,
         states$IMVGetLive
      )
   )

   events$imv_get_die$add_listener(
      create_infection_update_listener(
         individuals$human,
         states$IMVGetDie
      )
   )

   events$imv_not_get_live$add_listener(
      create_infection_update_listener(
         individuals$human,
         states$IMVNotGetLive
      )
   )

   events$imv_not_get_die$add_listener(
      create_infection_update_listener(
         individuals$human,
         states$IMVNotGetDie
      )
   )

   # IOx events
   events$iox_get_live$add_listener(
      create_infection_update_listener(
         individuals$human,
         states$IOxGetLive
      )
   )

   events$iox_get_die$add_listener(
      create_infection_update_listener(
         individuals$human,
         states$IOxGetDie
      )
   )

   events$iox_not_get_live$add_listener(
      create_infection_update_listener(
         individuals$human,
         states$IOxNotGetLive
      )
   )

   events$iox_not_get_die$add_listener(
      create_infection_update_listener(
         individuals$human,
         states$IOxNotGetDie
      )
   )

   # Recovery events
   events$recovery$add_listener(
      create_infection_update_listener(
         individuals$human,
         states$R
      )
   )

   # Stepdown events
   events$stepdown$add_listener(
      create_infection_update_listener(
         individuals$human,
         states$IRec
      )
   )

   # Death events
   events$death$add_listener(
      create_infection_update_listener(
         individuals$human,
         states$D
      )
   )

   # STATE SCHEDULES
   # These events trigger the scheduling for infection state changes
   # ----------------------------

   # Exposure events
   events$exposure$add_listener(
      create_exposure_update_listener(
         individuals$human,
         states,
         events,
         variables,
         parameters
      )
   )

   # Mild Infection events
   events$mild_infection$add_listener(
      create_progression_listener(
         event = events$recovery,
         duration = parameters$dur_IMild,
         func = r_exp
      )
   )

   # Case Infection events
   events$severe_infection$add_listener(
      create_progression_listener(
         event = events$hospitilisation,
         duration = parameters$dur_ICase
      )
   )

   # MV outcomes
   events$imv_get_live$add_listener(
      create_progression_listener(
         event = events$stepdown,
         duration = parameters$dur_get_mv_survive,
         shift = 1
      )
   )

   events$imv_get_die$add_listener(
      create_progression_listener(
         event = events$death,
         duration = parameters$dur_get_mv_die,
         shift = 1
      )
   )

   events$imv_not_get_live$add_listener(
      create_progression_listener(
         event = events$recovery,
         duration = parameters$dur_not_get_mv_survive,
         shift = 1
      )
   )

   events$imv_not_get_die$add_listener(
      create_progression_listener(
         event = events$death,
         duration = parameters$dur_not_get_mv_die,
         shift = 1
      )
   )

   # Ox outcomes
   events$iox_get_live$add_listener(
      create_progression_listener(
         event = events$recovery,
         duration = parameters$dur_get_ox_survive,
         shift = 1
      )
   )

   events$iox_get_die$add_listener(
      create_progression_listener(
         event = events$death,
         duration = parameters$dur_get_ox_die,
         shift = 1
      )
   )

   events$iox_not_get_live$add_listener(
      create_progression_listener(
         event = events$recovery,
         duration = parameters$dur_not_get_ox_survive,
         shift = 1
      )
   )

   events$iox_not_get_die$add_listener(
      create_progression_listener(
         event = events$death,
         duration = parameters$dur_not_get_ox_die,
         shift = 1
      )
   )

   # Stepdon
   events$stepdown$add_listener(
      create_progression_listener(
         event = events$recovery,
         duration = parameters$dur_rec
      )
   )

}
