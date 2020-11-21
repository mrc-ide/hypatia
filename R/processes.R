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

   # Hospitalisation events
   events$hospitilisation$add_listener(
      create_hospitilisation_update_listener(
         individuals$human,
         states,
         variables,
         parameters,
         events
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

#' @title Simulate hospitilisation flow
#' @description
#' Allocated individuals to hospital resources upon arrival
#' @param api simulation api
#' @param to_move individuals to move
#' @param human humans
#' @param states a list of all of the model states
#' @param variables a list of all of the model variables
#' @param events a list of all of the model events
#' @param parameters of the model
hospitilisation_flow_process <- function(
   api,
   to_move,
   human,
   states,
   variables,
   parameters,
   events
) {

   disc_ages <- api$get_variable(human, variables$discrete_age, to_move)
   prob_severe <- parameters$prob_severe[as.integer(disc_ages)]
   allocated <- c()

   # 1. Who needs a MV
   mv <- bernoulli_multi_p(length(to_move), prob_severe)
   if(sum(mv) > 0) {

      # mv bed allocation
      mv_beds_occupied <- api$get_state(human, states$IMVGetDie, states$IMVGetLive)
      mv_beds_available <- parameters$ICU_beds - length(mv_beds_occupied)

      # who is getting an mv from available
      mv_to_move <- to_move[mv]
      mv_get <- sample.int(length(mv_to_move), min(length(mv_to_move), mv_beds_available))
      mv_get <- mv_to_move[mv_get]
      mv_not_get <- setdiff(mv_to_move, mv_get)

      # schedule for those getting mv
      if (length(mv_get) > 0) {

         disc_ages <- api$get_variable(human, variables$discrete_age, mv_get)
         prob_die <- parameters$prob_severe_death_treatment[as.integer(disc_ages)]
         die <- bernoulli_multi_p(length(mv_get), prob_die)

         if(sum(die) > 0) {
            api$schedule(
               event = events$imv_get_die,
               target = mv_get[die],
               delay = 0
            )
            allocated <- c(allocated, mv_get[die])
         }

         if(sum(!die) > 0) {
            api$schedule(
               event = events$imv_get_live,
               target = mv_get[!die],
               delay = 0
            )
            allocated <- c(allocated, mv_get[!die])
         }

      }

      # schedule for those not getting mv
      if (length(mv_not_get) > 0) {

         disc_ages <- api$get_variable(human, variables$discrete_age, mv_not_get)
         prob_die <- parameters$prob_severe_death_no_treatment[as.integer(disc_ages)]
         die <- bernoulli_multi_p(length(mv_not_get), prob_die)

         if(sum(die) > 0) {
            api$schedule(
               event = events$imv_not_get_die,
               target = mv_not_get[die],
               delay = 0
            )
            allocated <- c(allocated, mv_not_get[die])
         }

         if(sum(!die) > 0) {
            api$schedule(
               event = events$imv_not_get_live,
               target = mv_not_get[!die],
               delay = 0
            )
            allocated <- c(allocated, mv_not_get[!die])
         }

      }
   }

   # 2. Who needs Ox
   if (sum(!mv) > 0) {

      ox_to_move <- to_move[!mv]

      # ox bed allocation
      ox_beds_occupied <- api$get_state(human, states$IOxGetLive, states$IOxGetDie, states$IRec)
      ox_beds_available <- parameters$hosp_beds - length(ox_beds_occupied)

      # who is getting an ox from available
      ox_get <- sample.int(length(ox_to_move), min(length(ox_to_move), ox_beds_available))
      ox_get <- ox_to_move[ox_get]
      ox_not_get <- setdiff(ox_to_move, ox_get)

      # schedule for those getting ox
      if (length(ox_get) > 0) {

         disc_ages <- api$get_variable(human, variables$discrete_age, ox_get)
         prob_die <- parameters$prob_non_severe_death_treatment[as.integer(disc_ages)]
         die <- bernoulli_multi_p(length(ox_get), prob_die)

         if(sum(die) > 0) {
            api$schedule(
               event = events$iox_get_die,
               target = ox_get[die],
               delay = 0
            )
            allocated <- c(allocated, ox_get[die])
         }

         if(sum(!die) > 0) {
            api$schedule(
               event = events$iox_get_live,
               target = ox_get[!die],
               delay = 0
            )
            allocated <- c(allocated, ox_get[!die])
         }

      }

      # schedule for those not getting ox
      if (length(ox_not_get) > 0) {

         disc_ages <- api$get_variable(human, variables$discrete_age, ox_not_get)
         prob_die <- parameters$prob_non_severe_death_no_treatment[as.integer(disc_ages)]
         die <- bernoulli_multi_p(length(ox_not_get), prob_die)

         if(sum(die) > 0) {
            api$schedule(
               event = events$iox_not_get_die,
               target = ox_not_get[die],
               delay = 0
            )
            allocated <- c(allocated, ox_not_get[die])
         }

         if(sum(!die) > 0) {
            api$schedule(
               event = events$iox_not_get_live,
               target = ox_not_get[!die],
               delay = 0
            )
            allocated <- c(allocated, ox_not_get[!die])
         }

      }

   }

   # Useful check here to make sure everyone has been allocated
   if((length(allocated) != length(to_move)) || !all(allocated %in% to_move)) {
      stop("hospital not fully allocated")
   }

}
