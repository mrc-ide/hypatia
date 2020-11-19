#' @title Create processes
#' @description Create processes for individual::simulate function
#' - note that first 2 will be replaced with better functions
#'
#' @param indivs human, etc
#' @param states states
#' @param pstates probability of states
#' @param psq parameters list
#' @param population total population for all age groups
#' @param beta beta
#' @param E_I E_I
#' @param numberof_days number of days
#'
#' @return processes
create_processes <- function(indivs, states, pstates, psq, population, beta,
                             E_I, numberof_days) {
  processes <- list(
   individual::fixed_probability_state_change_process(
     "human", states$S$name, states$E$name, pstates$pgamma_E),
   individual::fixed_probability_state_change_process(
     "human", states$E$name, states$IMild$name, pstates$pgamma_E),
   individual::fixed_probability_state_change_process(
     "human", states$IMild$name, states$R$name, pstates$pgamma_IMild),
   individual::fixed_probability_state_change_process(
     "human", states$ICase1$name, states$ICase2$name,
     pstates$pgamma_ICase),
   individual::fixed_probability_state_change_process(
     "human", states$IOxGetLive1$name, states$IOxGetLive2$name,
     pstates$pgamma_get_ox_survive),
   individual::fixed_probability_state_change_process(
     "human", states$IOxGetLive2$name, states$R$name,
     pstates$pgamma_get_ox_survive),
   individual::fixed_probability_state_change_process(
     "human", states$IOxNotGetLive1$name, states$IOxNotGetLive2$name,
     pstates$pgamma_not_get_ox_survive),
   individual::fixed_probability_state_change_process(
     "human", states$IOxNotGetLive2$name, states$R$name,
     pstates$pgamma_not_get_ox_survive),
   individual::fixed_probability_state_change_process(
     "human", states$IOxGetDie1$name, states$IOxGetDie2$name,
     pstates$pgamma_get_ox_die),
   individual::fixed_probability_state_change_process(
     "human", states$IOxGetDie2$name, states$D$name,
     pstates$pgamma_get_ox_die),
   individual::fixed_probability_state_change_process(
     "human", states$IOxNotGetDie1$name, states$IOxNotGetDie2$name,
     pstates$pgamma_not_get_ox_die),
   individual::fixed_probability_state_change_process(
     "human", states$IOxNotGetDie2$name, states$D$name,
     pstates$pgamma_not_get_ox_die),
   individual::fixed_probability_state_change_process(
     "human", states$IMVGetLive1$name, states$IMVGetLive2$name,
     pstates$pgamma_get_mv_survive),
   individual::fixed_probability_state_change_process(
     "human", states$IMVGetLive2$name, states$IRec1$name,
     pstates$pgamma_get_mv_survive),
   individual::fixed_probability_state_change_process(
     "human", states$IMVNotGetLive1$name, states$IMVNotGetLive2$name,
     pstates$pgamma_not_get_mv_survive),
   individual::fixed_probability_state_change_process(
     "human", states$IMVNotGetLive1$name, states$IMVNotGetLive2$name,
     pstates$pgamma_not_get_mv_survive),
   individual::fixed_probability_state_change_process(
     "human", states$IMVGetDie1$name, states$IMVGetDie2$name,
     pstates$pgamma_get_mv_die),
   individual::fixed_probability_state_change_process(
     "human", states$IMVGetDie2$name, states$D$name,
     pstates$pgamma_get_mv_die),
   individual::fixed_probability_state_change_process(
     "human", states$IMVNotGetDie1$name, states$IMVNotGetDie2$name,
     pstates$pgamma_not_get_mv_die),
   individual::fixed_probability_state_change_process(
     "human", states$IMVNotGetDie2$name, states$D$name,
     pstates$pgamma_not_get_mv_die),
   individual::fixed_probability_state_change_process(
     "human", states$IRec1$name, states$IRec2$name, pstates$pgamma_rec),
   individual::fixed_probability_state_change_process(
     "human", states$IRec2$name, states$R$name, pstates$pgamma_rec),
   render_all_state_sizes(
      states$S, states$E, states$IMild, states$ICase1, states$ICase2,
      states$IOxGetLive1, states$IOxGetLive2, states$IOxNotGetLive1,
      states$IOxNotGetLive2, states$IOxGetDie1, states$IOxGetDie2,
      states$IOxNotGetDie1, states$IOxNotGetDie2, states$IMVGetLive1,
      states$IMVGetLive2, states$IMVNotGetLive1, states$MVNotGetLive2,
      states$IMVGetDie1, states$IMVGetDie2, states$IMVNotGetDie1,
      states$IMVNotGetDie2, states$IRec1, states$IRec2, states$R,
      states$D, indivs$human)
   )

  processes
}

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
