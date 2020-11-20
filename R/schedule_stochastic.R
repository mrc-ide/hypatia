#' @title Infection process
#' @description
#' This is the process of infection for humans. It results in human future state
#' changes for infected humans and boosts in immunity.
#' @param individuals a list of individuals in the model
#' @param states a list of all of the model states
#' @param variables a list of all of the model variables
#' @param events a list of all of the model events
create_infection_process <- function(
  individuals,
  states,
  variables,
  events
) {
  function(api) {
    human <- individuals$human
    parameters <- api$get_parameters()
    timestep <- api$get_timestep()

    # Calculate EIR
    # age <- get_age(api$get_variable(human, variables$birth), api$get_timestep())
    # epsilon <- eir_from_api(api, individuals, states, variables, age)
    #
    # api$render("mean_EIR", mean(epsilon))

    # bitten_humans <- which(bernoulli_multi_p(length(epsilon), epsilon))
    # ib <- api$get_variable(human, variables$ib)
    # if (length(bitten_humans) > 0) {
    #   boost_immunity(
    #     api,
    #     human,
    #     variables$ib,
    #     bitten_humans,
    #     ib[bitten_humans],
    #     variables$last_boosted_ib,
    #     timestep,
    #     parameters$ub
    #   )
    # }


    api <- list(
      queue_state_update = mockery::mock(),
      get_parameters = mockery::mock(),
      get_state = mockery::mock()
    )

    pgamma <- 0.1 #probability

    # Work out this combination
    num_infected <- api$get_state(human, states$IMild) +
      api$get_state(human, states$ICase1) +
      api$get_state(human, states$ICase2)


    # # Calculate Infected
    #   infected_humans <- calculate_infections(
    #   api,
    #   human,
    #   states,
    #   variables,
    #   num_infected
    # )
    #
    # clinical_infections <- calculate_clinical_infections(
    #   api,
    #   human,
    #   variables,
    #   infected_humans
    # )
    #
    # treated <- calculate_treated(
    #   api,
    #   human,
    #   states,
    #   variables,
    #   clinical_infections
    # )

    schedule_infections(
      api,
      events,
      inf_process
      # clinical_infections,
      # treated,
      # infected_humans
    )
  }
}

# Need a proper infection process - use this for now
inf_process <- function(pgamma) {
  # individual::fixed_probability_state_change_process(
  #   "human", states$S$name, states$E$name, pgamma),
  # individual::fixed_probability_state_change_process(
  #   "human", states$E$name, states$IMild$name, pgamma),
  # individual::fixed_probability_state_change_process(
  #   "human", states$Imild$name, states$ICase1$name, pgamma)
  individual::fixed_probability_state_change_process(
    "human", "S", "E", pgamma)
  individual::fixed_probability_state_change_process(
    "human", "E", "IMild", pgamma)
  individual::fixed_probability_state_change_process(
    "human", "IMild", "ICase1", pgamma)
}

#' @title Schedule infections
#'
#' @param api api
#' @param events events
#' @param clinical_infections clinical infections
#' @param treated treated
#' @param infections infections
#'
#' @return
#' @export
schedule_infections <- function(
  api,
  events,
  inf_process
) {

  parameters <- api$get_parameters() #?????????
  scheduled_for_infection <- api$get_scheduled(events$infection)
  excluded <- c(scheduled_for_infection, treated)
  #
  # to_infect <- setdiff(clinical_infections, excluded)
  # all_new_infections <- setdiff(infections, excluded)
  # to_infect_asym <- setdiff(all_new_infections, clinical_infections)
  #
  # if(length(to_infect) > 0) {
  #   api$schedule(events$clinical_infection, to_infect, parameters$de)
  # }
  #
  # if(length(to_infect_asym) > 0) {
  #   api$schedule(events$asymptomatic_infection, to_infect_asym, parameters$de)
  # }
  #
  # if(length(all_new_infections) > 0) {
  #   api$schedule(events$infection, all_new_infections, parameters$de)
  # }
}

#' @title Clear schedules when individuals die
#'
#' @param died
#'
#' @return
clear_schedule <- function(died) {

  api$clear_schedule(events$infection, died)
  api$clear_schedule(events$asymptomatic_infection, died)

}
