test_that("test infection process", {

  # Create our population
  pop <- squire::get_population(iso3c = "ATG")

  # Scale it for speed
  pop$n <- round(pop$n / 100)

  # Create our equivalent parameters
  psq <- squire::parameters_explicit_SEEIR(
    population = pop$n,
    country = "Antigua and Barbuda",
    contact_matrix_set = squire::get_mixing_matrix(iso3c = "ATG"),
  )

  # create set up for model
  states <- create_states(psq)
  events <- create_events()
  parameters <- remove_non_numerics(psq)
  states <- create_states(psq)
  variables <- create_variables(pop = pop)
  individuals <- create_human(states, variables, events)
  create_event_based_processes(individuals, states, variables, events, parameters)

  # mock api
  # TODO: Giovanni: Could you explain how you do/recommend
  api <- list(
    queue_state_update = mockery::mock(),
    get_parameters = mockery::mock(),
    get_state = mockery::mock()
  )

  inf_process <-
    infection_process(
      individuals = individuals,
      states = states,
      variables = variables,
      events = events
    )

  inf_process(api)

})
