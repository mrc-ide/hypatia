test_that("test that renderer works", {

  R0 <- 2
  timestep <- 100
  time_period <- 1000
  tt_contact_matrix <- 0
  newpopulation <- 100000
  numberof_days <- 5
  contact_matrix_set <- squire::contact_matrices[[1]]
  pop <- get_population("Afghanistan")

  psq <- get_parameters(
    pop,
    R0 = R0,
    time_period = time_period,
    tt_contact_matrix = tt_contact_matrix,
    contact_matrix_set = contact_matrix_set
  )


  S <- individual::State$new("S", 10)
  E <- individual::State$new("E", 100)
  human <- individual::Individual$new("human", list(S, E))

  render_states <- individual::state_count_renderer_process(
    human$name,
    c(S$name, E$name)
  )


  renderer <- state_count_renderer_process(
    human,
    c(S, E))

  output <- run_simulation(
    pop,
    psq,
    renderer
  )

  rendered <- data.frame(output)
  expected <- data.frame(
    timestep = c(1, 2),
    human_S_count = c(10, 8),
    human_E_count = c(100, 102),

  )
  expect_mapequal(rendered, expected)

})
