test_that("test that renderer works for 1 state", {

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
  human <- individual::Individual$new("human", list(S))

  renderer <- state_count_renderer_process(
    human$name,
    c(S$name)
  )

  output <- run_simulation(
    pop,
    psq,
    renderer
  )

  rendered <- data.frame(output)
  expect_equal(length(rendered$timestep), 1000)
  expect_equal(length(rendered$human_S_count), 1000)
  expect_equal(sum(rendered$human_S_count), 38928321000)


})


test_that("test that renderer works for more than 1 state", {

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
  IMild <- individual::State$new("IMild", 0)
  human <- individual::Individual$new("human", list(S, E, IMild))

  renderer <- state_count_renderer_process(
    human$name,
    c(S$name, E$name, IMild$name)
  )

  output <- run_simulation(
    pop,
    psq,
    renderer
  )

  rendered <- data.frame(output)

  expect_equal(length(rendered$timestep), 1000)
  expect_equal(length(rendered$human_S_count), 1000)
  expect_equal(length(rendered$human_E_count), 1000)
  expect_equal(length(rendered$human_IMild_count), 1000)
  expect_equal(sum(rendered$human_S_count), 38928321000)
  expect_equal(sum(rendered$human_E_count), 20000)
  expect_equal(sum(rendered$human_IMild_count), 0)

})
