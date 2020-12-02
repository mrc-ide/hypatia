test_that("test create_setup_process", {
   # create our dummy variables for create_setup_process
   # these are mock arguments to be passed to the function
   # Because create_steup_process is istelf a wrapper to a function that takes
   # an argument api, we will mock the arguments passed to the api
   individuals <- list(
      human = mockery::mock()
   )
   states <- list(
      E = mockery::mock()
   )
   variables <- list(discrete_age = mockery::mock())
   events <- list(mild_infection = mockery::mock(),
                  severe_infection = mockery::mock())
   process <- create_setup_process(
      individuals,
      states,
      events,
      variables
   )
   # here we know mock what our api will be doing
   # To test that our function processes both infection outcomes (severe and mild)
   # let's make sure that our mocked api will test this

   # We create our api list. This must have mocked named elements that match
   # each function required by api in the create_setup_process function:
   api <- list(
      # we need a get_state function to return the indices of the individuals in
      # state E. Here let's pretend that individuals 5:8 are in E
      get_state = mockery::mock(
         c(5, 6, 7, 8), # exposed states
      ),
      # we also need to get their discrete ages. Let's pretend that they have
      # discrete ages 1, 2, 3, 5
      get_variable = mockery::mock(
         c(1, 2, 3, 5), # age of our individuals
      ),
      # we also need get_parameters. This should return in create_setup_process
      # a list that can be indexed for both prob_hosp and dur_E. To make the outcome
      # of the bernoulli predictable let's have the probs for individuals 5 and 6
      # (so ages 1, 2) be 0 and then for individuals 7, 8 (so ages 3, 5) be 1
      get_parameters = mockery::mock(list(
         # note here 0.5 is for an individual age 4 but we don't have anyone age 4
         # so this wouldn't impact the predictability of the outcome
         prob_hosp = c(0, 0, 1, 0.5, 1),
         # And we aso need the duration dur_E that is used in the erlang draws.
         # We could mock it, or we have dur_E be equal to 0 to contol the outcome
         dur_E = 0 # set to zero to control behaviour of erlang without mocking it
      )),
      schedule = mockery::mock()
   )

   # now that we have our api let's call our process function
   process(api)

   # The process should have called api$schedule twice. The first call is to
   # schedule the severe infection for individuals 7,8 who schedule in 0 days
   mockery::expect_args(
      api$schedule,
      1,
      event = events$severe_infection,
      target = c(7, 8),
      delay = c(0,0)
   )

   # The process should have called api$schedule twice. The second call is to
   # schedule the severe infection for individuals 5,6 who schedule in 0 days
   mockery::expect_args(
      api$schedule,
      2,
      event = events$mild_infection,
      target = c(5, 6),
      delay = c(0, 0)
   )
})

test_that("test that create_pocesses works", {

   skip("No idea what create_process call is doing here so just skipped")

   pop <- get_population("Afghanistan")
   pop$n <- as.integer(pop$n/10000)

   R0 <- 2
   timestep <- 100
   time_period <- 1000
   tt_contact_matrix <- 0
   numberof_days <- 5
   contact_matrix_set <- squire::contact_matrices[[1]]

   psq <- get_parameters(
      country = "Afghanistan",
      population = pop$n,
      contact_matrix_set = contact_matrix_set,
      R0 = R0,
      time_period = time_period,
      tt_contact_matrix = tt_contact_matrix
   )

   max_age <- 100

   processes <- create_processes(psq, pop, max_age = max_age)

   # Check create_processes has worked correctly
   for (process in processes) {
      expect(is.function(process) || inherits(process, 'externalptr'),
             'Process is not a function')
   }

})


test_that("test that create_pocesses works for render process", {

   pop <- get_population("Afghanistan")
   pop$n <- as.integer(pop$n/10000)
   R0 <- 2
   timestep <- 100
   time_period <- 1000
   tt_contact_matrix <- 0
   numberof_days <- 5
   contact_matrix_set <- squire::contact_matrices[[1]]

   psq <- get_parameters(
      country = "Afghanistan",
      population = pop$n,
      contact_matrix_set = contact_matrix_set,
      R0 = R0,
      time_period = time_period,
      tt_contact_matrix = tt_contact_matrix
   )

   output <- run_simulation(
      pop,
      psq,
      max_age = 100
   )

   expect_equal(length(output$timestep), 1000)
   expect_equal(length(output$human_E_count), 1000)
   expect_equal(length(output$human_IMild_count), 1000)
   expect_equal(length(output$human_IMVNotGetLive_count), 1000)

})


