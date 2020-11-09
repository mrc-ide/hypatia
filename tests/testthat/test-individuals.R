# test_that("test create_states", {
#   Test is not working. # init is not correct and needs fixing

#   pop = squire::get_population("Afghanistan", simple_SEIR = FALSE)
#
#
#   init <- data.frame(S = 'S', E1 = 'E1')
#   set.seed(123)
#   psq <- squire::run_explicit_SEEIR_model(population = pop$n,
#                                          dt = 1,
#                                          R0 = 2,
#                                          time_period = 1000,
#                                          init = init,
#                                          #seeding_cases = 5,
#                                          replicates = 10,
#                                          contact_matrix_set=squire::contact_matrices[[1]])
#   print(psq)
#
#   states <- hypatia::Create_states(psq$pars)
#   expect_equal(states$S, individual::State$new("S", psq$pars$S_0))
#
# })
