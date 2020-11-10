test_that("SQUIRE SEEIR model runs", {

   pop <- squire::get_population("Afghanistan", simple_SEIR = FALSE)

  set.seed(123)
  r1 <- squire::run_explicit_SEEIR_model(population = pop$n,
                             dt = 1,
                             R0 = 2,
                             time_period = 1000,
                             replicates = 10,
                             contact_matrix_set = squire::contact_matrices[[1]])
  expect_type(r1$output, "double")

  o1 <- squire::format_output(r1)
  expect_true(
    sum(dplyr::filter(o1, t == min(t), replicate == 1)$y) == sum(pop$n))

  uc <- unique(o1$compartment)

  for (i in seq_along(uc)) {
    expect_equal(sum(o1$compartment == uc[i]), 1000 * 10)
  }

  set.seed(123)
  # Multiple R0s
  r2 <- squire::run_explicit_SEEIR_model(population = pop$n,
                             dt = 1,
                             R0 = c(2, 2),
                             tt_R0 = c(0, 10),
                             time_period = 1000,
                             replicates = 10,
                             contact_matrix_set = squire::contact_matrices[[1]])
  expect_identical(r1$output, r2$output)
})
