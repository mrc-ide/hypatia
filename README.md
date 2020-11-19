
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hypatia

<!-- badges: start -->

[![R build
status](https://github.com/mrc-ide/hypatia/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/hypatia/actions)
[![CodeFactor](https://www.codefactor.io/repository/github/mrc-ide/hypatia/badge)](https://www.codefactor.io/repository/github/mrc-ide/hypatia)
[![codecov.io](https://codecov.io/github/mrc-ide/hypatia/coverage.svg?branch=main)](https://codecov.io/github/mrc-ide/hypatia?branch=main)
<!-- badges: end -->

The goal of hypatia is to enable SQUIRE to be run on an individual basis
rather than aggregate

## Installation

``` r
options(warn = - 1) 

install_github('mrc-ide/individual')
library(individual)
```

## Example 1

This is an example showing how states are checked

``` r
  pop <- squire::get_population("Afghanistan", simple_SEIR = FALSE)

  dt <- 1
  R0 <- 2
  time_period <- 1000
  tt_contact_matrix <- 0

  psq <- squire::parameters_explicit_SEEIR(
    population = pop$n,
    dt = dt,
    R0 = R0,
    tt_contact_matrix = tt_contact_matrix,
    time_period = time_period,
    contact_matrix_set = squire::contact_matrices[[1]])

  beta <- squire::beta_est_explicit(psq$dur_IMild, psq$dur_ICase,
                                    psq$prob_hosp,
                                    psq$mix_mat_set[1, , ], R0 = R0)

  states <- create_states(psq)

  pe <- sum(psq$E1_0) + sum(psq$E2_0)

  S = individual::State$new("S", sum(psq$S_0))
  E =  individual::State$new("E", pe)
  IMild = individual::State$new("IMild", sum(psq$IMild_0))
  ICase1 = individual::State$new("ICase1", sum(psq$ICase1_0))
  ICase2 = individual::State$new("ICase2", sum(psq$ICase2_0))
  IOxGetLive1 = individual::State$new("IOxGetLive1", sum(psq$IOxGetLive1_0))
  IOxGetLive2 = individual::State$new("IOxGetLive2", sum(psq$IOxGetLive2_0))
  IOxGetDie1 = individual::State$new("IOxGetDie1", sum(psq$IOxGetDie1_0))
  IOxGetDie2 = individual::State$new("IOxGetDie2", sum(psq$IOxGetDie2_0))
  IOxNotGetLive1 = individual::State$new("IOxNotGetLive1",
                                         sum(psq$IOxNotGetLive1_0))
  IOxNotGetLive2 = individual::State$new("IOxNotGetLive2",
                                         sum(psq$IOxNotGetLive2_0))
  IOxNotGetDie1 = individual::State$new("IOxNotGetDie1",
                                        sum(psq$IOxNotGetDie1_0))
  IOxNotGetDie2 = individual::State$new("IOxNotGetDie2",
                                        sum(psq$IOxNotGetDie2_0))
  IMVGetLive1 = individual::State$new("IMVGetLive1", sum(psq$IMVGetLive1_0))
  IMVGetLive2 = individual::State$new("IMVGetLive2", sum(psq$IMVGetLive2_0))
  IMVGetDie1 = individual::State$new("IMVGetDie1", sum(psq$IMVGetDie1_0))
  IMVGetDie2 = individual::State$new("IMVGetDie2", sum(psq$IMVGetDie2_0))
  IMVNotGetLive1 = individual::State$new("IMVNotGetLive1",
                                         sum(psq$IMVNotGetLive1_0))
  IMVNotGetLive2 = individual::State$new("IMVNotGetLive2",
                                         sum(psq$IMVNotGetLive2_0))
  IMVNotGetDie1 = individual::State$new("IMVNotGetDie1",
                                        sum(psq$IMVNotGetDie1_0))
  IMVNotGetDie2 = individual::State$new("IMVNotGetDie2",
                                        sum(psq$IMVNotGetDie2_0))
  IRec1 = individual::State$new("IRec1", sum(psq$IRec1_0))
  IRec2 = individual::State$new("IRec2", sum(psq$IRec2_0))
  R = individual::State$new("R", sum(psq$R_0))
  D = individual::State$new("D", sum(psq$D_0))

  expect_equal(states$S, S)
  expect_equal(states$E, E)
  expect_equal(states$IMild, IMild)
  expect_equal(states$ICase1, ICase1)
  expect_equal(states$ICase2, ICase2)
  expect_equal(states$IOxGetLive1, IOxGetLive1)
  expect_equal(states$IOxGetLive2, IOxGetLive2)
  expect_equal(states$IOxGetDie1, IOxGetDie1)
  expect_equal(states$IOxGetDie2, IOxGetDie2)
  expect_equal(states$IOxNotGetLive1, IOxNotGetLive1)
  expect_equal(states$IOxNotGetLive2, IOxNotGetLive2)
  expect_equal(states$IOxNotGetDie1, IOxNotGetDie1)
  expect_equal(states$IOxNotGetDie2, IOxNotGetDie2)
  expect_equal(states$IMVGetLive1, IMVGetLive1)
  expect_equal(states$IMVGetLive2, IMVGetLive2)
  expect_equal(states$IMVGetDie1, IMVGetDie1)
  expect_equal(states$IMVGetDie2, IMVGetDie2)
  expect_equal(states$IMVNotGetLive1, IMVNotGetLive1)
  expect_equal(states$IMVNotGetLive2, IMVNotGetLive2)
  expect_equal(states$IMVNotGetDie1, IMVNotGetDie1)
  expect_equal(states$IMVNotGetDie2, IMVNotGetDie2)
  expect_equal(states$IRec1, IRec1)
  expect_equal(states$IRec2, IRec2)
  expect_equal(states$R, R)
  expect_equal(states$D, D)
```

## Example 2

This is an example showing how to get SQUIRe parameters, psq


## License

MIT © Imperial College of Science, Technology and Medicine
