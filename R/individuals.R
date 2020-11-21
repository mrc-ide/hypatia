#' @title Define model individuals
#' @description
#' Create_individuals declares the individuals to simulate. It assigns the
#' relevant states and variables to each individual.
#'
#' @param states available states to assign
#' @param variables list of variables
#' @param events list of events
#' @param parameters list of parameters
create_individuals <- function(
  states, variables, events, parameters
) {
  human <- individual::Individual$new(
    "human",
    states = list(
      states$S,
      states$E,
      states$IMild,
      states$ICase1,
      states$ICase2,
      states$IOxGetLive1,
      states$IOxGetLive2,
      states$IOxGetDie1,
      states$IOxGetDie2,
      states$IOxNotGetLive1,
      states$IOxNotGetLive2,
      states$IOxNotGetDie1,
      states$IOxNotGetDie2,
      states$IMVGetLive1,
      states$IMVGetLive2,
      states$IMVGetDie1,
      states$IMVGetDie2,
      states$IMVNotGetLive1,
      states$IMVNotGetLive2,
      states$IMVNotGetDie1,
      states$IMVNotGetDie2,
      states$IRec1,
      states$IRec2,
      states$R,
      states$D),
    variables = variables,
    events = events
  )

  list(human = human)
}


#' @title Define model variables
#' @description
#' create_variables creates human variables

#'
#' @param psq, model parameters created by `get_parameters`
create_variables <- function(psq) {

  immunity <- individual::Variable$new('immunity',  rep(0, sum(psq$population)))
  age  <- individual::Variable$new('age', rep(0, sum(psq$population)))
  location <- individual::Variable$new('location', rep(0, sum(psq$population)))

  variables <- list(
    immunity,
    age,
    location
  )

  variables
}
