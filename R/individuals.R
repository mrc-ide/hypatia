#' @title Define the human model
#' @description Declares the human individual and assigns the
#' relevant states and variables
#'
#' @param states available states to assign
#' @param variables available variables to assign
#' @param events available events to assign
#'
#' @return human - human
create_human <- function(
  states,
  variables,
  events) {

  human <- individual::Individual$new(
    "human",
    states = states,
    variables = variables,
    events = events
  )

  human
}

#' @title Define the human model returning more than 1 human
#'
#' @param states available states to assign
#' @param variables available variables to assign
#' @param events available events to assign
#'
#' @return list of humans
create_individuals <- function(states,
                               variables,
                               events) {
  human <- individual::Individual$new(
    "human",
    states = list(
      states$S,
      states$E,
      states$IMild,
      states$ICase,
      states$IOxGetLive,
      states$IOxGetDie,
      states$IOxNotGetLive,
      states$IOxNotGetDie,
      states$IMVGetLive,
      states$IMVGetDie,
      states$IMVNotGetLive,
      states$IMVNotGetDie,
      states$IRec,
      states$R,
      states$D
    ),

    variables = variables,

    events = events
  )

  list(human = human)
}
