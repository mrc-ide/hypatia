#' @title Define the human model
#' @description Declares the human individual and assigns the
#' relevant states and variables
#'
#' @param states available states to assign
#' @param variables available variables to assign
#' @param events available events to assign
#' @param parameters available parameters to assign
#'
#' @return human - human
create_human <- function(
  states,
  variables,
  events,
  parameters) {

  human <- individual::Individual$new(
    "human",
    states = states,
    variables = variables,
    events = events
  )

  list(human = human)
}

