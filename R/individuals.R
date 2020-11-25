#' @title Define the human model
#' @description
#' Declares the human individual and assigns the
#' relevant states and variables 
#'
#' @param states available states to assign
#' @param variables available variables to assign
#' @param events available events to assign
#' @param parameters model parameters
create_human <- function(
  states,
  variables,
  events,
  parameters) {
  individual::Individual$new(
    "human",
    states = states,
    variables = variables,
    events = events
  )
}
