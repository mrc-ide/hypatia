#' @title Define the human model
#' @description
#' Declares the human individual and assigns the
#' relevant states and variables 
#'
#' @param states available states to assign
create_human <- function(states) {
  individual::Individual$new("human", states = states)
}
