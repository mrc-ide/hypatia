#' @title Check state update is valid
#'
#' @param api api
#' @param i human etc
#' @param state S, I, R, etc
#' @param ix index
#' @param population_size population size
validated_state_update <- function(api, i, state, ix, population_size) {
  
  if (any(ix > population_size)) {
    stop(sprintf("Your ix %d for %s:%s is greater than the population size\n",
                 ix, i$name, state$name))
  }
  if (any(ix <= 0)) {
    stop(sprintf("Your ix %d for %s:%s is less than or equal to 0 \n",
                 ix, i$name, state$name))
  }
  if (any(is.na(ix))) {
    stop(sprintf("Your ix %d for %s:%s is not a number \n",
                 ix, i$name, state$name))
  }
  if (any(!is_integer_like(ix))) {
    stop(sprintf("Your ix %d for %s:%s is not an integer\n",
                 ix, i$name, state$name))
  }
  
  api$queue_state_update(i, state, ix)
}
