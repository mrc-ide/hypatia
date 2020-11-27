#' @title Process to render number of individuals in specified states
#'
#' @param individual the individual of interest
#' @param states a list of state objects
#'
#' @export
#' @examples
#' state_count_renderer_process(individual, states)
state_count_renderer_process <- function(individual, states) {

  function(api) {
    for (state in states) {
      state_index = api$get_state(individual, state)
      name <- paste(individual$name, "_", state$name, "_count")
      name <- gsub(" ", "", name, fixed = TRUE)
      api$render(name, length(state_index))
    }
  }

}

