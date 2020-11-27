#' @title Process to render number of individuals in specified states
#'
#' @param individualname name of the individual of interest
#' @param statenames a list of state object's names
#'
#' @return rendered data
#' @export
#' @examples
#' state_count_renderer_process(individual, states)
state_count_renderer_process <- function(individualname, statenames) {

  individual::state_count_renderer_process(
    individualname,
    statenames
  )

}


