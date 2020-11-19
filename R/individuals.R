#' @title Define model individuals
#' @description
#' Create_individuals declares the individuals to simulate. It assigns the
#' relevant states and variables to each individual.
#'
#' @param states available states to assign
#' @param states list of states
create_individuals <- function(
  states
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
      states$D)
  )

  list(human = human)
}

create_events <- () {


}
