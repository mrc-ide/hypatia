#' @title Create processes
#' @description Create processes for individual::simulate function
#' - note that first 2 will be replaced with better functions
#'
#' @param indivs human, etc
#' @param states states
#' @param pstates probability of states
#' @param psq parameters list
#' @param population total population for all age groups
#' @param beta beta
#' @param E_I E_I
#' @param numberof_days number of days
#'
#' @return processes
create_processes <- function(indivs, states, pstates, psq, population, beta,
                             E_I, numberof_days) {
  processes <- list(
   individual::fixed_probability_state_change_process(
     "human", states$S$name, states$E$name, pstates$pgamma_E),
   individual::fixed_probability_state_change_process(
     "human", states$E$name, states$IMild$name, pstates$pgamma_E),
   individual::fixed_probability_state_change_process(
     "human", states$IMild$name, states$R$name, pstates$pgamma_IMild),
   individual::fixed_probability_state_change_process(
     "human", states$ICase1$name, states$ICase2$name,
     pstates$pgamma_ICase),
   individual::fixed_probability_state_change_process(
     "human", states$IOxGetLive1$name, states$IOxGetLive2$name,
     pstates$pgamma_get_ox_survive),
   individual::fixed_probability_state_change_process(
     "human", states$IOxGetLive2$name, states$R$name,
     pstates$pgamma_get_ox_survive),
   individual::fixed_probability_state_change_process(
     "human", states$IOxNotGetLive1$name, states$IOxNotGetLive2$name,
     pstates$pgamma_not_get_ox_survive),
   individual::fixed_probability_state_change_process(
     "human", states$IOxNotGetLive2$name, states$R$name,
     pstates$pgamma_not_get_ox_survive),
   individual::fixed_probability_state_change_process(
     "human", states$IOxGetDie1$name, states$IOxGetDie2$name,
     pstates$pgamma_get_ox_die),
   individual::fixed_probability_state_change_process(
     "human", states$IOxGetDie2$name, states$D$name,
     pstates$pgamma_get_ox_die),
   individual::fixed_probability_state_change_process(
     "human", states$IOxNotGetDie1$name, states$IOxNotGetDie2$name,
     pstates$pgamma_not_get_ox_die),
   individual::fixed_probability_state_change_process(
     "human", states$IOxNotGetDie2$name, states$D$name,
     pstates$pgamma_not_get_ox_die),
   individual::fixed_probability_state_change_process(
     "human", states$IMVGetLive1$name, states$IMVGetLive2$name,
     pstates$pgamma_get_mv_survive),
   individual::fixed_probability_state_change_process(
     "human", states$IMVGetLive2$name, states$IRec1$name,
     pstates$pgamma_get_mv_survive),
   individual::fixed_probability_state_change_process(
     "human", states$IMVNotGetLive1$name, states$IMVNotGetLive2$name,
     pstates$pgamma_not_get_mv_survive),
   individual::fixed_probability_state_change_process(
     "human", states$IMVNotGetLive1$name, states$IMVNotGetLive2$name,
     pstates$pgamma_not_get_mv_survive),
   individual::fixed_probability_state_change_process(
     "human", states$IMVGetDie1$name, states$IMVGetDie2$name,
     pstates$pgamma_get_mv_die),
   individual::fixed_probability_state_change_process(
     "human", states$IMVGetDie2$name, states$D$name,
     pstates$pgamma_get_mv_die),
   individual::fixed_probability_state_change_process(
     "human", states$IMVNotGetDie1$name, states$IMVNotGetDie2$name,
     pstates$pgamma_not_get_mv_die),
   individual::fixed_probability_state_change_process(
     "human", states$IMVNotGetDie2$name, states$D$name,
     pstates$pgamma_not_get_mv_die),
   individual::fixed_probability_state_change_process(
     "human", states$IRec1$name, states$IRec2$name, pstates$pgamma_rec),
   individual::fixed_probability_state_change_process(
     "human", states$IRec2$name, states$R$name, pstates$pgamma_rec),
   render_all_state_sizes(
      states$S, states$E, states$IMild, states$ICase1, states$ICase2,
      states$IOxGetLive1, states$IOxGetLive2, states$IOxNotGetLive1,
      states$IOxNotGetLive2, states$IOxGetDie1, states$IOxGetDie2,
      states$IOxNotGetDie1, states$IOxNotGetDie2, states$IMVGetLive1,
      states$IMVGetLive2, states$IMVNotGetLive1, states$MVNotGetLive2,
      states$IMVGetDie1, states$IMVGetDie2, states$IMVNotGetDie1,
      states$IMVNotGetDie2, states$IRec1, states$IRec2, states$R,
      states$D, indivs$human)
   )

  processes
}
