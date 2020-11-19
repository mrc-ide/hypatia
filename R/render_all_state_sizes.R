#' Render sizes for SQUIRE states - note IMild is missing for now
#'
#' @param S Susceptible
#' @param E Latent infection compartments
#' @param IMild rest of the infections, which we consider to be mild and not
#'  require hospitalisation
#' @param ICase1 First of the compartments for infections that will require
#' hospitalisation
#' @param ICase2 Second of the compartments for infections that will require
#' hospitalisation
#' @param IOxGetLive1 First of the compartments for infections that will require
#' oxygen, get it, and who survive
#' @param IOxGetLive2 Second of the compartments for infections that will
#' require oxygen, get it, and who survive
#' @param IOxNotGetLive1 First of the compartments for infections that will
#' require oxygen, do NOT get it, and live
#' @param IOxNotGetLive2 Second of the compartments for infections that will
#'  require oxygen, do NOT get it, and live
#' @param IOxGetDie1 First of the compartments for infections that will
#' require oxygen, get it, and die
#' @param IOxGetDie2 Second of the compartments for infections that will
#' require oxygen, get it, and die
#' @param IOxNotGetDie1 First of the compartments for infections that will
#' require oxygen, do NOT get it, and die
#' @param IOxNotGetDie2 Second of the compartments for infections that will
#'  require oxygen, do NOT get it, and die
#' @param IMVGetLive1 First of the compartments for infections that will
#' require mechanical ventilation, get it, and who survive
#' @param IMVGetLive2 Second of the compartments for infections that will
#' require mechanical ventilation, get it, and who survive
#' @param IMVNotGetLive1 First of the compartments for infections that will
#'  require mechanical ventilation, do NOT get it, and survive
#' @param IMVNotGetLive2 Second of the compartments for infections that will
#'  require mechanical ventilation, do NOT get it, and survive
#' @param IMVGetDie1 First of the compartments for infections that will
#'  require mechanical ventilation, get it, and die
#' @param IMVGetDie2 Second of the compartments for infections that will
#'  require mechanical ventilation, get it, and die
#' @param IMVNotGetDie1 First of the compartments for infections that will
#'  require mechanical ventilation, do NOT get it, and die
#' @param IMVNotGetDie2 Second of the compartments for infections that will
#'  require mechanical ventilation, do NOT get it, and die
#' @param IRec1 First of the compartments for those recovering from ICU
#' @param IRec2 Second of the compartments for those recovering from ICU
#' @param R Recovered
#' @param D Dead
#' @param human human
render_all_state_sizes <- function(S, E, IMild, ICase1, ICase2,
                                IOxGetLive1, IOxGetLive2,
                                IOxNotGetLive1, IOxNotGetLive2, IOxGetDie1,
                                IOxGetDie2, IOxNotGetDie1, IOxNotGetDie2,
                                IMVGetLive1, IMVGetLive2, IMVNotGetLive1,
                                IMVNotGetLive2, IMVGetDie1, IMVGetDie2,
                                IMVNotGetDie1, IMVNotGetDie2, IRec1, IRec2, R,
                                D, human) {
  function(api) {
    api$render("S", length(api$get_state(human, S)))
    api$render("E", length(api$get_state(human, E)))
    api$render("IMild", length(api$get_state(human, IMild)))
    api$render("ICase1", length(api$get_state(human, ICase1)))
    api$render("ICase2", length(api$get_state(human, ICase2)))
    api$render("IOxGetLive1", length(api$get_state(human, IOxGetLive1)))
    api$render("IOxGetLive2", length(api$get_state(human, IOxGetLive2)))
    api$render("IOxNotGetLive1", length(api$get_state(human, IOxNotGetLive1)))
    api$render("IOxNotGetLive2", length(api$get_state(human, IOxNotGetLive2)))
    api$render("IOxGetDie1", length(api$get_state(human, IOxGetDie1)))
    api$render("IOxGetDie2", length(api$get_state(human, IOxGetDie2)))
    api$render("IOxNotGetDie1", length(api$get_state(human, IOxNotGetDie1)))
    api$render("IOxNotGetDie2", length(api$get_state(human, IOxNotGetDie2)))
    api$render("IMVGetLive1", length(api$get_state(human, IMVGetLive1)))
    api$render("IMVGetLive2", length(api$get_state(human, IMVGetLive2)))
    api$render("IMVNotGetLive1", length(api$get_state(human, IMVNotGetLive1)))
    api$render("IMVNotGetLive2", length(api$get_state(human, IMVNotGetLive2)))
    api$render("IMVGetDie1", length(api$get_state(human, IMVGetDie1)))
    api$render("IMVGetDie2", length(api$get_state(human, IMVGetDie2)))
    api$render("IMVNotGetDie1", length(api$get_state(human, IMVNotGetDie1)))
    api$render("IMVNotGetDie2", length(api$get_state(human, IMVNotGetDie2)))
    api$render("IRec1", length(api$get_state(human, IRec1)))
    api$render("IRec2", length(api$get_state(human, IRec2)))
    api$render("D", length(api$get_state(human, D)))
    api$render("R", length(api$get_state(human, R)))
  }
}
