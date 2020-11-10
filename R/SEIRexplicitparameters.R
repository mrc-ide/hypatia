#' Calculating the FOI
#'
#' @param human, human
#' @param IMild rest of the infections, which we consider to be mild and not
#'  require hospitalisation
#' @param ICase1 First of the compartments for infections that will require
#'  hospitalisation
#' @param ICase2 Second of the compartments for infections that will require
#'  hospitalisation
#' @param cum_hosp_inc cumulative hosptal incidences
#' @param i index
#' @param N_age number of points for a particular age
#' @param lambda FOI
#' @param problambda probability of
#' @param beta beta
#' @param m m
#' @param dt dt
#'
#' @export
#'
#' @examples
#' SEIRexplicitparameters(human, IMild, ICase1, ICase2, cum_hosp_inc, i, N_age,
#' lambda, problambda, beta, m, dt)
SEIRexplicitparameters <- function(human, IMild, ICase1, ICase2, cum_hosp_inc,
                                   i, N_age, lambda, problambda, beta, m, dt) {
  function(api) {

    # Generating Force of Infection
    newImild <- api$get_state(human, IMild)
    newICase1 <- api$get_state(human, ICase1)
    newICase2 <- api$get_state(human, ICase2)

    temp <- c(newImild, newICase1, newICase2)

    if (length(temp) == 0) {
      problambda <- 0
    }
    else {

      lambda <- beta * length(temp) * mean(m)
      if (!isEmpty(lambda) && lambda != numeric(0) && !is.na(lambda) == FALSE) {
        problambda <- 1 - exp(-lambda * dt)
      }
      else{
        problambda <- 0
      }

    }
  }
}

isEmpty <- function(x) {
  return(length(x) == 0)
}
