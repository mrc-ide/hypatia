#' @title Define model states
#' @description Create_states creates and initialises the human states for
#'  the model
#'
#' @title Create and initialise states
#'
#' @export
#'
#' @param psq the model parameters
#'
#' @return states
create_states <- function(psq) {

  pE <- sum(psq$E1_0) + sum(psq$E2_0)

  states <- list(
    # Human states
    S = individual::State$new("S", sum(psq$S_0)),
    E =  individual::State$new("E", pE),
    IMild = individual::State$new("IMild", sum(psq$IMild_0)),
    ICase1 = individual::State$new("ICase1", sum(psq$ICase1_0)),
    ICase2 = individual::State$new("ICase2", sum(psq$ICase2_0)),
    IOxGetLive1 = individual::State$new("IOxGetLive1", sum(psq$IOxGetLive1_0)),
    IOxGetLive2 = individual::State$new("IOxGetLive2", sum(psq$IOxGetLive2_0)),
    IOxGetDie1 = individual::State$new("IOxGetDie1", sum(psq$IOxGetDie1_0)),
    IOxGetDie2 = individual::State$new("IOxGetDie2", sum(psq$IOxGetDie2_0)),
    IOxNotGetLive1 = individual::State$new("IOxNotGetLive1",
                                           sum(psq$IOxNotGetLive1_0)),
    IOxNotGetLive2 = individual::State$new("IOxNotGetLive2",
                                           sum(psq$IOxNotGetLive2_0)),
    IOxNotGetDie1 = individual::State$new("IOxNotGetDie1",
                                          sum(psq$IOxNotGetDie1_0)),
    IOxNotGetDie2 = individual::State$new("IOxNotGetDie2",
                                          sum(psq$IOxNotGetDie2_0)),
    IMVGetLive1 = individual::State$new("IMVGetLive1", sum(psq$IMVGetLive1_0)),
    IMVGetLive2 = individual::State$new("IMVGetLive2", sum(psq$IMVGetLive2_0)),
    IMVGetDie1 = individual::State$new("IMVGetDie1", sum(psq$IMVGetDie1_0)),
    IMVGetDie2 = individual::State$new("IMVGetDie2", sum(psq$IMVGetDie2_0)),
    IMVNotGetLive1 = individual::State$new("IMVNotGetLive1",
                                           sum(psq$IMVNotGetLive1_0)),
    IMVNotGetLive2 = individual::State$new("IMVNotGetLive2",
                                           sum(psq$IMVNotGetLive2_0)),
    IMVNotGetDie1 = individual::State$new("IMVNotGetDie1",
                                          sum(psq$IMVNotGetDie1_0)),
    IMVNotGetDie2 = individual::State$new("IMVNotGetDie2",
                                          sum(psq$IMVNotGetDie2_0)),
    IRec1 = individual::State$new("IRec1", sum(psq$IRec1_0)),
    IRec2 = individual::State$new("IRec2", sum(psq$IRec2_0)),
    R = individual::State$new("R", sum(psq$R_0)),
    D = individual::State$new("D", sum(psq$D_0))
  )

  states
}
