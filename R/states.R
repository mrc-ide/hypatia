#' @title Define model states
#' @description
#' Create_states creates and initialises the human states for the model
#'
#' @title Create and initialise states
#'
#' @param psq the model parameters
#'
#' @noRd
#' @return list of states
create_states <- function(psq) {

  # Sum up the states from squire
  S <- sum(psq$sq$S_0)
  E <- sum(psq$sq$E1_0, psq$sq$E2_0)
  IMild <- sum(psq$sq$IMild_0)
  IAsymp <- sum(psq$IAsymp_0)
  ICase <- sum(psq$sq$ICase1_0, psq$sq$ICase2_0)
  IOxGetLive <- sum(psq$sq$IOxGetLive1_0, psq$sq$IOxGetLive2_0)
  IOxGetDie <- sum(psq$sq$IOxGetDie1_0, psq$sq$IOxGetDie2_0)
  IOxNotGetLive <- sum(psq$sq$IOxNotGetLive1_0, psq$sq$IOxNotGetLive2_0)
  IOxNotGetDie <- sum(psq$sq$IOxNotGetDie1_0, psq$sq$IOxNotGetDie2_0)
  IMVGetLive <- sum(psq$sq$IMVGetLive1_0, psq$sq$IMVGetLive2_0)
  IMVGetDie <- sum(psq$sq$IMVGetDie1_0, psq$sq$IMVGetDie2_0)
  IMVNotGetLive <- sum(psq$sq$IMVNotGetLive1_0, psq$sq$IMVNotGetLive2_0)
  IMVNotGetDie <- sum(psq$sq$IMVNotGetDie1_0, psq$sq$IMVNotGetDie2_0)
  IRec <- sum(psq$sq$IRec1_0, psq$sq$IRec2_0)
  R <- sum(psq$sq$R_0)
  D <- sum(psq$sq$D_0)

  # Create states for Humans
  list(
    S = individual::State$new("S", S),
    E = individual::State$new("E", E),
    IMild = individual::State$new("IMild", IMild),
    IAsymp = individual::State$new("IAsymp", IAsymp),
    ICase = individual::State$new("ICase", ICase),
    IOxGetLive = individual::State$new("IOxGetLive", IOxGetLive),
    IOxGetDie = individual::State$new("IOxGetDie", IOxGetDie),
    IOxNotGetLive = individual::State$new("IOxNotGetLive", IOxNotGetLive),
    IOxNotGetDie = individual::State$new("IOxNotGetDie", IOxNotGetDie),
    IMVGetLive = individual::State$new("IMVGetLive", IMVGetLive),
    IMVGetDie = individual::State$new("IMVGetDie", IMVGetDie),
    IMVNotGetLive = individual::State$new("IMVNotGetLive", IMVNotGetLive),
    IMVNotGetDie = individual::State$new("IMVNotGetDie", IMVNotGetDie),
    IRec = individual::State$new("IRec", IRec),
    R = individual::State$new("R", R),
    D = individual::State$new("D", D)
  )
}
