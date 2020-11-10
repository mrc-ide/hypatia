#' @importFrom stats rbinom
Infections2 <- function(I, S, pars) {

  # SIR: two types of events for S, so competing hazards. A fraction of
  # S events are deaths and the rest are infections.

  FOI <- (pars$beta * I) / pars$N
  fmu <- FOI + pars$mu
  fmudt <- fmu * pars$dt

  prob1 <- 1.0 - exp(-1.0 * fmudt)
  prob2 <- pars$mu / fmu

  n_events_S <- rbinom(1, S, prob1)

  if (n_events_S > 0) {
    n_deaths_S <- rbinom(1, n_events_S, prob2)
    n_infections_S <- n_events_S - n_deaths_S
  }
  else
  {
    n_deaths_S <- 0
    n_infections_S <- 0
  }

  list(n_deaths_S = n_deaths_S, n_infections_S = n_infections_S)
}

Recoveries2 <- function(I, pars) {
  # SIR: two types of events for I, so competing hazards and a fraction of
  # I events are deaths and the rest are recoveries
  coeff <- pars$nu + pars$mu
  coeffdt <- coeff * pars$dt

  prob1 <- 1.0 - exp(-1.0 * coeffdt)
  prob2 <- 1.0 - exp(-1.0 * pars$mu / coeff)

  n_events_I <- rbinom(1, I, prob1)

  n_deaths_I <- rbinom(1, n_events_I, prob2)
  n_recoveries_I <- n_events_I - n_deaths_I

  list(n_deaths_I = n_deaths_I, n_recoveries_I = n_recoveries_I)
}

#' @importFrom stats rbinom
Births2 <- function(R, pars) {

  coeffdt <- pars$mu * pars$dt
  prob <- 1.0 - exp(-1.0 * coeffdt)
  n_deaths_R <- rbinom(1, R, prob)
  n_births <- pars$n_deaths_S  + pars$n_deaths_I + n_deaths_R

  list(n_deaths_R = n_deaths_R, n_births = n_births)
}

Update2 <- function(S, I, R, pars) {

  news <- S - pars$n_deaths_S - pars$n_infections_S + pars$n_births
  newi <- I + pars$n_infections_S  - pars$n_recoveries_I  - pars$n_deaths_I
  newr <- R + pars$n_recoveries_I - pars$n_deaths_R
  newN <- news + newi + newr

  list(news = news, newi = newi, newr = newr)
}

#' @title Run the simulation with repetitions
#'
#' @param end_time end time for run
#' @param repetitions n times to run the simulation
#' @param pars parameter list
#' @param parallel execute runs in parallel, TRUE or FALSE
#' @return dataframe
#' @export
Run_with_repetitions2 <- function(
  end_time,
  repetitions,
  pars,
  parallel = FALSE
) {
  if (parallel) {
    fapply <- parallel::mclapply
  } else {
    fapply <- lapply
  }
  dfs <- fapply(
    seq(repetitions),
    function(repetition) {
      #df <- compartmental_sirmodel(end_time, pars)
      df$repetition <- repetition
      df
    }
  )

  do.call("rbind", dfs)
}

Displaythemodel_1 <- function(df) {

  # This function displays data in a list. df must be in the form of a list.

  # Check if df is a dataframe. If yes then turn it into a list
  numruns <- 0
  datapoints <- 0
  subtitle <- ""

  if (is.data.frame(df)) {
    numdatapoints <- paste(length(df$time))
    numruns <- 1
    df <- list(df)
    subtitle <- paste("Simulation for", numruns, "run and", numdatapoints,
                      "data points")
  }
  else{
    numruns <- length(df)
    numdatapoints <- length(df[[1]][[1]]) - 1
    subtitle <- paste("Simulation for", numruns, "runs,", numdatapoints,
                      "data points per run")
  }

  # Create group id for data
  df <- dplyr::bind_rows(df, .id = "group")

  # Convert to long format
  df <- tidyr::pivot_longer(tibble::as_tibble(df), c("S", "I", "R"))

  strname <- paste("SIR", df$type, "Model Simulation")

  ggplot2::ggplot(df, ggplot2::aes(x = df$time, y = df$value,
                  group = interaction(df$group, df$name), colour = df$name)) +
    ggplot2::geom_line(size=0.5) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = strname, subtitle = subtitle, color = df$legend) +
    ggplot2::labs(y ="S, I, & R", x="time") +
    ggplot2::theme(
      legend.justification = c("right", "top"),
      legend.box = c("horizontal", "vertical")
    ) +
    ggplot2::scale_colour_manual(values = c("blue", "red", "green")) +
    ggplot2::theme(text = ggplot2::element_text(color = "#444444",
                                                family = "Lucida Bright"),
                   plot.title = ggplot2::element_text(size = 26,
                                                      color = "#333333"),
                   plot.subtitle = ggplot2::element_text(size = 13),
                   axis.title.x = ggplot2::element_text(size = 16,
                                                        color = "#333333"),
                   axis.title.y = ggplot2::element_text(angle = 0, vjust = .5))

}

#' @title Infection_process -> S to I
#'
#' @param S susceptible
#' @param I infected
#' @param human human
#' @param immunity immunity
#' @param age age
#' @param location location
#' @param pars parameter list
#'
#' @export
#'
#' @examples
#' Individual_S_to_I_2(S, I, human, immunity, age, location, pars)
#' @importFrom stats runif
Individual_S_to_I_2 <- function(S, I, human, immunity, age, location,
                                pars = NULL) {
  function(api) {
    pars <- Get_parameters_for_sirstochastic(pars)

    # calculate information for infections, recoveries and births
    inf <- Infections2(length(api$get_state(human, I)),
                       length(api$get_state(human, S)), pars)

    n_to_infect <- inf$n_infections_S
    susceptible <- api$get_state(human, S)

    if (pars$novariations) {
      infected <- susceptible[sample.int(length(susceptible), n_to_infect)]
      api$queue_state_update(human, I, infected)
    }
    if (pars$includeimmune) {
      # Get the immunity for susceptible humans and use the complement to modify the
      # infection rate
      rate_modifier <- 1 - api$get_variable(human, immunity, susceptible)
      infected <- susceptible[runif(length(susceptible)) <
                                (pars$infection_rate * rate_modifier)]
      api$queue_state_update(human, I, infected)
    }
    if (pars$includeage) {
      # Get the age for susceptible humans and use the complement to modify the
      # infection rate
      rate_modifier <- 1 - api$get_variable(human, age, susceptible)
      infected <- susceptible[runif(length(susceptible)) <
                                (pars$location_rate * rate_modifier)]
      api$queue_state_update(human, I, infected)
    }
    if (pars$includelocation) {
      # Get the location for susceptible humans and use the complement to modify the
      # infection rate
      rate_modifier <- 1 - api$get_variable(human, location, susceptible)
      infected <- susceptible[runif(length(susceptible)) <
                                (pars$location_rate * rate_modifier)]
      api$queue_state_update(human, I, infected)
    }
  }
}

#' @title Recovery_process -> I to R
#'
#' @param I infected
#' @param R recovered
#' @param human human
#' @param immunity immunity
#' @param age age
#' @param location location
#' @param pars parameter list
#'
#' @export
#'
#' @examples
#' Individual_I_to_R_2(I, R, human, immunity, age, location, pars)
Individual_I_to_R_2 <- function(I, R, human, immunity, age, location,
                                pars = NULL) {
  function(api) {

    pars <- Get_parameters_for_sirstochastic(pars)
    rec <- Recoveries2(length(api$get_state(human, I)), pars)
    n_to_recover <- rec$n_recoveries_I
    infected <- api$get_state(human, I)

    if (pars$novariations) {
      recovered <- infected[sample.int(length(infected), n_to_recover)]
      api$queue_state_update(human, R, recovered)
    }
    if (pars$includeage) {
      rate_modifier <- 1 - api$get_variable(human, age, infected)
      recovered <- infected[runif(length(infected)) <
                              (pars$age_rate * rate_modifier)]
      api$queue_state_update(human, I, recovered)
      #api$queue_state_update(human, R, recovered)
    }
    if (pars$includelocation) {
      rate_modifier <- 1 - api$get_variable(human, location, infected)
      recovered <- infected[runif(length(infected)) <
                              (pars$location_rate * rate_modifier)]
      api$queue_state_update(human, I, recovered)
      #api$queue_state_update(human, R, recovered)
    }
  }
}

#' @title Recovered to susceptible -> R to S
#'
#' @param S susceptible
#' @param R recovered
#' @param human human
#' @param immunity immunity
#' @param age age
#' @param location location
#' @param pars parameter list
#'
#' @export
#'
#' @examples
#' Individual_R_to_S_2(S, R, human, immunity, age, location, pars)
#' @importFrom stats runif
Individual_R_to_S_2 <- function(S, R, human, immunity, age, location,
                                pars = NULL) {
  function(api) {

    pars <- Get_parameters_for_sirstochastic(pars)
    bir <- Births2(length(api$get_state(human, R)), pars)
    n_to_susceptible <- bir$n_births
    from_state <- api$get_state(human, R)

    if (pars$novariations) {
      if (length(from_state) != 0 && length(from_state) > n_to_susceptible)
      {
        thenewsusceptible <- from_state[sample.int(length(from_state),
                                                   n_to_susceptible)]
        api$queue_state_update(human, S, thenewsusceptible)
      }
    }
    if (pars$includeimmune) {
      recovered <- from_state[runif(length(from_state)) < pars$recovery_rate]
      api$queue_state_update(human, R, recovered)
      api$queue_variable_update(human, immunity,
                                api$get_parameters()$immunity_level, recovered)
    }
    if (pars$includeage) {
      recovered <- from_state[runif(length(from_state)) < pars$age_rate]
      api$queue_state_update(human, R, recovered)
      api$queue_variable_update(human, age, api$get_parameters()$age_level,
                                recovered)
    }
    if (pars$includelocation) {
      recovered <- from_state[runif(length(from_state)) < pars$recovery_rate]
      api$queue_state_update(human, R, recovered)
      api$queue_variable_update(human, age, api$get_parameters()$location_level,
                                recovered)
    }
  }
}

#' @title Renders the sizes for S, I, R
#' @param S S
#' @param I I
#' @param R R
#' @param human human
#' @export
#' @examples
#' Render_state_sizes_2(S, I, R, human)
Render_state_sizes_2 <- function(S, I, R, human) {
  function(api) {
    api$render("susceptable_counts", length(api$get_state(human, S)))
    api$render("infected_counts", length(api$get_state(human, I)))
    api$render("recovered_counts", length(api$get_state(human, R)))
  }
}










