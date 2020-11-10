#' @title infections for SIR model
#' @param I I
#'
#' @param S S
#' @param pars list
#' @export
#'
#' @importFrom stats rbinom
Infections <- function(I, S, pars) {

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
  else {
    n_deaths_S <- 0
    n_infections_S <- 0
  }

  list(n_deaths_S = n_deaths_S, n_infections_S = n_infections_S)
}

Recoveries <- function(I, pars) {
  # SIR: two types of events for I, so competing hazards and a fraction of
  # I events are deaths and the rest are recoveries
  coeff <- pars$nu + pars$mu
  coeffdt <- coeff * pars$dt

  prob1 <- 1.0 - exp(-1.0 * coeffdt)
  prob2 <- 1.0 - exp(-1.0 * pars$mu/coeff)

  n_events_I <- rbinom(1, I, prob1)

  n_deaths_I <- rbinom(1, n_events_I, prob2)
  n_recoveries_I <- n_events_I - n_deaths_I

  list(n_deaths_I = n_deaths_I, n_recoveries_I = n_recoveries_I)
}

#' @importFrom stats rbinom
Births <- function(R, pars) {

  coeffdt <- pars$mu * pars$dt
  prob <- 1.0 - exp(-1.0 * coeffdt)
  n_deaths_R <- rbinom(1, R, prob)
  n_births <- pars$n_deaths_S  + pars$n_deaths_I + n_deaths_R

  list(n_deaths_R = n_deaths_R, n_births = n_births)
}

Update <- function(S, I, R, pars) {

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
Run_with_repetitions <- function(
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

Displaythemodel <- function(df) {

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
  } else {
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

  ggplot2::ggplot(df,
                  ggplot2::aes(x = df$time, y = df$value,
                               group = interaction(df$group, df$name),
                               colour = df$name)) +
    ggplot2::geom_line(size = 0.5) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = strname, subtitle = subtitle, color = df$legend) +
    ggplot2::labs(y = "S, I, & R", x = "time") +
    ggplot2::theme(
      legend.justification = c("right", "top"),
      legend.box = c("horizontal", "vertical")
    ) +
    ggplot2::scale_colour_manual(values = c("blue", "red", "green")) +
    ggplot2::theme(
      text = ggplot2::element_text(color = "#444444", family = "Lucida Bright"),
      plot.title = ggplot2::element_text(size = 26, color = "#333333"),
      plot.subtitle = ggplot2::element_text(size = 13),
      axis.title.x = ggplot2::element_text(size = 16, color = "#333333"),
      axis.title.y = ggplot2::element_text(angle = 0, vjust = .5))
}

Displaythemodel2 <- function(df) {

  # This function displays data in a list. df must be in the form of a list.

  # Check if df is a dataframe. If yes then turn it into a list
  numruns <- 0
  datapoints <- 0
  subtitle <- ""

  if (is.data.frame(df)) {
    numdatapoints <- paste(length(df$time))
    numruns <- 1
    df <- list(df)
    subtitle <- paste(
      "Simulation for", numruns, "run and", numdatapoints, "data points")
  } else {
    numruns <- length(df)
    numdatapoints <- length(df[[1]][[1]]) - 1
    subtitle <- paste(
      "Simulation for", numruns, "runs,", numdatapoints, "data points per run")
  }

  # Create group id for data
  df <- dplyr::bind_rows(df, .id = "group")

  # Convert to long format
  df <- tidyr::pivot_longer(tibble::as_tibble(df), c("S", "I", "I2", "R", "D"))

  strname <- paste("SIR", df$type, "Model Simulation")

  ggplot2::ggplot(
    df, ggplot2::aes(
      x = df$time, y = df$value,
      group = interaction(df$group, df$name), colour = df$name)) +
    ggplot2::geom_line(size = 0.5) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = strname, subtitle = subtitle, color=df$legend) +
    ggplot2::labs(y = "S, I, & R", x = "time") +
    ggplot2::theme(
      legend.justification = c("right", "top"),
      legend.box = c("horizontal", "vertical")
    ) +
    ggplot2::scale_colour_manual(
      values = c("blue", "red", "green", "purple", "yellow")) +
    ggplot2::theme(
      text = ggplot2::element_text(color = "#444444", family = "Lucida Bright"),
      plot.title = ggplot2::element_text(size = 26, color = "#333333"),
      plot.subtitle = ggplot2::element_text(size = 13),
      axis.title.x = ggplot2::element_text(size = 16, color = "#333333"),
      axis.title.y = ggplot2::element_text(angle = 0, vjust = .5))
}

Displaythemodel3 <- function(df) {

  # This function displays data in a list. df must be in the form of a list.

  # Check if df is a dataframe. If yes then turn it into a list
  numruns <- 0
  datapoints <- 0
  subtitle <- ""

  if (is.data.frame(df)) {
    numdatapoints <- paste(length(df$time))
    numruns <- 1
    df <- list(df)
    subtitle <- paste(
      "Simulation for", numruns, "run and", numdatapoints, "data points")
  } else {
    numruns <- length(df)
    numdatapoints <- length(df[[1]][[1]]) - 1
    subtitle <- paste(
      "Simulation for", numruns, "runs,", numdatapoints, "data points per run")
  }

  # Create group id for data
  df <- dplyr::bind_rows(df, .id = "group")

  #Convert to long format
  df <- tidyr::pivot_longer(tibble::as_tibble(df),
                            c("S", "E1", "E2", "IMild", "ICase1", "ICase2",
                              "cum_hosp_inc", "IOxGetLive1", "IOxGetLive2",
                              "IOxNotGetLive1", "IOxNotGetLive2", "IOxGetDie1",
                              "IOxGetDie2", "IOxNotGetDie1", "IOxNotGetDie2",
                              "IMVGetLive1", "IMVGetLive2", "IMVNotGetLive1",
                              "IMVNotGetLive2", "IMVGetDie1", "IMVGetDie2",
                              "IMVNotGetDie1", "IMVNotGetDie2", "IRec1",
                              "IRec2", "R", "D"))

  #df <- tidyr::pivot_longer(tibble::as_tibble(df), cols = df$nameslist)

  strname <- paste("SIR", df$type, "Model Simulation")

  ggplot2::ggplot(
                  df, ggplot2::aes(x = df$time, y = df$value,
                  group = interaction(df$group, df$name), colour = df$name)) +
    ggplot2::geom_line(size = 0.5) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = strname, subtitle = subtitle, color = df$legend) +
    ggplot2::labs(y = "States", x = "time") +
    ggplot2::theme(
      legend.justification = c("right", "top"),
      legend.box = c("horizontal", "vertical")
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(color = "#444444", family = "Lucida Bright"),
      plot.title = ggplot2::element_text(size = 26, color = "#333333"),
      plot.subtitle = ggplot2::element_text(size = 13),
      axis.title.x = ggplot2::element_text(size = 16, color = "#333333"),
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
#' Individual_S_to_I(S, I, immunity, age, location, pars)
#' @importFrom stats runif
Individual_S_to_I <- function(
  S, I, human, immunity, age, location, pars = NULL) {
  function(api) {

    pars <- Get_parameters_for_sirstochastic(pars)

    # calculate information for infections, recoveries and births
    inf <- Infections(length(api$get_state(human, I)),
                      length(api$get_state(human, S)), pars)

    n_to_infect <- inf$n_infections_S
    susceptible <- api$get_state(human, S)

    if (pars$novariations) {
      infected <- susceptible[sample.int(length(susceptible), n_to_infect)]
      api$queue_state_update(human, I, infected)
    }
    if (pars$includeimmune) {
      # Get the immunity for susceptible humans and use the complement to modify
      # the infection rate
      rate_modifier <- 1 - api$get_variable(human, immunity, susceptible)
      prob <- pars$infection_rate * rate_modifier
      individual::fixed_probability_state_change_process(
        "human", S$name, I$name, prob)
    }
    if (pars$includeage) {
      # Get the age for susceptible humans and use the complement to modify the
      # infection rate
      rate_modifier <- 1 - api$get_variable(human, age, susceptible)
      prob <- pars$age_rate * rate_modifier
      individual::fixed_probability_state_change_process(
        "human", S$name, I$name, prob)
    }
    if (pars$includelocation) {
      # Get the location for susceptible humans and use the complement to modify
      # the infection rate
      rate_modifier <- 1 - api$get_variable(human, location, susceptible)
      prob <- pars$location_rate * rate_modifier
      individual::fixed_probability_state_change_process(
        "human", S$name, I$name, prob)
    }
  }
}

#' @title Infection_process -> S to I and I2
#'
#' @param S susceptible
#' @param I infected
#' @param I2 severelyinfected
#' @param human human
#' @param immunity immunity
#' @param age age
#' @param location location
#' @param pars parameter list
#'
#' @export
#'
#' @examples
#' Individual_S_to_I_and_I2(S, I, I2, human, immunity, age, location, pars)
#' @importFrom stats runif
Individual_S_to_I_and_I2 <- function(
  S, I, I2, human, immunity, age, location, pars = NULL) {
  function(api) {
    warnings()

    pars <- Get_parameters_for_sirstochastic(pars)
    # calculate information for infections, recoveries and births
    inf <- Infections(
      length(api$get_state(human, I)) + length(api$get_state(human, I2)),
      length(api$get_state(human, S)), pars)

    n_to_infect <- inf$n_infections_S
    susceptible <- api$get_state(human, S)

    if (pars$novariations) {
      if (length(susceptible) != 0 && length(susceptible) > n_to_infect) {
        infected <- as.integer(
          round(susceptible[sample.int(length(susceptible), n_to_infect)]))

        infected1 <- as.integer(pars$infection_rate * infected)
        severelyinfected <- as.integer(infected - infected1)

        if (length(infected1) != 0 && infected1 != 0) {
          Validated_state_update(api, human, I, infected1, pars$N)
        }

        if (length(severelyinfected) != 0 && severelyinfected != 0) {
          Validated_state_update(api, human, I2, severelyinfected, pars$N)
        }
      }
    }
    if (pars$includeimmune) {
      # Get the immunity for susceptible humans and use the complement to modify
      # the infection rate
      rate_modifier <- 1 - api$get_variable(human, immunity, susceptible)

      prob <- pars$infection_rate * rate_modifier
      individual::fixed_probability_state_change_process("human", S, I, prob)

      prob <- pars$severe_infection_rate * rate_modifier
      individual::fixed_probability_state_change_process("human", S, I2, prob)

    }
    if (pars$includeage) {
      # Get the age for susceptible humans and use the complement to modify the
      # infection rate
      rate_modifier <- 1 - api$get_variable(human, age, susceptible)
      if (length(susceptible) != 0
          && susceptible != 0 && !(any(is.na(susceptible)))) {

        prob <- pars$infection_rate * pars$age_rate * rate_modifier
        individual::fixed_probability_state_change_process("human", S, I, prob)

        prob <- pars$severe_infection_rate * pars$age_rate * rate_modifier
        individual::fixed_probability_state_change_process("human", S, I2, prob)
      }

    }
    if (pars$includelocation) {
      # Get the location for susceptible humans and use the complement to modify
      # the infection rate
      rate_modifier <- 1 - api$get_variable(human, location, susceptible)

      infected <- susceptible[
        runif(length(susceptible)) < (pars$location_rate * rate_modifier)]

      if (length(infected) != 0 && infected != 0) {

        prob <- pars$infection_rate * pars$location_rate * rate_modifier
        individual::fixed_probability_state_change_process("human", S, I, prob)

        prob <- pars$severe_infection_rate * pars$location_rate * rate_modifier
        individual::fixed_probability_state_change_process("human", S, I2, prob)
      }
    }
  }
}

#' @title Recovery_process and death process -> I to R and I2 to D
#'
#' @param I infected
#' @param R recovered
#' @param I2 severely infected
#' @param D dead
#' @param human human
#' @param immunity immunity
#' @param age age
#' @param location location
#' @param pars parameter list
#'
#' @export
#'
#' @examples
#' Individual_I_to_R_I2_to_D(I, R, I2, D, human, immunity, age, location, pars)
Individual_I_to_R_I2_to_D <- function(
  I, R, I2, D, human, immunity, age, location, pars = NULL) {
  function(api) {
    warnings()

    pars <- Get_parameters_for_sirstochastic(pars)
    rec <- Recoveries(
      length(api$get_state(human, I)) + length(api$get_state(human, I2)), pars)

    n_to_recover <- rec$n_recoveries_I
    infected <- api$get_state(human, I)
    n_to_die <- rec$n_deaths_I
    severelyinfected <- api$get_state(human, I2)

    if (pars$novariations) {
      if (length(infected) != 0 && length(infected) > n_to_recover)
      {
        recovered <- as.integer(
          round(infected[sample.int(length(infected), n_to_recover)]))

        if (length(recovered) != 0 && recovered != 0) {
          Validated_state_update(api, human, R, recovered, pars$N)
        }
      }

      if (length(severelyinfected) != 0 && length(severelyinfected) > n_to_die)
      {
        dead <- severelyinfected[sample.int(length(severelyinfected), n_to_die)]
        if (length(dead) != 0 && dead != 0 && !(any(is.na(dead)))) {
          Validated_state_update(api, human, D, dead, pars$N)
        }
      }
    }
    if (pars$includeage) {
      rate_modifier_infected <- 1 - api$get_variable(human, age, infected)
      rate_modifier_severelyinfected <-
        1 - api$get_variable(human, age, severelyinfected)

      if (length(infected) != 0 && infected != 0 && !any(is.na(infected))) {

        recovered <- as.integer(round(infected[
          sample.int(length(infected),
                     as.integer(abs(pars$age_rate * rate_modifier_infected)))]))

        if (length(recovered) != 0) {
          Validated_state_update(api, human, R, recovered, pars$N)
        }
      }

      dead <- as.integer(round(severelyinfected[
        sample.int(length(severelyinfected),
        as.integer(abs(pars$age_rate * rate_modifier_severelyinfected)))]))

      if (length(dead) != 0 && dead != 0 && !(any(is.na(dead)))) {
        Validated_state_update(api, human, D, dead, pars$N)
      }
    }
    if (pars$includelocation) {
      rate_modifier_infected <- 1 - api$get_variable(human, location, infected)
      rate_modifier_severelyinfected <-
        1 - api$get_variable(human, location, severelyinfected)

      if (length(infected) != 0 && infected != 0 && !any(is.na(infected))) {
        recovered <- as.integer(
          round(infected[sample.int(length(infected),
          as.integer(abs(pars$age_rate * rate_modifier_infected)))]))

        if (length(recovered) != 0 && recovered != 0)
        {
          Validated_state_update(api, human, R, recovered, pars$N)
        }
      }

      dead <- as.integer(round(severelyinfected[
        sample.int(length(severelyinfected),
        as.integer(abs(pars$location_rate * rate_modifier_severelyinfected)))]))
      if (length(dead) != 0 && dead != 0 && !(any(is.na(dead)))) {
        Validated_state_update(api, human, D, dead, pars$N)
      }
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
#' Individual_R_to_S(R, S, human, immunity, age, location, pars)
#' @importFrom stats runif
Individual_R_to_S <- function(
  S, R, human, immunity, age, location, pars = NULL) {
  function(api) {

    pars <- Get_parameters_for_sirstochastic(pars)
    bir <- Births(length(api$get_state(human, R)), pars)
    n_to_susceptible <- bir$n_births
    from_state <- api$get_state(human, R)

    if (pars$novariations) {
      if (length(from_state) != 0 && length(from_state) > n_to_susceptible)
      {
        thenewsusceptible <- from_state[
          sample.int(length(from_state), n_to_susceptible)]
        api$queue_state_update(human, S, thenewsusceptible)
      }
    }
    if (pars$includeimmune) {
      recovered <- from_state[runif(length(from_state)) < pars$recovery_rate]
      api$queue_state_update(human, R, recovered)
      api$queue_variable_update(
        human, immunity, api$get_parameters()$immunity_level, recovered)
    }
    if (pars$includeage) {
      recovered <- from_state[runif(length(from_state)) < pars$age_rate]
      api$queue_state_update(human, R, recovered)
      api$queue_variable_update(
        human, age, api$get_parameters()$age_level, recovered)
    }
    if (pars$includelocation) {
      recovered <- from_state[runif(length(from_state)) < pars$recovery_rate]
      api$queue_state_update(human, R, recovered)
      api$queue_variable_update(
        human, age, api$get_parameters()$location_level, recovered)
    }
  }
}

#' @title Function for transition from E2 to IMild
#'
#' @param human human
#' @param IMild IMild
#' @param E2 E2
#' @param ICase1 ICase1
#' @param p_E2_I p_E2_I
#' @param prob_hosp prob_hosp
#' @export
#'
#' @examples
#' E2_IMild(human, IMild, E2, ICase1, p_E2_I, prob_hosp)
E2_IMild <- function(human, IMild, E2, ICase1, p_E2_I, prob_hosp) {
  function(api) {
    E2 <- api$get_state(human, E2)
    E2_I <- rbinom(length(E2), 1, p_E2_I)
    E2_I_pos <- E2[as.logical(E2_I)]

    n_E2_ICase1 <- rbinom(length(E2_I_pos), 1, prob_hosp)
    new_ICase1 <- E2_I_pos[as.logical(n_E2_ICase1)]
    new_IMild1 <- E2_I_pos[!as.logical(n_E2_ICase1)]

    api$queue_state_update(human, ICase1, new_ICase1)
    api$queue_state_update(human, IMild, new_IMild1)
  }
}

#' @title Renders the sizes for S, I, R
#' @param S S
#' @param I I
#' @param R R
#' @param human human
#' @export
#' @examples
#' Render_state_sizes(S, I, R, human)
#'
Render_state_sizes <- function(S, I, R, human) {
  function(api) {
    api$render("susceptable_counts", length(api$get_state(human, S)))
    api$render("infected_counts", length(api$get_state(human, I)))
    api$render("recovered_counts", length(api$get_state(human, R)))
  }
}

#' @title Renders the sizes for S, I, R, I2 and D
#' @param S S
#' @param I I
#' @param R R
#' @param I2 I2
#' @param D D
#' @param human human
#' @export
#' @examples
#' Render_state_sizes2(S, I, R, I2, D, human)
Render_state_sizes2 <- function(S, I, R, I2, D, human) {
  function(api) {
    api$render("susceptable_counts", length(api$get_state(human, S)))
    api$render("infected_counts", length(api$get_state(human, I)))
    api$render("severelyinfected_counts", length(api$get_state(human, I2)))
    api$render("dead_counts", length(api$get_state(human, D)))
    api$render("recovered_counts", length(api$get_state(human, R)))
  }
}

#' Render sizes for SQUIRE states - note IMild is missing for now
#'
#' @param S susceptible
#' @param E1 First of the latent infection compartments
#' @param E2 Second of the latent infection compartments
#' @param IMild rest of the infections, which we consider to be mild and not
#'  require hospitalisation
#' @param ICase1 First of the compartments for infections that will require
#' hospitalisation
#' @param ICase2 Second of the compartments for infections that will require
#' hospitalisation
#' @param cum_hosp_inc Those requiring hospitalisation
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
#'
#' @export
#' @examples
#' Render_state_sizes3(S, E1, E2, IMildICase1, ICase2, cum_hosp_inc,
#'  IOxGetLive1, IOxGetLive2, IOxNotGetLive1, IOxNotGetLive2, IOxGetDie1,
#'  IOxGetDie2, IOxNotGetDie1, IOxNotGetDie2, IMVGetLive1, IMVGetLive2,
#'   IMVNotGetLive1, IMVNotGetLive2, IMVGetDie1, IMVGetDie2, IMVNotGetDie1,
#'    IMVNotGetDie2, IRec1, IRec2, R, D, human)
Render_state_sizes3 <- function(S, E1, E2, IMild, ICase1, ICase2,
                                cum_hosp_inc, IOxGetLive1, IOxGetLive2,
                                IOxNotGetLive1, IOxNotGetLive2, IOxGetDie1,
                                IOxGetDie2, IOxNotGetDie1, IOxNotGetDie2,
                                IMVGetLive1, IMVGetLive2, IMVNotGetLive1,
                                IMVNotGetLive2, IMVGetDie1, IMVGetDie2,
                                IMVNotGetDie1, IMVNotGetDie2, IRec1, IRec2, R,
                                D, human) {
  function(api) {
    api$render("S", length(api$get_state(human, S)))
    api$render("E1", length(api$get_state(human, E1)))
    api$render("E2", length(api$get_state(human, E2)))
    api$render("IMild", length(api$get_state(human, IMild)))
    api$render("ICase1", length(api$get_state(human, ICase1)))
    api$render("ICase2", length(api$get_state(human, ICase2)))
    api$render("cum_hosp_inc", length(api$get_state(human, cum_hosp_inc)))
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

#' @title Renders the sizes for S, E1, E2, I
#' @param S S
#' @param E1 E1
#' @param E2 E2
#' @param I I
#' @param human human
#' @export
#' @examples
#' Render_state_sizes4(S, E1, E2, I, human)
Render_state_sizes4 <- function(S, E1, E2, I, human) {
  function(api) {
    api$render("S", length(api$get_state(human, S)))
    api$render("E1", length(api$get_state(human, E1)))
    api$render("E2", length(api$get_state(human, E2)))
    api$render("I", length(api$get_state(human, I)))
  }
}

#' @title Check state update is valid
#'
#' @param api api
#' @param i human etc
#' @param state S, I, R, etc
#' @param ix index
#' @param population_size population size
#'
#' @export
Validated_state_update <- function(api, i, state, ix, population_size) {

  if (any(ix > population_size)) {
    stop(paste0("Your ix ", ix, " for ", i$name, ":", state$name,
                " is greater than the population size \n"))
  }
  if (any(is.integer(ix) == FALSE)) {
    stop(paste0("Your ix ", ix, " for ", i$name, ":", state$name,
                " is not an integer \n"))
  }
  if (any(ix <= 0)) {
    stop(paste0("Your ix ", ix, " for ", i$name, ":", state$name,
                " is less than or equal to 0 \n"))
  }
  if (any(is.na(ix))) {
    stop(paste0("Your ix ", ix, " for ", i$name, ":", state$name,
                " is not a number \n"))
  }

  api$queue_state_update(i, state, ix)
}
