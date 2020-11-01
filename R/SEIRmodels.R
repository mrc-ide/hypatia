#' Create an explicit model
#'
#' @title Explicit SEIR model creation.
#'
#' We will use this structure to ensure that model fitting is flexible in the
#' future as more models are added
#'
#' @export
explicit_model <- function() {

  model_class <- "explicit_SEIR_model"
  compare_model <- function(model, pars_obs, data) {
    compare_output(model, pars_obs, data, type=model_class)
  }

  explicit_model <- list(odin_model = explicit_SEIR,
                         generate_beta_func = beta_est_explicit,
                         parameter_func = parameters_explicit_SEEIR,
                         run_func = run_explicit_SEEIR_model,
                         compare_model = compare_model)
  class(explicit_model) <- c(model_class, "stochastic", "squire_model")
  explicit_model

}


#' @importFrom stats rbinom
infections <- function(I, S, pars){

  # SIR: two types of events for S, so competing hazards. A fraction of
  # S events are deaths and the rest are infections.

  FOI <- (pars$beta * I)/pars$N
  fmu <- FOI + pars$mu
  fmudt <- fmu * pars$dt

  prob1 <- 1.0 - exp(-1.0 * fmudt)
  prob2 <- pars$mu/fmu

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

recoveries <- function(I, pars){
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
births <- function(R, pars){

  coeffdt <- pars$mu * pars$dt
  prob <- 1.0 - exp(-1.0 * coeffdt)
  n_deaths_R <- rbinom(1, R, prob)
  n_births <- pars$n_deaths_S  + pars$n_deaths_I + n_deaths_R

  list(n_deaths_R = n_deaths_R, n_births = n_births)
}

update <- function(S, I, R, pars){

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
run_with_repetitions <- function(
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

displaythemodel <- function(df) {

  # This function displays data in a list. df must be in the form of a list.

  # Check if df is a dataframe. If yes then turn it into a list
  numruns <- 0
  datapoints <- 0
  subtitle <- ""

  if (is.data.frame(df)) {
    numdatapoints <- paste(length(df$time))
    numruns <- 1
    df <- list(df)
    subtitle <- paste('Simulation for', numruns, 'run and', numdatapoints, 'data points')
  }
  else{
    numruns <- length(df)
    numdatapoints <- length(df[[1]][[1]])-1
    subtitle <- paste('Simulation for', numruns, 'runs,', numdatapoints, 'data points per run')
  }

  # Create group id for data
  df <- dplyr::bind_rows(df, .id = "group")

  # Convert to long format
  df <- tidyr::pivot_longer(tibble::as_tibble(df), c("S", "I", "R"))

  strname <- paste("SIR", df$type, "Model Simulation")

  ggplot2::ggplot(df, ggplot2::aes(x=df$time, y=df$value, group=interaction(df$group, df$name), colour=df$name ) ) +
    ggplot2::geom_line(size=0.5) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = strname, subtitle = subtitle, color=df$legend) +
    ggplot2::labs(y ="S, I, & R", x="time") +
    ggplot2::theme(
      legend.justification = c("right", "top"),
      legend.box = c("horizontal", "vertical")
    ) +
    ggplot2::scale_colour_manual(values=c("blue", "red", "green")) +
    ggplot2::theme(text = ggplot2::element_text(color = "#444444", family = 'Lucida Bright'),
                   plot.title = ggplot2::element_text(size = 26, color = '#333333'),
                   plot.subtitle = ggplot2::element_text(size = 13),
                   axis.title.x = ggplot2::element_text(size = 16, color = '#333333'),
                   axis.title.y = ggplot2::element_text(angle = 0, vjust = .5))

}


displaythemodel2 <- function(df) {

  # This function displays data in a list. df must be in the form of a list.

  # Check if df is a dataframe. If yes then turn it into a list
  numruns <- 0
  datapoints <- 0
  subtitle <- ""

  if (is.data.frame(df)) {
    numdatapoints <- paste(length(df$time))
    numruns <- 1
    df <- list(df)
    subtitle <- paste('Simulation for', numruns, 'run and', numdatapoints, 'data points')
  }
  else{
    numruns <- length(df)
    numdatapoints <- length(df[[1]][[1]])-1
    subtitle <- paste('Simulation for', numruns, 'runs,', numdatapoints, 'data points per run')
  }

  # Create group id for data
  df <- dplyr::bind_rows(df, .id = "group")

  # Convert to long format
  df <- tidyr::pivot_longer(tibble::as_tibble(df), c("S", "I", "I2", "R", "D"))

  strname <- paste("SIR", df$type, "Model Simulation")

  ggplot2::ggplot(df, ggplot2::aes(x=df$time, y=df$value, group=interaction(df$group, df$name), colour=df$name ) ) +
    ggplot2::geom_line(size=0.5) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = strname, subtitle = subtitle, color=df$legend) +
    ggplot2::labs(y ="S, I, & R", x="time") +
    ggplot2::theme(
      legend.justification = c("right", "top"),
      legend.box = c("horizontal", "vertical")
    ) +
    ggplot2::scale_colour_manual(values=c("blue", "red", "green", "purple", "yellow")) +
    ggplot2::theme(text = ggplot2::element_text(color = "#444444", family = 'Lucida Bright'),
                   plot.title = ggplot2::element_text(size = 26, color = '#333333'),
                   plot.subtitle = ggplot2::element_text(size = 13),
                   axis.title.x = ggplot2::element_text(size = 16, color = '#333333'),
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
#' individual_S_to_I(S, I, immunity, age, location, pars)
#' @importFrom stats runif
individual_S_to_I <- function(S, I, human, immunity, age, location, pars = NULL) {
function(api) {

  pars <- get_parameters_for_sirstochastic(pars)

  # calculate information for infections, recoveries and births
  inf <- infections(length(api$get_state(human, I)), length(api$get_state(human, S)), pars)

  n_to_infect <- inf$n_infections_S
  susceptible <- api$get_state(human, S)

  if(pars$novariations){
    infected <- susceptible[sample.int(length(susceptible), n_to_infect)]
    api$queue_state_update(human, I, infected)
  }
  if (pars$includeimmune) {
    # Get the immunity for susceptible humans and use the complement to modify the
    # infection rate
    rate_modifier <- 1 - api$get_variable(human, immunity, susceptible)
    infected <- susceptible[runif(length(susceptible)) , 20] #< (pars$infection_rate * rate_modifier)]
    api$queue_state_update(human, I, infected)
  }
  if (pars$includeage) {
    # Get the age for susceptible humans and use the complement to modify the
    # infection rate
    rate_modifier <- 1 - api$get_variable(human, age, susceptible)
    infected <- susceptible[runif(length(susceptible)) , 20] #< (pars$location_rate * rate_modifier)]
    api$queue_state_update(human, I, infected)
  }
  if (pars$includelocation) {
    # Get the location for susceptible humans and use the complement to modify the
    # infection rate
    rate_modifier <- 1 - api$get_variable(human, location, susceptible)
    infected <- susceptible[runif(length(susceptible)) , 20] #< (pars$location_rate * rate_modifier)]
    api$queue_state_update(human, I, infected)
  }
}
}
# individual_S_to_I <- function(S, I, human, immunity, age, location, pars = NULL) {
#   function(api) {
#
#     pars <- get_parameters_for_sirstochastic(pars)
#
#     # calculate information for infections, recoveries and births
#     inf <- infections(length(api$get_state(human, I)), length(api$get_state(human, S)), pars)
#
#     n_to_infect <- inf$n_infections_S
#     susceptible <- api$get_state(human, S)
#
#     if(pars$novariations){
#       if(length(susceptible) != 0 && length(susceptible) > n_to_infect)
#       {
#         infected <- susceptible[sample.int(length(susceptible), n_to_infect)]
#         if(length(infected) != 0) api$queue_state_update(human, I, infected)
#       }
#     }
#     if (pars$includeimmune) {
#       # Get the immunity for susceptible humans and use the complement to modify the
#       # infection rate
#       rate_modifier <- 1 - api$get_variable(human, immunity, susceptible)
#       infected <- susceptible[runif(length(susceptible)) < (pars$infection_rate * rate_modifier)]
#       if(length(infected) != 0) api$queue_state_update(human, I, infected)
#     }
#     if (pars$includeage) {
#       # Get the age for susceptible humans and use the complement to modify the
#       # infection rate
#       rate_modifier <- 1 - api$get_variable(human, age, susceptible)
#       infected <- susceptible[runif(length(susceptible)) < (pars$age_rate * rate_modifier)]
#       if(length(infected) != 0) api$queue_state_update(human, I, infected)
#     }
#     if (pars$includelocation) {
#       # Get the location for susceptible humans and use the complement to modify the
#       # infection rate
#       rate_modifier <- 1 - api$get_variable(human, location, susceptible)
#       infected <- susceptible[runif(length(susceptible)) < (pars$location_rate * rate_modifier)]
#       if(length(infected) != 0) api$queue_state_update(human, I, infected)
#     }
#   }
# }


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
#' individual_S_to_I_and_I2(S, I, I2, human, immunity, age, location, pars)
#' @importFrom stats runif
individual_S_to_I_and_I2 <- function(S, I, I2, human, immunity, age, location, pars = NULL) {
  function(api) {
    warnings()

    pars <- get_parameters_for_sirstochastic(pars)
    # calculate information for infections, recoveries and births
    inf <- infections(length(api$get_state(human, I)) + length(api$get_state(human, I2)),
                      length(api$get_state(human, S)), pars)

    n_to_infect <- inf$n_infections_S
    susceptible <- api$get_state(human, S)

    if(pars$novariations){
      if(length(susceptible) != 0 && length(susceptible) > n_to_infect)
      {
        infected <- as.integer(round(susceptible[sample.int(length(susceptible), n_to_infect)]))

        infected1 <- as.integer(pars$infection_rate * infected)
        severelyinfected <- as.integer(infected - infected1)

        if(length(infected1) != 0 && infected1 !=0) {
          validated_state_update(api, human, I, infected1, pars$N)
        }

        if(length(severelyinfected) != 0 && severelyinfected !=0){
          validated_state_update(api, human, I2, severelyinfected, pars$N)
        }

      }
    }
    if (pars$includeimmune) {
      # Get the immunity for susceptible humans and use the complement to modify the
      # infection rate
      rate_modifier <- 1 - api$get_variable(human, immunity, susceptible)
      #infected <- as.integer(round(susceptible[sample.int(length(susceptible), (pars$infection_rate * rate_modifier))]))

      infected <- susceptible[runif(length(susceptible)) < (pars$recovery_rate * rate_modifier)]

      if(length(infected) != 0 && infected !=0) {
        infected1 <- as.integer(pars$infection_rate * infected)
        severelyinfected <- as.integer(infected - infected1)
        if(length(infected1) != 0 && infected1 !=0) {
          validated_state_update(api, human, I, infected1, pars$N)
        }

        if(length(severelyinfected) != 0 && severelyinfected !=0){
          validated_state_update(api, human, I2, severelyinfected, pars$N)
        }

      }
    }
    if (pars$includeage) {
      # Get the age for susceptible humans and use the complement to modify the
      # infection rate
      rate_modifier <- 1 - api$get_variable(human, age, susceptible)
      if(length(susceptible) != 0 && susceptible != 0 && !(any(is.na(susceptible)))) {

        infected <- susceptible[runif(length(susceptible)) < (pars$age_rate * rate_modifier)]

        #infected <- as.integer(round(susceptible[sample.int(length(susceptible), (pars$age_rate * rate_modifier))]))

        if(length(infected) != 0 && infected !=0) {

          infected1 <- as.integer(pars$age_rate * infected)
          severelyinfected <- as.integer(infected - infected1)
          if(length(infected1) != 0 && infected1 !=0) {
            validated_state_update(api, human, I, infected1, pars$N)
          }

          if(length(severelyinfected) != 0 && severelyinfected !=0){
            validated_state_update(api, human, I2, severelyinfected, pars$N)
          }
        }
      }

    }
    if (pars$includelocation) {
      # Get the location for susceptible humans and use the complement to modify the
      # infection rate
      rate_modifier <- 1 - api$get_variable(human, location, susceptible)

      infected <- susceptible[runif(length(susceptible)) < (pars$location_rate * rate_modifier)]

      #infected <- as.integer(round(susceptible[sample.int(length(susceptible), (pars$location_rate * rate_modifier))]))

      if(length(infected) != 0 && infected !=0) {

        infected1 <- as.integer(pars$location_rate * infected)
        severelyinfected <- as.integer(infected - infected1)

        if(length(infected1) != 0 && infected1 !=0) {
          validated_state_update(api, human, I, infected1, pars$N)
        }

        if(length(severelyinfected) != 0 && severelyinfected !=0){
          validated_state_update(api, human, I2, severelyinfected, pars$N)
        }
      }
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
#' individual_I_to_R(I, R, human, immunity, age, location, pars)
individual_I_to_R <- function(I, R, human, immunity, age, location, pars = NULL) {
  function(api) {

    pars <- get_parameters_for_sirstochastic(pars)
    rec <- recoveries(length(api$get_state(human, I)), pars)
    n_to_recover <- rec$n_recoveries_I
    infected <- api$get_state(human, I)

    if(pars$novariations){
      recovered <- infected[sample.int(length(infected), n_to_recover)]
      api$queue_state_update(human, R, recovered)
    }
    if (pars$includeage) {
      rate_modifier <- 1 - api$get_variable(human, age, infected)
      recovered <- infected[runif(length(infected)) , 20] #< (pars$age_rate * rate_modifier)]
      api$queue_state_update(human, I, recovered)
    }
    if (pars$includelocation) {
      rate_modifier <- 1 - api$get_variable(human, location, infected)
      recovered <- infected[runif(length(infected)) , 20] #< (pars$location_rate * rate_modifier)]
      api$queue_state_update(human, I, recovered)
    }
  }
}
# individual_I_to_R <- function(I, R, human, immunity, age, location, pars = NULL) {
#   function(api) {
#
#     pars <- get_parameters_for_sirstochastic(pars)
#     rec <- recoveries(length(api$get_state(human, I)), pars)
#     n_to_recover <- rec$n_recoveries_I
#     infected <- api$get_state(human, I)
#
#
#     if (pars$novariations) {
#       recovered <- infected[sample.int(length(infected), n_to_recover)]
#       if(length(recovered) != 0 && recovered != 0) {
#         validated_state_update(api, human, R, recovered, pars$N)
#       }
#
#     }
#     if (pars$includeage) {
#       rate_modifier <- #1 - api$get_variable(human, age, infected)
#       if(length(infected) != 0 && infected != 0 && !(any(is.na(infected))))
# print(length(infected))
#       print(pars$age_rate)
#       print(rate_modifier)
#
#       recovered <- infected[runif(length(infected)) < (pars$age_rate * rate_modifier)]
#
#         #recovered <- as.integer(round(infected[sample.int(length(infected), (pars$age_rate * rate_modifier))]))
#
#         if(length(recovered) != 0 && recovered != 0) {
#           validated_state_update(api, human, R, recovered, pars$N)
#         }
#     }
#
#     if (pars$includelocation) {
#       rate_modifier <- 1 - api$get_variable(human, location, infected)
#       if(length(infected) != 0 && infected != 0 && !(any(is.na(infected)))) {
#         #recovered <- as.integer(round(infected[sample.int(length(infected), (pars$location_rate * rate_modifier))]))
#         recovered <- infected[runif(length(infected)) < (pars$location_rate * rate_modifier)]
#         if(length(recovered) != 0 && recovered != 0) {
#           validated_state_update(api, human, R, recovered, pars$N)
#       }
#     }
#
#     }
#   }
# }

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
#' individual_I_to_R_I2_to_D(I, R, I2, D, human, immunity, age, location, pars)
individual_I_to_R_I2_to_D <- function(I, R, I2, D, human, immunity, age, location, pars = NULL) {
  function(api) {
    warnings()

    pars <- get_parameters_for_sirstochastic(pars)
    rec <- recoveries(length(api$get_state(human, I)) + length(api$get_state(human, I2)), pars)

    n_to_recover <- rec$n_recoveries_I
    infected <- api$get_state(human, I)
    n_to_die <- rec$n_deaths_I
    severelyinfected <- api$get_state(human, I2)

    if (pars$novariations) {
      if(length(infected) != 0 && length(infected) > n_to_recover)
      {
        recovered <- as.integer(round(infected[sample.int(length(infected), n_to_recover)]))

        if(length(recovered) != 0 && recovered != 0) validated_state_update(api, human, R, recovered, pars$N)
      }

      if(length(severelyinfected) != 0 && length(severelyinfected) > n_to_die)
      {
        dead <- severelyinfected[sample.int(length(severelyinfected), n_to_die)]
        if(length(dead) != 0 && dead != 0 && !(any(is.na(dead)))) {
          validated_state_update(api, human, D, dead, pars$N)
        }
      }
    }
    if (pars$includeage) {
      rate_modifier_infected <- 1 - api$get_variable(human, age, infected)
      rate_modifier_severelyinfected <- 1 - api$get_variable(human, age, severelyinfected)

      if(length(infected) != 0 && infected != 0 && !any(is.na(infected))){

        recovered <- as.integer(round(infected[sample.int(length(infected), as.integer(abs(pars$age_rate * rate_modifier_infected)))]))

        if(length(recovered) != 0) {
          validated_state_update(api, human, R, recovered, pars$N)
          #validated_state_update(api, human, I, recovered, pars$N)
        }
      }

      dead <- as.integer(round(severelyinfected[sample.int(length(severelyinfected), as.integer(abs(pars$age_rate * rate_modifier_severelyinfected)))]))

      if(length(dead) != 0 && dead != 0 && !(any(is.na(dead)))) {
        validated_state_update(api, human, D, dead, pars$N)
      }

    }
    if (pars$includelocation) {
      rate_modifier_infected <- 1 - api$get_variable(human, location, infected)
      rate_modifier_severelyinfected <- 1 - api$get_variable(human, location, severelyinfected)

      if(length(infected) != 0 && infected != 0 && !any(is.na(infected))){
        recovered <- as.integer(round(infected[sample.int(length(infected),  as.integer(abs(pars$age_rate * rate_modifier_infected)))])) #, (pars$location_rate * rate_modifier_infected))]))

        if(length(recovered) != 0 && recovered != 0)
        {
          validated_state_update(api, human, R, recovered, pars$N)
          #validated_state_update(api, human, I, recovered, pars$N)
        }
      }

      dead <- as.integer(round(severelyinfected[sample.int(length(severelyinfected), as.integer(abs(pars$location_rate * rate_modifier_severelyinfected)))]))
      if(length(dead) != 0 && dead != 0 && !(any(is.na(dead)))) {
        validated_state_update(api, human, D, dead, pars$N)
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
#' individual_R_to_S(R, S, human, immunity, age, location, pars)
#' @importFrom stats runif
individual_R_to_S <- function(S, R, human, immunity, age, location, pars = NULL) {
  function(api) {

    pars <- get_parameters_for_sirstochastic(pars)
    bir <- births(length(api$get_state(human, R)), pars)
    n_to_susceptible <- bir$n_births
    from_state <- api$get_state(human, R)

    if(pars$novariations){
      if(length(from_state) != 0 && length(from_state) > n_to_susceptible)
      {
        thenewsusceptible <- from_state[sample.int(length(from_state), n_to_susceptible)]
        api$queue_state_update(human, S, thenewsusceptible)
      }
    }
    if (pars$includeimmune) {
      recovered <- from_state[runif(length(from_state)) < pars$recovery_rate]
      api$queue_state_update(human, R, recovered)
      api$queue_variable_update(human, immunity, api$get_parameters()$immunity_level, recovered)
    }
    if (pars$includeage) {
      recovered <- from_state[runif(length(from_state)) < pars$age_rate]
      api$queue_state_update(human, R, recovered)
      api$queue_variable_update(human, age, api$get_parameters()$age_level, recovered)
    }
    if (pars$includelocation) {
      recovered <- from_state[runif(length(from_state)) < pars$recovery_rate]
      api$queue_state_update(human, R, recovered)
      api$queue_variable_update(human, age, api$get_parameters()$location_level, recovered)
    }
  }
}
# individual_R_to_S <- function(R, S, human, immunity, age, location, pars = NULL) {
#   function(api) {
#     warnings()
#     pars <- get_parameters_for_sirstochastic(pars)
#     bir <- births(length(api$get_state(human, R)), pars)
#     n_to_susceptible <- bir$n_births
#     recovered <- api$get_state(human, R)
#
#     if (pars$novariations) {
#       if(length(recovered) != 0 && length(recovered) > n_to_susceptible)
#       {
#         thenewsusceptible <- as.integer(round(recovered[sample.int(length(recovered), n_to_susceptible)]))
#         if(length(thenewsusceptible) != 0) validated_state_update(api, human, S, thenewsusceptible, pars$N)
#       }
#     }
#     if (pars$includeimmune) {
#       rate_modifier <- 1 - api$get_variable(human, immunity, recovered)
#       thenewsusceptible <- recovered[runif(length(recovered)) < (pars$recovery_rate)]
#       #thenewsusceptible <- as.integer(round(recovered[sample.int(length(recovered), (pars$recovery_rate * rate_modifier))]))
#
#       if(length(thenewsusceptible) != 0 && thenewsusceptible != 0 && !any(is.na(thenewsusceptible))) {
#
#         validated_state_update(api, human, S, thenewsusceptible, pars$N)
#         #api$queue_variable_update(human, immunity, api$get_parameters()$immunity_level, thenewsusceptible)
#       }
#
#     }
#     if (pars$includeage) {
#
#       rate_modifier <- 1 - api$get_variable(human, age, recovered)
#       thenewsusceptible <- recovered[runif(length(recovered)) < (pars$age_rate)]
#       #thenewsusceptible <- as.integer(round(recovered[sample.int(length(recovered), (pars$age_rate * rate_modifier))]))
#
#       if(length(thenewsusceptible) != 0 && thenewsusceptible != 0 && !any(is.na(thenewsusceptible))) {
#         validated_state_update(api, human, S, thenewsusceptible, pars$N)
#         #api$queue_variable_update(human, age, api$get_parameters()$immunity_level, thenewsusceptible)
#       }
#     }
#     if (pars$includelocation) {
#
#       rate_modifier <- 1 - api$get_variable(human, location, recovered)
#       thenewsusceptible <- recovered[runif(length(recovered)) < (pars$location_rate)]
#       #thenewsusceptible <- as.integer(round(recovered[sample.int(length(recovered), (pars$location_rate * rate_modifier))]))
#
#       if(length(thenewsusceptible) != 0 && thenewsusceptible != 0 && !any(is.na(thenewsusceptible))) {
#         validated_state_update(api, human, S, thenewsusceptible, pars$N)
#         #api$queue_variable_update(human, age, api$get_parameters()$immunity_level, thenewsusceptible)
#       }
#     }
#   }
# }

#' @title Renders the sizes for S, I, R
#' @param S S
#' @param I I
#' @param R R
#' @param human human
#' @export
#' @examples
#' render_state_sizes(S, I, R, human)
#'
render_state_sizes <- function(S, I, R, human) {
  function(api) {
    api$render('susceptable_counts', length(api$get_state(human, S)))
    api$render('infected_counts', length(api$get_state(human, I)))
    api$render('recovered_counts', length(api$get_state(human, R)))
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
#' render_state_sizes2(S, I, R, I2, D, human)
#'
render_state_sizes2 <- function(S, I, R, I2, D, human) {
  function(api) {
    api$render('susceptable_counts', length(api$get_state(human, S)))
    api$render('infected_counts', length(api$get_state(human, I)))
    api$render('severelyinfected_counts', length(api$get_state(human, I2)))
    api$render('dead_counts', length(api$get_state(human, D)))
    api$render('recovered_counts', length(api$get_state(human, R)))
  }
}

#' @title Check state update is valid
#'
#' @param api api
#' @param i human etc
#' @param state S, I, R etc
#' @param index recovered, susceptable, etc
#' @param population_size population size
#'
#' @export
#'
#' @examples
#' validated_state_update(api, i, state, index, population_size)
validated_state_update <- function(api, i, state, index, population_size) {

  if (any(index > population_size)) {
    stop(paste0('Your index ', index, ' for ', i$name, ':', state$name, ' is greater than the population size \n'))
  }

  if (any(typeof(index) != "integer")) {
    #stop(paste0('Your index ', index, ' for ', i$name, ':', state$name, ' is not an integer type; it is a ', typeof(index),'\n'))

  }
  if (any(index <= 0)) {
    stop(paste0('Your index ', index, ' for ', i$name, ':', state$name, ' is less than or equal to 0 \n'))
  }
  if (any(is.na(index))) {
    stop(paste0('Your index ', index, ' for ', i$name, ':', state$name, ' is not a number \n'))
  }

  api$queue_state_update(i, state, index)
}

