#' @title Continuous age variable
#' @description Create a continuous age variable for the population
#'
#' @param pop population list
#' @param max_age maximum age to be drawn
#'
#' @return continuous age variable
#' @importFrom stats dexp
create_continuous_age_variable <- function(pop, max_age = 100) {
  # get out counntry median ages
  iso3c <- pop$iso3c[1]
  med_age <- iso3c_ages$age[iso3c_ages$iso3c == iso3c]
  
  # get the top end of the 5 year age bins
  age_bins <-
    c(0, as.numeric(gsub("^(\\d{1,2}).*", "\\1", pop$age_group)[-1]))
  
  # use these to work out the ages in each bin
  r <- list()
  for (i in seq_along(age_bins)[-1]) {
    r[[i - 1]] <- seq(age_bins[i - 1], age_bins[i] - 1, 1)
  }
  r[[length(r) + 1]] <- seq(max(age_bins), max_age, 1)
  
  # now sample from these
  ages <- list()
  for (i in seq_len(length(pop$age_group))) {
    ages[[i]] <- sample(r[[i]], pop$n[i], replace = TRUE,
                        prob = dexp(r[[i]], 1 / (med_age * 365)))
  }
  
  ages <- sample(unlist(ages))
  return(ages)
}

#' @title Discrete age variable
#' @description Create a discrete age variable for each of the
#' length(pop$age_group) distinct age groups
#'
#' @inheritParams create_continuous_age_variable
#' @param pop Vector of integer ages created by
#'   \code{\link{create_continuous_age_variable}}
#'
#' @return discrete age variable
create_discrete_age_variable <- function(ages, pop) {
  # get the top end of the 5 year age bins
  age_bins <-
    c(0, as.numeric(gsub("^(\\d{1,2}).*", "\\1", pop$age_group)[-1]))
  age_bins <- c(age_bins, max(ages))
  
  # put these into bins
  disc_ages <-
    cut(ages, age_bins, include.lowest = TRUE, right = FALSE)
  disc_ages <- as.integer(pop$age_group[as.numeric(disc_ages)])
  
  return(disc_ages)
}


create_age_variables <- function(pop) {
  
  age <- create_continuous_age_variable(pop, max_age = 100)
  discrete_age <- create_discrete_age_variable(age, pop)
  
  return(
    list(
      age = individual::Variable$new("age", age),
      discrete_age = individual::Variable$new("discrete_age", discrete_age)
    )
  )
  
}

create_variables <- function(pop) {
  
  variables <- c(
    create_age_variables(pop)
  )
  
}
