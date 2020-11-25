#' @title Continuous age variable
#' @description Create a continuous age variable for the population
#'
#' @param pop population list
#' @param max_age maximum age to be drawn
#'
#' @return continuous age variable
#' @importFrom stats dexp
#' @noRd
#' @importFrom stats dexp
create_continuous_age_variable <- function(pop, max_age = 100) {

  # get out counntry median ages
  iso3c <- pop$iso3c[1]
  iso3c_ages <- hypatia::iso3c_ages
  med_age <- iso3c_ages$age[iso3c_ages$iso3c == iso3c]

  # get the top end of the 5 year age bins
  age_bins <- get_age_bins(pop$age_group)

  # use these to work out the ages in each bin
  ages_in_bin <- list()
  for (i in seq_along(age_bins)[-1]) {
    ages_in_bin[[i - 1]] <- seq(age_bins[i - 1], age_bins[i] - 1, 1)
  }
  ages_in_bin[[length(ages_in_bin) + 1]] <- seq(max(age_bins), max_age, 1)

  # now sample from these
  ages <- c()
  for (i in seq_len(length(pop$age_group))) {
    ages <- c(
      ages,
      sample(
        ages_in_bin[[i]],
        pop$n[i],
        replace = TRUE,
        prob = dexp(ages_in_bin[[i]], 1 / (med_age * 365))
      )
    )
  }

  sample(ages)
}

#' @title Discrete age variable
#' @description Create a discrete age variable for each of the
#' length(pop$age_group) distinct age groups
#'
#' @param ages Vector of ages from [create_continuous_age_variable]
#' @param pop Vector of integer ages created by
#'
#' @noRd
#' @return discrete age variable
create_discrete_age_variable <- function(ages, pop) {
  # get the top end of the 5 year age bins
  age_bins <- c(get_age_bins(pop$age_group), max(ages))

  # put these into bins
  disc_ages <- cut(ages, age_bins, include.lowest = TRUE, right = FALSE)
  as.integer(pop$age_group[as.numeric(disc_ages)])
}

#' @title Get age bins
#'
#' @param groups a character vector of strings representing the age groups
#'
#' @noRd
#' @return A vector of age boundaries
get_age_bins <- function(groups) {
  c(
    0,
    as.numeric(gsub("^(\\d{1,2}).*", "\\1", groups)[-1])
  )
}

#' @title Create age variables
#' @description Create individual variables for continuous and discrete age
#'
#' @param pop population list
#' @param max_age maximum age - default 100
#'
#' @noRd
#' @return named list of individual::Variable
create_age_variables <- function(pop, max_age = 100) {
  age <- create_continuous_age_variable(pop, max_age)
  discrete_age <- create_discrete_age_variable(age, pop)

  list(
    age = individual::Variable$new("age", age),
    discrete_age = individual::Variable$new("discrete_age", discrete_age)
  )
}

#' @title Create variables
#' @description Create all individual variables for humans
#'
#' @param pop population list
#' @param max_age maximum age - default 100
#'
#' @noRd
#' @return named list of individual::Variable
create_variables <- function(pop, max_age = 100) {
  create_age_variables(pop, max_age)
}
