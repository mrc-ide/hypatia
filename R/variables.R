#' @title Continuous age variable
#' @description Create a continuous age variable for the population
#'
#' @param pop population list
#' @param max_age maximum age to be drawn
#'
#' @importFrom stats dexp
#' @return continuous age variable
#' @noRd
create_continuous_age_variable <- function(pop, max_age = 100) {

  # get out country median ages
  iso3c <- pop$iso3c[1]
  med_age <- hypatia::iso3c_ages$age[hypatia::iso3c_ages$iso3c == iso3c]

  # get the top end of the 5 year age bins
  age_bins <- get_age_bins(pop$age_group)

  # use these to work out the ages in each bin
  ages_in_bin <- list()
  for (i in seq_along(age_bins)[-1]) {
    ages_in_bin[[i - 1]] <- seq(age_bins[i - 1], age_bins[i] - 1, 1)
  }
  ages_in_bin[[length(ages_in_bin) + 1]] <- seq(max(age_bins), max_age, 1)

  # now sample from these
  ages <- NULL
  for (i in seq_along(pop$age_group)) {
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
#' @param pop Vector of integer ages
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
#'
#'
#'
#' @noRd
#' @return named list of individual::Variable
create_age_variables <- function(pop, parameters) {
  age <- adjust_seeding_ages(
    create_continuous_age_variable(pop, parameters$max_age),
    parameters
  )
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
#' @param parameters model parameters
#'
#' @return named list of individual::Variable
#' @noRd
create_variables <- function(pop, parameters) {
  create_age_variables(pop, parameters)
}

#' Adjust seeding ages
#'
#' @details Switches age variables based on parameters object
#' @param initial_values Vector of inital values from with variables object
#'   created by \code{\link{create_variables}}
#' @param parameters Parameters object created by \code{\link{get_parameters}}
#' @return Returns modified initial_values vector
#' @importFrom utils head tail
#'
#' @noRd
#' @examples
#' \dontrun{
#' Create our parameters
#' pop <- squire::get_population(iso3c = "ATG")
#' pop$n <- as.integer(pop$n)/100
#' parameters <- get_parameters(
#'    population = pop$n, contact_matrix_set = squire::contact_matrices[1]
#' )
#'
#' # Create our variables
#' variables <- create_variables(pop)
#'
#' # adjust the seeding ages
#' variables$discrete_age$initial_values <- adjust_seeding_ages(
#' initial_values = variables$discrete_age$initial_values,
#'   parameters = parameters
#' )
#' }

adjust_seeding_ages <- function(initial_values, parameters) {

  # what are the ages that have been initialised
  iv <- initial_values

  # what ages need to be at the back of our initials for seeding
  ages <- rep(which(parameters$E1_0 > 0),
              parameters$E1_0[parameters$E1_0 > 0])

  # position of iv to be swapped out
  to_distribute <- tail(seq_along(iv), length(ages))

  # position of iv to be swapped in
  to_swap <- vector()
  for (i in seq_along(unique(ages))) {

    tsi <- which(iv == unique(ages)[i])
    tsi <- head(tsi, sum(ages == unique(ages)[i]))
    to_swap <- c(to_swap, tsi)

  }

  # what values are being moved around
  to_distribute_values <- iv[to_distribute]
  to_swap_values <- iv[to_swap]

  # do the swap
  iv[to_distribute] <- to_swap_values
  iv[to_swap] <- to_distribute_values

  return(iv)

}
