#' @title Continuous age variable
#' @description Create a continuous age variable for the population
#'
#' @param pop population list
#' @param max_age maximum age to be drawn
#'
#' @return continuous age variable
#' @importFrom stats dexp
create_continuous_age_variable <- function(pop, max_age = 100) {

  # get out country median ages
  iso3c <- pop$iso3c[1]
  med_age <- hypatia::iso3c_ages$age[hypatia::iso3c_ages$iso3c == iso3c]

  # get the top end of the 5 year age bins
  age_bins <- c(0, as.numeric(gsub("^(\\d{1,2}).*", "\\1", pop$age_group)[-1]))

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

  sample(unlist(ages))
}

#' @title Discrete age variable
#' @description Create a discrete age variable for each of the
#' length(pop$age_group) distinct age groups
#'
#' @param ages Vector of ages from \code{\link{create_continuous_age_variable}}
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
  disc_ages <- cut(ages, age_bins, include.lowest = TRUE, right = FALSE)
  as.integer(pop$age_group[as.numeric(disc_ages)])
}

#' @title Create age variables
#' @description Create individual variables for continuous and discrete age
#'
#' @param pop population list
#' @param max_age maximum age - default 100
#'
#' @return named list of individual::Variable
create_age_variables <- function(pop, max_age = 100) {

  age <- create_continuous_age_variable(pop, max_age = 100)
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
#' @return named list of individual::Variable
create_variables <- function(pop, max_age = 100) {

  ret <- create_age_variables(pop, max_age = 100)
  list(discrete_age = ret$discrete_age)

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

  # whate are the ages that have been initialised
  iv <- initial_values

  # what ages need to be at the back of our initials for seeding
  ages <- rep(which(parameters$E1_0 > 0), parameters$E1_0[parameters$E1_0 > 0])

  # position of iv to be swapped out
  to_distribute <- tail(seq_along(iv), length(ages))

  # position of iv to be swapped in
  to_swap <- c()
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
