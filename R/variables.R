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
#' @param pop population list
#' @param parameters model parameters
#' @noRd
#' @return named list of individual::Variable
create_age_variables <- function(pop, parameters) {

  cont_age <- create_continuous_age_variable(pop, parameters$max_age)

  discrete_age <- create_discrete_age_variable(cont_age, pop)

  swaps <- identify_ages_to_adjust(discrete_age, parameters)

  discrete_age <- doing_the_swap(swaps, discrete_age)

  cont_age <- doing_the_swap(swaps, cont_age)

  list(
    age = individual::Variable$new("age", cont_age),
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

#' @title Identify ages to adjust
#'
#' @details Identifies age variables to be switched based on parameters object
#' @param discrete_ages discrete ages
#' @param initial_values Vector of inital values from with variables object
#'   created by \code{\link{create_variables}}
#' @param parameters Parameters object created by \code{\link{get_parameters}}
#' @return Returns values to swap
#' @importFrom utils head tail
#'
#' @noRd
identify_ages_to_adjust <- function(discrete_ages, parameters) {

  # what are the ages that have been initialised
  iv <- discrete_ages

  e1 <- parameters$E1_0

  # what ages need to be at the back of our initials for seeding
  ages <- rep(which(e1 > 0), e1[e1 > 0])

  # # position of iv to be swapped out
  to_distribute <- tail(seq_along(iv), length(ages))

  # position of iv to be swapped in
  to_swap <- vector()

  for (i in seq_along(unique(ages))) {
    unique_age <- unique(ages)[i]
    tsi1 <- which(iv == unique_age)
    tsi2 <- head(tsi1, sum(ages == unique_age))
    to_swap <- c(to_swap, tsi2)
  }

  list(to_distribute = to_distribute, to_swap = to_swap)

}

#' @title Swap values that have been identified
#'
#' @param swaps values to be swapped
#' @param age discrete age or continuous age
#'
#' @return age
doing_the_swap <- function(swaps, age) {

  # what values are being moved around
  to_distribute_values <- age[swaps$to_distribute]
  to_swap_values <- age[swaps$to_swap]

  # do the swap
  age[swaps$to_distribute] <- to_swap_values
  age[swaps$to_swap] <- to_distribute_values

  return(age)

}
