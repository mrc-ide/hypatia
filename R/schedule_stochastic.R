schedule_infections <- function(
  api,
  events,
  clinical_infections,
  treated,
  infections
) {
  parameters <- api$get_parameters()
  scheduled_for_infection <- api$get_scheduled(events$infection)
  excluded <- c(scheduled_for_infection, treated)

  to_infect <- setdiff(clinical_infections, excluded)
  all_new_infections <- setdiff(infections, excluded)
  to_infect_asym <- setdiff(all_new_infections, clinical_infections)

  if(length(to_infect) > 0) {
    api$schedule(events$clinical_infection, to_infect, parameters$de)
  }

  if(length(to_infect_asym) > 0) {
    api$schedule(events$asymptomatic_infection, to_infect_asym, parameters$de)
  }

  if(length(all_new_infections) > 0) {
    api$schedule(events$infection, all_new_infections, parameters$de)
  }
}

clear_schedule <- (died) {

  api$clear_schedule(events$infection, died)
  api$clear_schedule(events$asymptomatic_infection, died)


}
