# map season over season to simulate an entire era of college football

source("simulation_world/simulation_workflow/simulate_season.R")
source("simulation_world/simulation_workflow/reset_tiers.R")
source("simulation_world/simulation_workflow/build_schedule.R")

# simulate multiple years
simulate_years <- function(
  initial_tiers,
  sp_rankings,
  rivalry_preferences,
  regions,
  home_field_advantage = 2.5,
  start_year = 1978,
  end_year = 2025
) {
  all_results <- tibble()
  this_year <- start_year
  tier_tibble <- initial_tiers
  tier_years <- tier_tibble |> mutate(year = this_year)
  prev_schedules <- NULL
  while (this_year <= end_year) {
    print(str_c("simulating year ", this_year))
    year_list <- simulate_year(
      tier_tibble,
      sp_rankings,
      rivalry_preferences,
      regions,
      home_field_advantage,
      prev_schedules,
      this_year
    )
    tier_tibble <- year_list$new_tiers
    all_results <- bind_rows(
      year_list$results |> mutate(year = this_year),
      all_results
    )
    tier_years <- bind_rows(
      tier_tibble |> mutate(year = this_year + 1),
      tier_years
    )
    prev_schedules <- all_results
    this_year <- this_year + 1
  }

  list("results" = all_results, "tiers" = tier_years)
}

# simulate an entire year, take in a tier, generate the schedule, return the results and new tier
simulate_year <- function(
  tier_tibble,
  sp_rankings,
  rivalry_preferences,
  regions,
  home_field_advantage,
  prev_schedules,
  this_year
) {
  sp <- sp_rankings |> filter(year == this_year) |> select(team, rating)

  sp_next <- sp_rankings |>
    filter(year == this_year + 1) |>
    select(team, rating)

  if (nrow(sp_next) == 0) {
    # if this is the last year of sp+, retain this year (basically assume no players are added or leave FBS)
    sp_next <- sp
  }

  # generate schedule
  schedule <- schedule_wrapper(
    tier_tibble,
    rivalry_preferences,
    prev_season_schedule = prev_schedules
  )

  # simulate the season
  season_list <- simulate_season(
    schedule,
    tier_tibble,
    sp,
    home_field_advantage
  )

  season_list$standings |> filter(wins < 0)

  # reset tiers
  new_tiers <- reset_tiers(
    tier_tibble,
    season_list$standings,
    season_list$results,
    sp_next,
    regions
  )

  list(
    "results" = season_list$results,
    "new_tiers" = new_tiers
  )
}
