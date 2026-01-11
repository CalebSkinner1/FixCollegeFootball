# this file shows how a simulation could run

# load functions ---------------------------------------------------------

source("simulation_world/simulation_workflow/season_over_season.R")
source("simulation_world/simulation_workflow/summarize_results.R")

# load data --------------------------------------------------------------
regions <- read_csv("simulation_world/data/regions.csv", show_col_types = FALSE)

initial_tiers <- read_csv(
  "simulation_world/data/initial_tiers.csv",
  show_col_types = FALSE
)

rivalry_preferences <- readRDS(
  file = "simulation_world/data/rivalry_preferences.rds"
)

sp_rankings <- read_csv(
  "simulation_world/data/sp_rankings.csv",
  show_col_types = FALSE
) |>
  select(year, team, rating)


# simulate seasons -------------------------------------------------------

set.seed(123)
# simulate every season - may take a few moments
results_list <- simulate_years(
  initial_tiers,
  sp_rankings,
  rivalry_preferences,
  regions
)

saveRDS(
  results_list,
  file = "simulation_world/data/example_results.rds"
)


# print cfp champions
cfp_champs(results_list)$year_by_year |> print(n = 48)
cfp_champs(results_list)$champs_total |> print(n = 30)

# print teams by playoff appearances
playoff_app(results_list) |> print(n = 50)

# print tier journey for Baylor and Oklahoma State
tier_journey(results_list, "Baylor")
tier_journey(results_list, "Oklahoma State")

# print win percentage against teams
win_percentage(results_list, "Baylor")
win_percentage(results_list, "Oklahoma State")
