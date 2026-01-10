# multiple simulations
# here I simulate the landscape many times, say 100, and compute the average number of
# playoff appearances, titles, etc. for each team

# load libraries
suppressWarnings(
  library("furrr")
)

# load functions
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


# simulate many times ----------------------------------------------------
multiple_simulations <- function(n_simulations = n_sims,
  initial_tiers,
  sp_rankings,
  rivalry_preferences,
  regions,
  home_field_advantage = 2.5,
  start_year = 1978,
  end_year = 2025){

    # run the many simulations
plan(multisession, workers = 4)
many_sims <- future_map(
  1:n_simulations,
  ~ {
    suppressMessages(
      results_list <- simulate_years(
        initial_tiers,
        sp_rankings,
        rivalry_preferences,
        regions,
        print = FALSE
      )
    )

# championship list
playoff_app <- playoff_app(results_list)

# game-by-game
team_results <- results_list$results |>
pivot_longer(cols = c(team, opponent), values_to = "team") |>
mutate(win = if_else(winner == team, 1, 0)) |> group_by(team, year) |>
summarize(wins = sum(win), games = n(),
    .groups = "keep")

    # tier
    tiers <- results_list$tiers |>
      select(year, team, region, tier) |>
      group_by(team, region) |>
      summarize(
        average_tier = mean(tier),
        .groups = "keep"
      )

    list("playoff_app" = playoff_app, "team_results" = team_results, "tiers" = tiers)
  },
  .progress = TRUE,
  .options = furrr_options(seed = TRUE)
) |>
  transpose() |>
  map(~ bind_rows(.x))

# generate average playoff appearances
playoff_app <- many_sims$playoff_app |>
  group_by(team) |>
  summarize(across(appearances:championships, ~sum(.x)/n_simulations)) |>
  arrange(desc(championships), desc(finals), desc(semis), desc(round_2), desc(appearances))

# average win percentage
win_percentage <- many_sims$team_results |>
  group_by(team) |>
  summarize(
    games = sum(games)/n_simulations,
    wins = sum(wins)/n_simulations) |>
  mutate(
    losses = games - wins,
    win_percentage = wins/games) |>
  left_join(many_sims$team_results |> distinct(team, year) |> group_by(team) |> summarize(seasons = n(), .groups = "keep"), by = join_by(team)) |>
  arrange(desc(win_percentage)) |>
  relocate(team, seasons, win_percentage, games, wins, losses)
  
# average tier placement
tier_placement <- many_sims$tiers |>
  group_by(team) |>
  summarize(
    average_tier = mean(average_tier)
  )

  # return tibble of averages
  win_percentage |>
    left_join(tier_placement, by = join_by(team)) |>
    left_join(playoff_app, by = join_by(team)) |>
    arrange(desc(championships), desc(finals), desc(semis), desc(round_2), desc(appearances)) |>
    relocate(team, seasons, championships, finals, semis, round_2, appearances, average_tier, win_percentage, games, wins, losses)
  }

# example:

n_sims <- 100

average_results <- multiple_simulations(n_sims, initial_tiers, sp_rankings, rivalry_preferences, regions)

average_results |> write_csv("simulation_world/data/average_results.csv")
