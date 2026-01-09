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
n_sims <- 4

plan(multisession, workers = 4)
many_sims <- future_map(
  1:n_sims,
  ~ {
    suppressMessages(
      results_list <- simulate_years(
        initial_tiers,
        sp_rankings,
        rivalry_preferences,
        regions
      )
    )

    # championship list
    playoff_app <- playoff_app(results_list)

    # game-by-game
    results <- results_list$results

    list("playoff_app" = playoff_app, "results" = results)
  },
  .progress = TRUE,
  .options = furrr_options(seed = TRUE)
) |>
  transpose() |>
  map(~ bind_rows(.x))

map(many_sims, ~ bind_rows(.x))
