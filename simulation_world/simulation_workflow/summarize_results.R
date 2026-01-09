# this file provides a way to summarize the last 50 years of results

suppressWarnings({
  library("plotly")
  library("tidyverse")
  theme_set(theme_minimal())
})


# print the cfp champions
cfp_champs <- function(results_list) {
  year_by_year <- results_list$results |>
    filter(str_detect(type, "cfp-F")) |>
    mutate(runner_up = if_else(winner == team, opponent, team)) |>
    select(winner, runner_up, year)

  champs_total <- year_by_year |>
    group_by(winner) |>
    summarize(
      championships = n(),
      most_recent = max(year),
    ) |>
    arrange(desc(championships))

  list("year_by_year" = year_by_year, "champs_total" = champs_total)
}

# print the most playoff appearances
playoff_app <- function(results_list) {
  temp <- results_list$results |>
    filter(str_detect(type, "cfp")) |>
    pivot_longer(
      cols = c(team, opponent),
      names_to = "burn",
      values_to = "team"
    )

  app <- temp |>
    group_by(year) |>
    distinct(team) |>
    group_by(team) |>
    summarize(appearances = n())

  round2 <- temp |>
    filter(type == "cfp-2R") |>
    group_by(team) |>
    summarize(round_2 = n())

  semis <- temp |>
    filter(type == "cfp-SF") |>
    group_by(team) |>
    summarize(semis = n())

  finals <- temp |>
    filter(type == "cfp-F") |>
    group_by(team) |>
    summarize(finals = n())

  champs <- temp |>
    filter(type == "cfp-F", team == winner) |>
    group_by(team) |>
    summarize(championships = n())

  app |>
    left_join(round2, by = join_by(team)) |>
    left_join(semis, by = join_by(team)) |>
    left_join(finals, by = join_by(team)) |>
    left_join(champs, by = join_by(team)) |>
    mutate(across(everything(), ~ replace_na(.x, 0))) |>
    arrange(
      desc(championships),
      desc(finals),
      desc(semis),
      desc(round_2),
      desc(appearances)
    )
}

# print a teams tier journey
tier_journey <- function(results_list, this_team) {
  tiers <- results_list$tiers |> filter(team == this_team)

  record <- results_list$results |>
    pivot_longer(cols = c(team, opponent), values_to = "team") |>
    filter(team == this_team) |>
    group_by(year) |>
    summarize(
      wins = sum(winner == this_team),
      losses = sum(winner != this_team)
    ) |>
    mutate(
      record = str_c(wins, "-", losses)
    ) |>
    select(year, record)

  suppressWarnings(
    p <- tiers |>
      left_join(record, by = join_by(year)) |>
      mutate(record = replace_na(record, "unplayed")) |>
      ggplot(aes(x = year, y = tier)) +
      geom_line() +
      geom_point(aes(text = paste0("Record: ", record)), size = .1)
  )

  ggplotly(p, tooltip = c("x", "y", "text"))
}

# print win percentage against teams
win_percentage <- function(results_list, this_team) {
  team_results <- results_list$results |>
    filter(team == this_team | opponent == this_team) |>
    mutate(
      opponent = if_else(team == this_team, opponent, team),
      win = if_else(winner == this_team, 1, 0),
      location = case_when(
        team != this_team & location == "away" ~ "home",
        team != this_team & location == "home" ~ "away",
        .default = location
      )
    ) |>
    select(year, type, opponent, location, win)

  # results by opponent
  opp <- team_results |>
    group_by(opponent) |>
    summarize(wins = sum(win), losses = sum(win == 0), games = n()) |>
    arrange(desc(games))

  type <- team_results |>
    mutate(
      type = case_when(
        str_detect(type, "relegation") &
          location == "home" ~ "relegation-defend",
        str_detect(type, "relegation") &
          location == "away" ~ "relegation-challenge",
        .default = type
      )
    ) |>
    group_by(type) |>
    summarize(wins = sum(win), losses = sum(win == 0), games = n()) |>
    arrange(desc(games))

  year <- team_results |>
    left_join(
      results_list$tiers |>
        filter(team == this_team) |>
        select(year, region, tier),
      by = join_by(year)
    ) |>
    group_by(year, region, tier) |>
    summarize(
      wins = sum(win),
      losses = sum(win == 0),
      games = n(),
      .groups = "keep"
    ) |>
    ungroup()

  tier <- team_results |>
    left_join(
      results_list$tiers |>
        filter(team == this_team) |>
        select(year, region, tier),
      by = join_by(year)
    ) |>
    group_by(region, tier) |>
    summarize(
      wins = sum(win),
      losses = sum(win == 0),
      games = n(),
      .groups = "keep"
    ) |>
    arrange(tier)

  list(
    "opponents" = opp,
    "game_type" = type,
    "year" = year,
    "tier" = tier
  )
}
