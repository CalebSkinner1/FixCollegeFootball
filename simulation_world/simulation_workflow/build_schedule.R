# this file creates a function to build the schedule of a team
suppressWarnings(library("tidyverse"))


`%!in%` <- Negate(`%in%`)

# this function should be iteratively mapped over each team, starting with the top team in tier one
build_schedule <- function(
  this_team,
  tier_tibble,
  rivalry_preferences,
  this_schedule = NULL
) {
  this_region <- tier_tibble |> filter(team == this_team) |> pull(region)
  this_tier <- tier_tibble |> filter(team == this_team) |> pull(tier)
  this_tier_rank <- tier_tibble |> filter(team == this_team) |> pull(prev_rank)

  # inter-region games
  inter_region_games <- tier_tibble |>
    filter(
      region != this_region,
      tier == this_tier,
      prev_rank == this_tier_rank
    ) |>
    pull(team)

  # conference games
  conference_games <- tier_tibble |>
    filter(region == this_region, tier == this_tier, team != this_team) |>
    pull(team)

  # rivalry games
  rivalry_pref <- rivalry_preferences[[this_team]]
  rivalry_pref <- rivalry_pref[rivalry_pref %in% tier_tibble$team] # ensure scheduled teams are in FBS this year
  rivalry_pref <- unique(rivalry_pref)

  if (!is.null(this_schedule)) {
    # teams already selected
    previous_selections <- this_schedule |>
      filter(opponent == this_team) |>
      pull(team)

    # rivalry games already selected
    prev_rivalry <- this_schedule |>
      filter(opponent == this_team, type == "rivalry") |>
      pull()
    # remaining rivalry games left to choose
    open_selections <- 3 - length(prev_rivalry)

    # teams with blocked out schedules
    blocked_teams <- this_schedule |>
      filter(type == "rivalry") |>
      pivot_longer(
        cols = c(team, opponent),
        names_to = "burn",
        values_to = "team"
      ) |>
      group_by(team) |>
      summarize(count = n()) |>
      filter(count >= 3) |>
      pull(team)
  } else {
    open_selections <- 3
    prev_rivalry <- NULL
    blocked_teams <- NULL
    previous_selections <- NULL
  }
  if (open_selections > 0) {
    rivalry_games <- rivalry_pref[
      !rivalry_pref %in%
        c(inter_region_games, conference_games, prev_rivalry, blocked_teams)
    ][c(1:open_selections)]
  } else {
    rivalry_games <- NULL
  }

  # remove previously generated inter-region and conference games
  inter_region_games <- inter_region_games[
    !inter_region_games %in% previous_selections
  ]
  conference_games <- conference_games[
    !conference_games %in% previous_selections
  ]

  # return schedule
  tibble(
    team = this_team,
    opponent = c(inter_region_games, conference_games, rivalry_games),
    type = c(
      rep("inter-region", length(inter_region_games)),
      rep("conference", length(conference_games)),
      rep("rivalry", length(rivalry_games))
    )
  ) |>
    bind_rows(this_schedule)
}

replace_missing_games <- function(
  this_team,
  missing_games_tibble,
  this_schedule
) {
  this_region <- missing_games_tibble |>
    filter(team == this_team) |>
    pull(region)
  this_tier <- missing_games_tibble |> filter(team == this_team) |> pull(tier)

  existing_games <- this_schedule |>
    filter(team == this_team | opponent == this_team) |>
    pivot_longer(cols = c(team, opponent), values_to = "team") |>
    filter(team != this_team) |>
    pull(team)

  needed_games <- 12 - length(existing_games)

  # preference one: same region
  tier_tibblenew_games <- missing_games_tibble |>
    filter(region == this_region, team %!in% c(existing_games, this_team)) |>
    slice(c(1:needed_games)) |>
    pull(team)

  # preference two: leave region, but no duplicate games
  if (length(new_games) < needed_games) {
    needed_games <- needed_games - length(new_games)

    new_games <- missing_games_tibble |>
      filter(team %!in% c(existing_games, this_team, new_games)) |>
      slice(c(1:needed_games)) |>
      pull(team) |>
      c(new_games)
  }

  # preference three: allow duplicate games
  if (length(new_games) < needed_games) {
    needed_games <- needed_games - length(new_games)

    new_games <- missing_games_tibble |>
      filter(team != this_team, team %!in% new_games) |>
      slice(c(1:needed_games)) |>
      pull(team) |>
      c(new_games)
  }

  this_schedule <- tibble(
    team = this_team,
    opponent = new_games,
    type = "makeup"
  ) |>
    bind_rows(this_schedule)

  missing_games_tibble <- missing_games_tibble |>
    mutate(
      count = case_when(
        team == this_team ~ count + length(new_games),
        team %in% new_games ~ count + 1,
        .default = count
      )
    ) |>
    filter(count != 12)

  list(
    "new_schedule" = this_schedule,
    "missing_games_tibble" = missing_games_tibble
  )
}

determine_location <- function(
  this_year_schedule,
  tier_tibble,
  prev_year_schedule
) {
  if (is.null(prev_year_schedule)) {
    # determine if east plays at home against west, etc.
    if (runif(1) > 0.5) {
      EW <- "home"
      EC <- "away"
      WC <- "home"
    } else {
      EW <- "away"
      EC <- "home"
      WC <- "away"
    }
  } else {
    EW <- prev_year_schedule |>
      filter(year == max(prev_year_schedule$year)) |>
      filter(type == "inter-region") |>
      left_join(select(tier_tibble, team, region), by = join_by(team)) |>
      left_join(
        select(tier_tibble, team, region),
        by = join_by(opponent == team)
      ) |>
      filter(region.x == "east", region.y == "west") |>
      slice(1) |>
      pull(location)

    if (EW == "home") {
      EC <- "away"
      WC <- "home"
    } else {
      EC <- "home"
      WC <- "away"
    }
  }

  inter_region <- this_year_schedule |>
    filter(type == "inter-region") |>
    left_join(select(tier_tibble, team, region), by = join_by(team)) |>
    left_join(
      select(tier_tibble, team, region),
      by = join_by(opponent == team)
    ) |>
    mutate(
      location = case_when(
        region.x == "east" & region.y == "west" ~ EW,
        region.x == "east" & region.y == "central" ~ EC,
        region.x == "west" & region.y == "central" ~ WC,
        # reversed below
        region.x == "west" & region.y == "east" ~ EC,
        region.x == "central" & region.y == "east" ~ EW,
        region.x == "central" & region.y == "west" ~ EC,
        .default = NA
      )
    ) |>
    select(team, opponent, location, type)

  conference <- this_year_schedule |>
    filter(type == "conference") |>
    left_join(select(tier_tibble, team, prev_rank), by = join_by(team)) |>
    left_join(
      select(tier_tibble, team, prev_rank),
      by = join_by(opponent == team)
    ) |>
    mutate(
      location = if_else((prev_rank.x - prev_rank.y) %% 2 == 0, "away", "home")
    ) |>
    select(-contains("prev_rank"))

  if (is.null(prev_year_schedule)) {
    rivalry <- this_year_schedule |>
      filter(type %in% c("rivalry", "makeup")) |>
      arrange(desc(team), desc(opponent))

    if (nrow(rivalry) %% 2 == 0) {
      rivalry <- rivalry |>
        mutate(location = rep(c("away", "home"), nrow(rivalry) / 2))
    } else {
      rivalry <- rivalry |>
        mutate(
          location = c(rep(c("away", "home"), (nrow(rivalry) - 1) / 2), "away")
        )
    }
  } else {
    rivalry <- this_year_schedule |>
      filter(type %in% c('makeup', "rivalry")) |>
      rowwise() |>
      mutate(
        matchup = if_else(
          team > opponent,
          str_c(team, "-", opponent),
          str_c(opponent, "-", team)
        )
      ) |>
      ungroup() |>
      select(matchup, type) |>
      left_join(
        prev_year_schedule |>
          filter(location != "neutral") |>
          mutate(
            matchup = if_else(
              team > opponent,
              str_c(team, "-", opponent),
              str_c(opponent, "-", team)
            ),
            location = case_when(
              opponent > team & location == "home" ~ "away",
              opponent > team & location == "away" ~ "home",
              .default = location
            )
          ) |>
          arrange(desc(year)) |>
          select(matchup, location) |>
          distinct(matchup, .keep_all = TRUE),
        by = join_by(matchup)
      ) |>
      separate(col = matchup, into = c("team", "opponent"), sep = "-") |>
      mutate(
        location = if_else(location == "home", "away", "home"),
        location = case_when(
          # if never played before, set as random
          is.na(location) & row_number() %% 2 == 0 ~ "home",
          is.na(location) & row_number() %% 2 == 1 ~ "away",
          .default = location
        )
      )
  }

  bind_rows(inter_region, rivalry, conference)
}

schedule_wrapper <- function(
  tier_tibble,
  rivalry_preferences,
  prev_season_schedule
) {
  updated_schedule <- NULL
  for (team in tier_tibble$team) {
    updated_schedule <- build_schedule(
      team,
      tier_tibble,
      rivalry_preferences,
      this_schedule = updated_schedule
    )
  }
  updated_schedule <- updated_schedule |> drop_na()

  # teams missing games
  missing_games <- updated_schedule |>
    pivot_longer(
      cols = c(team, opponent),
      names_to = "burn",
      values_to = "team"
    ) |>
    group_by(team) |>
    summarize(count = n()) |>
    filter(count != 12) |>
    left_join(tier_tibble, by = join_by(team)) |>
    arrange(tier, prev_rank) # give higher ranked teams preference

  while (nrow(missing_games) > 1) {
    missing_team <- missing_games$team[1]

    list <- replace_missing_games(missing_team, missing_games, updated_schedule)

    updated_schedule <- list$new_schedule
    missing_games <- list$missing_games_tibble
  }

  schedule <- determine_location(
    updated_schedule,
    tier_tibble,
    prev_season_schedule
  )

  return(schedule)
}

# example:
# tier_tibble <- read_csv("simulation_world/initial_tiers.csv")
# rivalry_preferences <- readRDS(
#   file = "simulation_world/rivalry_preferences.rds"
# )

# updated_schedule <- schedule_wrapper(tier_tibble, rivalry_preferences)

# updated_schedule |>
#   pivot_longer(
#     cols = c(team, opponent),
#     names_to = "burn",
#     values_to = "team"
#   ) |>
#   group_by(team) |>
#   summarize(count = n()) |>
#   filter(count != 12)
