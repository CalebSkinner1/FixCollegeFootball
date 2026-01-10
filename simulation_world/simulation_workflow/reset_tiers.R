# reset tiers using results from previous season
`%!in%` <- Negate(`%in%`)

reset_tiers <- function(
  prev_year_tier_tibble,
  final_standings,
  results,
  next_sp_rankings,
  regions
) {
  # relegation games
  relegation_games <- results |>
    filter(str_detect(type, "relegation")) |>
    separate(type, sep = "-", into = c("region", "tier", "rank")) |>
    mutate(
      rank = as.double(str_remove(rank, " relegation")),
      tier = as.double(tier)
    )

  relegation_winners <- relegation_games |>
    select(winner, tier, region, rank) |>
    rename(team = winner)

  relegation_losers <- relegation_games |>
    mutate(
      team = if_else(winner == team, opponent, team),
      rank = 9 - rank,
      tier = tier + 1
    ) |>
    select(team, region, tier, rank)

  playoff_teams <- filter(results, str_detect(type, "cfp")) |>
    pivot_longer(cols = c(team, opponent), values_to = "team") |>
    distinct(team) |>
    pull(team)

  locked_teams <- c(
    final_standings |> filter(tier == 1, rank <= 4) |> pull(team),
    playoff_teams
  ) |>
    unique()

  wild_cards <- final_standings |>
    filter(
      !(tier == 1 & rank <= 4),
      team %in% locked_teams
    ) |>
    pull(team)

  new_tiers <- tibble()

  for (t in seq_len(max(final_standings$tier))) {
    reset_tier_list <- reset_this_tier(
      locked_teams = locked_teams,
      accelerated_teams = wild_cards,
      playoff_teams,
      standings = final_standings,
      relegation_winners,
      relegation_losers,
      this_tier = t
    )
    locked_teams <- reset_tier_list$auto_drop
    new_tiers <- bind_rows(new_tiers, reset_tier_list$tier)
  }

  # now, adjust tiers for new teams!
  new_teams <- next_sp_rankings |>
    filter(team %!in% new_tiers$team) |>
    pull(team)

  leaving_teams <- new_tiers |>
    filter(team %!in% next_sp_rankings$team) |>
    pull(team)

  # adjusts tiers for teams that may have left or entered the FBS
  team_movement_adjustment(new_tiers, new_teams, leaving_teams, regions)
}

# account for team movement year to year
team_movement_adjustment <- function(
  end_of_season_tiers,
  new_teams,
  leaving_teams,
  regions
) {
  bind_rows(
    regions |> filter(team %in% new_teams) |> mutate(tier = 10, prev_rank = 9),
    end_of_season_tiers |> filter(team %!in% leaving_teams)
  ) |>
    mutate(points = (10 - tier) * 8 + (9 - prev_rank)) |>
    group_by(region) |>
    mutate(
      rank = rank(-points, ties.method = "random"),
      tier = ((rank - 1) %/% 8) + 1
    ) |>
    group_by(region, tier) |>
    mutate(prev_rank = rank(-points, ties.method = "random")) |>
    select(team, region, tier, prev_rank) |>
    arrange(tier, region, prev_rank) |>
    ungroup()
}

reset_this_tier <- function(
  locked_teams, # teams that are guaranteed to be in this tier
  accelerated_teams = NULL, # teams that already jumped up
  playoff_teams,
  standings,
  relegation_winners,
  relegation_losers,
  this_tier
) {
  # spots that are locked up in this tier
  tier_locks <- bind_rows(
    standings |> filter(tier == this_tier, rank <= 4),
    standings |> filter(team %in% locked_teams)
  ) |>
    distinct() |>
    group_by(region) |>
    summarize(locks = n()) |>
    group_by(region) |>
    group_split()

  # spots vacated by teams accelerating up (usually by making cfp)
  vacated_spots <- standings |>
    filter(team %in% accelerated_teams, tier == this_tier, tier != 1) |>
    group_by(region) |>
    summarize(empty = n())

  # update tier locks with vacated spots
  tier_locks <- map(
    tier_locks,
    ~ {
      this_region <- .x$region[1]

      empty <- vacated_spots |> filter(region == this_region) |> pull(empty)
      if (length(empty) == 0) {
        empty <- 0
      }

      tibble(region = this_region, locks = .x$locks[1] - empty)
    }
  )

  # teams that are automatically dropped to next tier
  auto_drop <- map_dfr(
    tier_locks,
    ~ {
      this_region <- .x$region[1]
      locks <- .x$locks[1]

      # add teams that aren't moving in, but also can't drop
      protected_teams <- standings |>
        filter(
          region == this_region,
          team %in% accelerated_teams,
          tier == this_tier,
          tier == 1
        ) |>
        nrow()

      standings |>
        filter(region == this_region, team %!in% locked_teams, tier == this_tier) |>
        slice_tail(n = (locks - 4 - protected_teams))
    }
  ) |>
    pull(team)

  idle_teams <- standings |>
    filter(
      tier == this_tier,
      team %!in% c(locked_teams, auto_drop, playoff_teams),
      team %!in% relegation_winners$team,
      team %!in% relegation_losers$team
    ) |>
    pull(team)

  # add locked teams
  add_locked <- standings |>
    filter(team %in% c(locked_teams, idle_teams)) |>
    select(team, tier, region, rank) |>
    mutate(value = if_else(tier == this_tier, rank, 10)) |>
    group_by(region) |>
    mutate(
      rank = rank(value, ties.method = "random")
    ) |>
    select(-value) |>
    ungroup()

  add_winners <- relegation_winners |> filter(tier == this_tier)
  add_losers <- relegation_losers |> filter(tier == this_tier)

  last_year_points <- standings |>
    mutate(points = (8 - tier) * 8 + (9 - rank)) |>
    select(team, points)

  tier <- bind_rows(add_locked, add_winners, add_losers) |>
    arrange(region) |>
    mutate(
      tier = this_tier
    ) |>
    left_join(last_year_points, by = join_by(team)) |>
    group_by(region) |>
    mutate(rank = rank(-points)) |>
    arrange(region, rank) |>
    ungroup() |>
    rename(prev_rank = rank) |>
    select(team, tier, region, prev_rank)

  list("auto_drop" = auto_drop, "tier" = tier)
}
