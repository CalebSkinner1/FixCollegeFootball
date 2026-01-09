# simulate a year using sp+ rankings
suppressWarnings(
  library("tidyverse")
)

`%!in%` <- Negate(`%in%`)

simulate_season <- function(
  schedule,
  tier_tibble,
  sp_rankings,
  home_field_advantage
) {
  schedule_ratings <- schedule |>
    left_join(
      rename(sp_rankings, "team_rating" = rating),
      by = join_by(team)
    ) |>
    left_join(
      rename(sp_rankings, "opp_rating" = rating),
      by = join_by(opponent == team)
    ) |>
    mutate(
      location_adv = case_when(
        location == "home" ~ home_field_advantage,
        location == "away" ~ -home_field_advantage,
        .default = 0
      )
    )
  results <- schedule_ratings |>
    mutate(
      rating_diff = team_rating - opp_rating + location_adv,
      z_score = rating_diff / 17,
      winner = if_else(rnorm(n(), 0, 1) < z_score, team, opponent)
    ) |>
    select(team, opponent, location, type, winner)

  conference_results <- results |>
    pivot_longer(
      cols = c(team, opponent),
      names_to = "burn",
      values_to = "team"
    ) |>
    filter(type == "conference") |>
    select(team, winner) |>
    left_join(select(tier_tibble, c(team, tier, region)), by = join_by(team)) |>
    group_by(tier, region, team) |>
    summarize(
      wins = sum(team == winner),
      losses = sum(team != winner),
      .groups = "keep"
    ) |>
    arrange(tier, region, desc(wins)) |>
    group_by(tier, region) |>
    group_split()

  # compute standings for each region
  standings <- map(conference_results, ~ determine_standings(results, .x))

  # compute SOR for each team
  sor <- compute_sor(results, sp_rankings, home_field_advantage)

  # postseason
  postseason_matchups <- generate_postseason_matchups(
    standings,
    sor,
    tier_tibble
  )

  postseason_results <- simulate_postseason(
    postseason_matchups,
    sp_rankings,
    home_field_advantage
  )

  return(list(
    "standings" = bind_rows(standings),
    "results" = bind_rows(results, postseason_results)
  ))
}

simulate_postseason <- function(
  postseason_schedule,
  sp_rankings,
  home_field_advantage
) {
  # playoff_results
  playoff_results <- simulate_playoffs(
    postseason_schedule$playoffs,
    sp_rankings,
    home_field_advantage
  )

  # relegation results
  relegation_games <- postseason_schedule$relegation |>
    left_join(sp_rankings, by = join_by(team)) |>
    left_join(
      rename(sp_rankings, challenger_rating = rating),
      by = join_by(challenger == team)
    ) |>
    mutate(location = "home") |>
    mutate(
      location_adv = case_when(
        location == "home" ~ home_field_advantage,
        location == "away" ~ -home_field_advantage,
        .default = 0
      )
    )

  relegation_results <- relegation_games |>
    mutate(
      rating_diff = rating - challenger_rating + location_adv,
      z_score = rating_diff / 17,
      winner = if_else(rnorm(n(), 0, 1) < z_score, team, challenger),
      type = str_c(region, "-", tier, "-", spot, " relegation")
    ) |>
    select(team, challenger, location, type, winner) |>
    rename(opponent = challenger)

  consolation_results <- postseason_schedule$consolation |>
    mutate(location = "neutral") |>
    left_join(sp_rankings, by = join_by(team1 == team)) |>
    left_join(
      rename(sp_rankings, rating2 = rating),
      by = join_by(team2 == team)
    ) |>
    mutate(
      rating_diff = rating - rating2,
      z_score = rating_diff / 17,
      winner = if_else(rnorm(n(), 0, 1) < z_score, team1, team2),
      location = "neutral",
      type = "consolation"
    ) |>
    select(team1, team2, location, type, winner) |>
    rename(team = team1) |>
    rename(opponent = team2)

  bind_rows(playoff_results, relegation_results, consolation_results)
}

simulate_playoffs <- function(
  playoff_schedule,
  sp_rankings,
  home_field_advantage
) {
  # first round
  first_round <- tibble(
    seed = c(5:8),
    team = slice(playoff_schedule, c(5:8)) |> pull(team),
    opp_seed = c(12:9),
    opponent = slice(playoff_schedule, c(12:9)) |> pull(team),
    location = "home"
  ) |>
    left_join(sp_rankings, by = join_by(team)) |>
    left_join(
      rename(sp_rankings, opp_rating = rating),
      by = join_by(opponent == team)
    ) |>
    mutate(
      location_adv = case_when(
        location == "home" ~ home_field_advantage,
        location == "away" ~ -home_field_advantage,
        .default = 0
      )
    )

  first_round_results <- first_round |>
    mutate(
      rating_diff = rating - opp_rating + location_adv,
      z_score = rating_diff / 17,
      winner = if_else(rnorm(n(), 0, 1) < z_score, team, opponent),
      winner_seed = if_else(team == winner, seed, opp_seed),
      type = "cfp-1R"
    )

  # second round
  second_round <- tibble(
    seed = c(1:4),
    team = slice(playoff_schedule, c(1:4)) |> pull(team),
    opp_seed = slice(first_round_results, c(4:1)) |> pull(winner_seed),
    opponent = slice(first_round_results, c(4:1)) |> pull(winner),
    location = "home"
  ) |>
    left_join(sp_rankings, by = join_by(team)) |>
    left_join(
      rename(sp_rankings, opp_rating = rating),
      by = join_by(opponent == team)
    ) |>
    mutate(
      location_adv = case_when(
        location == "home" ~ home_field_advantage,
        location == "away" ~ -home_field_advantage,
        .default = 0
      )
    )

  second_round_results <- second_round |>
    mutate(
      rating_diff = rating - opp_rating + location_adv,
      z_score = rating_diff / 17,
      winner = if_else(rnorm(n(), 0, 1) < z_score, team, opponent),
      winner_seed = if_else(team == winner, seed, opp_seed),
      type = "cfp-2R"
    )

  # semifinals
  semifinals <- tibble(
    seed = slice(second_round_results, c(1:2)) |> pull(winner_seed),
    team = slice(second_round_results, c(1:2)) |> pull(winner),
    opp_seed = slice(second_round_results, c(4:3)) |> pull(winner_seed),
    opponent = slice(second_round_results, c(4:3)) |> pull(winner),
    location = "neutral"
  ) |>
    left_join(sp_rankings, by = join_by(team)) |>
    left_join(
      rename(sp_rankings, opp_rating = rating),
      by = join_by(opponent == team)
    )

  semifinals_results <- semifinals |>
    mutate(
      rating_diff = rating - opp_rating,
      z_score = rating_diff / 17,
      winner = if_else(rnorm(n(), 0, 1) < z_score, team, opponent),
      winner_seed = if_else(team == winner, seed, opp_seed),
      type = "cfp-SF"
    )

  # championship
  championship <- tibble(
    seed = slice(semifinals_results, 1) |> pull(winner_seed),
    team = slice(semifinals_results, 1) |> pull(winner),
    opp_seed = slice(semifinals_results, 2) |> pull(winner_seed),
    opponent = slice(semifinals_results, 2) |> pull(winner),
    location = "neutral"
  ) |>
    left_join(sp_rankings, by = join_by(team)) |>
    left_join(
      rename(sp_rankings, opp_rating = rating),
      by = join_by(opponent == team)
    )

  championship_results <- championship |>
    mutate(
      rating_diff = rating - opp_rating,
      z_score = rating_diff / 17,
      winner = if_else(rnorm(n(), 0, 1) < z_score, team, opponent),
      winner_seed = if_else(team == winner, seed, opp_seed),
      type = "cfp-F"
    )

  bind_rows(
    first_round_results,
    second_round_results,
    semifinals_results,
    championship_results
  ) |>
    select(team, opponent, location, type, winner)
}

generate_postseason_matchups <- function(
  standings,
  sor,
  tier_tibble
) {
  all_standings <- bind_rows(standings)
  # find playoff matchup
  auto_bids <- all_standings |>
    filter(tier == 1, rank <= 3) |>
    pull(team)

  wild_cards <- sor |>
    filter(team %!in% auto_bids) |>
    arrange(desc(sor)) |>
    slice(1:3) |>
    pull(team)

  playoff_teams <- c(auto_bids, wild_cards)

  playoff_schedule <- sor |>
    filter(team %in% playoff_teams) |>
    mutate(rank = rank(-sor))

  # relegation games schedule:
  relegation_games <- relegation_games_wrapper(
    wild_cards,
    playoff_teams,
    all_standings
  )

  # consolation games
  eligible_teams_tibble <- all_standings |>
    filter(
      team %!in% playoff_schedule$team,
      team %!in% relegation_games$team,
      team %!in% relegation_games$challenger
    )

  consolation_games <- tibble()
  while (nrow(eligible_teams_tibble) > 1) {
    consolation_list <- consolation_selecter(
      eligible_teams_tibble$team[1],
      eligible_teams_tibble
    )
    eligible_teams_tibble <- consolation_list$eligible_tibble
    consolation_games <- bind_rows(
      consolation_games,
      consolation_list$matchup_tibble
    )
  }

  return(list(
    "playoffs" = playoff_schedule,
    "relegation" = relegation_games,
    "consolation" = consolation_games
  ))
}

consolation_selecter <- function(this_team, eligible_tibble) {
  this_region <- eligible_tibble |> filter(team == this_team) |> pull(region)
  this_tier <- eligible_tibble |> filter(team == this_team) |> pull(tier)
  opponent <- eligible_tibble |>
    filter(team != this_team) |>
    filter(!(tier == this_tier & region == this_region)) |>
    arrange(tier, rank) |>
    slice(1) |>
    pull(team)
  if (length(opponent) == 0) {
    matchup_tibble <- tibble()
    eligible_tibble <- tibble()
  } else {
    matchup_tibble <- tibble(team1 = this_team, team2 = opponent)
    eligible_tibble <- eligible_tibble |>
      filter(team %!in% c(this_team, opponent))
  }

  list("matchup_tibble" = matchup_tibble, "eligible_tibble" = eligible_tibble)
}

relegation_games_wrapper <- function(wild_cards, playoff_teams, all_standings) {
  locks <- wild_cards
  relegation_games <- tibble()
  for (t in seq_len(max(all_standings$tier) - 1)) {
    tier_list <- compute_tier_relegation_games(
      locked_teams = locks,
      accelerated_teams = wild_cards,
      playoff_teams = playoff_teams,
      all_standings = all_standings,
      this_tier = t
    )
    relegation_games <- bind_rows(relegation_games, tier_list$relegation)
    locks <- tier_list$auto_drop
  }

  relegation_games
}

compute_tier_relegation_games <- function(
  locked_teams, # teams that are guaranteed to be in this tier
  accelerated_teams = NULL, # teams that already jumped up
  playoff_teams,
  all_standings,
  this_tier
) {
  # spots that are locked up in this tier
  tier_locks <- bind_rows(
    all_standings |> filter(tier == this_tier, rank <= 4),
    all_standings |> filter(team %in% locked_teams)
  ) |>
    group_by(region) |>
    summarize(locks = n()) |>
    group_by(region) |>
    group_split()

  # spots vacated by teams accelerating up (usually by making cfp)
  vacated_spots <- all_standings |>
    filter(team %in% accelerated_teams, tier == this_tier, tier != 1) |>
    group_by(region) |>
    summarize(empty = n())

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
      protected_teams <- all_standings |>
        filter(
          region == this_region,
          team %in% accelerated_teams,
          tier == this_tier,
          tier == 1
        ) |>
        nrow()

      all_standings |>
        filter(
          region == this_region,
          tier == this_tier,
          rank > 12 - locks + protected_teams
        )
    }
  ) |>
    pull(team)

  # compute tier relegation games
  relegation_games <- map_dfr(
    tier_locks,
    ~ {
      this_region <- .x$region[1]
      locks <- .x$locks[1]

      if (
        all_standings |>
          filter(region == this_region, tier == this_tier + 1) |>
          nrow() ==
          0
      ) {
        tibble()
      } else {
        lower_tier <- all_standings |>
          filter(
            region == this_region,
            tier == this_tier + 1,
            team %!in% playoff_teams
          ) |>
          slice_head(n = 8 - locks) |>
          arrange(-rank)

        upper_tier <- all_standings |>
          filter(
            region == this_region,
            tier == this_tier,
            team %!in% auto_drop,
            team %!in% playoff_teams
          ) |>
          slice_tail(n = 8 - locks) |>
          slice_tail(n = nrow(lower_tier))

        tibble(
          team = upper_tier$team,
          challenger = lower_tier$team,
          region = this_region,
          tier = this_tier,
          spot = seq(from = 9 - nrow(upper_tier), to = 8, by = 1)
        )
      }
    }
  )

  list("auto_drop" = auto_drop, "relegation" = relegation_games)
}

compute_sor <- function(results, sp_rankings, home_field_advantage) {
  team_results <- results |>
    mutate(team2 = team, opponent2 = opponent) |>
    pivot_longer(
      cols = c(team, opponent),
      names_to = "burn",
      values_to = "team"
    ) |>
    mutate(
      opponent = if_else(burn == "team", opponent2, team2),
      location = case_when(
        burn != "team" & location == "home" ~ "away",
        burn != "team" & location == "away" ~ "home",
        .default = location
      ),
      outcome = if_else(team == winner, "win", "loss")
    ) |>
    select(team, opponent, location, type, outcome)

  top_12_team <- sp_rankings |> slice(12) |> pull(rating)

  # compute probability that a typical top 12 team would win
  team_results |>
    left_join(sp_rankings, by = join_by(opponent == team)) |>
    mutate(
      location_adv = case_when(
        location == "home" ~ home_field_advantage,
        location == "away" ~ -home_field_advantage,
        .default = 0
      ),
      rating_diff = top_12_team - rating + location_adv,
      t12_win_prob = pnorm(rating_diff / 17, mean = 0, sd = 1),
      loss_quality = if_else(outcome == "loss", -t12_win_prob, 0),
      win_quality = if_else(outcome == "win", 1 - t12_win_prob, 0)
    ) |>
    # aggregate by team
    group_by(team) |>
    summarize(
      win_quality = sum(win_quality),
      loss_quality = sum(loss_quality),
      wins = sum(outcome == "win"),
      losses = sum(outcome == "loss")
    ) |>
    mutate(sor = win_quality + loss_quality) |>
    arrange(desc(sor))
}

determine_standings <- function(results, conference_standings) {
  conference_teams <- conference_standings$team

  games <- results |> filter(winner %in% conference_teams, type == "conference")

  ties_remaining <- TRUE
  while (ties_remaining) {
    ties <- find_ties(conference_standings)
    if (length(ties) == 0) {
      ties_remaining <- FALSE
    } else {
      conference_standings <- resolve_ties(
        ties[[1]],
        conference_standings,
        games,
        sp_rankings
      )
    }
  }
  conference_standings |>
    mutate(
      rank = rank(-wins),
      wins = floor(wins)
    )
}

find_ties <- function(conference_standings) {
  conference_standings |>
    mutate(
      rank = rank(-wins, ties.method = "max")
    ) |>
    group_by(wins) |>
    mutate(
      tie = if_else(n() > 1, "tie", NA)
    ) |>
    ungroup() |>
    filter(!is.na(tie)) |>
    group_by(rank) |>
    group_split() |>
    map(~ .x |> pull(team))
}

resolve_ties <- function(tied_teams, conference_standings, games, sp_rankings) {
  order <- NULL
  if (length(tied_teams) == 2) {
    # if only two teams are tied, resolve by h2h
    leader <- games |>
      filter(team %in% tied_teams & opponent %in% tied_teams) |>
      pull(winner)

    order <- c(leader, tied_teams[tied_teams != leader])
  } else {
    # if more than two teams are tied, find record amongst each other
    tiebreaker <- games |>
      filter(team %in% tied_teams & opponent %in% tied_teams) |>
      group_by(winner) |>
      summarize(wins = n()) |>
      arrange(desc(wins)) |>
      mutate(rank = rank(-wins)) |>
      group_by(wins) |>
      mutate(
        tie = if_else(n() > 1, "tie", NA)
      ) |>
      ungroup() |>
      rename("team" = winner)

    conference_standings <- conference_standings |>
      left_join(select(tiebreaker, c(team, wins)), by = join_by(team)) |>
      mutate(wins = if_else(is.na(wins.y), wins.x, wins.x + wins.y * 0.01)) |>
      select(tier, region, team, wins, losses)

    new_groups <- tiebreaker |> distinct(wins) |> nrow()

    if (new_groups == length(tied_teams)) {
      order <- tiebreaker$team
    }
    # if the records amongst each other did not change anything, resolve using sp_rankings
    if (new_groups == 1) {
      order <- sp_rankings |> filter(team %in% tied_teams) |> pull(team)
    }
  }

  if (!is.null(order)) {
    for (i in seq_along(order)) {
      conference_standings <- conference_standings |>
        mutate(wins = if_else(team == order[[i]], wins + (10 - i) * .001, wins))
    }
  }

  conference_standings |> arrange(desc(wins))
}
