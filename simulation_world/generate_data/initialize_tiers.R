# initialize tiers

library("tidyverse")
# we need to first initialize the tiers in 1978
sp_rankings <- read_csv(
  "simulation_world/data/sp_rankings.csv",
  show_col_types = FALSE
)

sp1977 <- sp_rankings |>
  filter(year <= 1977) |>
  mutate(ranking_points = 142 - ranking) |>
  group_by(team) |>
  summarize(aggregate_rating = sum(ranking_points * (year - 1972) / 15)) |>
  arrange(desc(aggregate_rating))

sp1978 <- sp_rankings |> filter(year == 1978)

west <- c(
  "Oklahoma",
  "USC",
  "Texas",
  "Colorado",
  "Texas A&M",
  "UCLA",
  "Texas Tech",
  "Nebraska",
  "Oklahoma State",
  "San Diego State",
  "California",
  "BYU",
  "Stanford",
  "Washington",
  "Houston",
  "Arizona State",
  "North Texas",
  "Baylor",
  "Kansas",
  "San José State",
  "Fresno State",
  "Arizona",
  "Washington State",
  "Tulsa",
  "SMU",
  "Colorado State",
  "Kansas State",
  "New Mexico",
  "Utah State",
  "West Texas A&M",
  "Hawai'i",
  "Wyoming",
  "Oregon State",
  "Oregon",
  "Air Force",
  "New Mexico State",
  "Lamar",
  "TCU",
  "Utah",
  "UTEP",
  "UNLV",
  "Nevada",
  "Idaho",
  "Boise State",
  "Texas State",
  "UTSA",
  "Rice",
  "Sam Houston"
)
central <- c(
  "Ohio State",
  "Michigan",
  "Pittsburgh",
  "Penn State",
  "Notre Dame",
  "Arkansas",
  "Yale",
  "Iowa State",
  "Rutgers",
  "Miami (OH)",
  "Cincinnati",
  "West Virginia",
  "Louisville",
  "Kentucky",
  "Michigan State",
  "Temple",
  "Harvard",
  "Navy",
  "Ball State",
  "Brown",
  "Arkansas State",
  "Bowling Green",
  "Dartmouth",
  "Kent State",
  "Boston College",
  "Virginia Tech",
  "Minnesota",
  "Central Michigan",
  "Syracuse",
  "Colgate",
  "Purdue",
  "Ohio",
  "Princeton",
  "Illinois",
  "Wisconsin",
  "Villanova",
  "Iowa",
  "Western Michigan",
  "Pennsylvania",
  "Indiana",
  "Eastern Michigan",
  "Army",
  "Toledo",
  "Cornell",
  "Columbia",
  "Indiana State",
  "Holy Cross",
  "Northwestern",
  "Southern Illinois",
  "Northern Illinois",
  "Illinois State",
  "Marshall",
  "Drake",
  "Akron",
  "UConn",
  "Buffalo",
  "Western Kentucky",
  "Massachusetts",
  "Missouri State",
  "Missouri",
  "Delaware"
)
east <- c(
  "Alabama",
  "Maryland",
  "Mississippi State",
  "Florida",
  "Georgia",
  "Ole Miss",
  "NC State",
  "Tennessee",
  "Memphis",
  "LSU",
  "East Carolina",
  "Southern Miss",
  "Auburn",
  "Louisiana Tech",
  "Miami",
  "South Carolina",
  "North Carolina",
  "Duke",
  "Clemson",
  "Louisiana",
  "Georgia Tech",
  "Florida State",
  "William & Mary",
  "Richmond",
  "Chattanooga",
  "App State",
  "McNeese",
  "Vanderbilt",
  "Furman",
  "VMI",
  "The Citadel",
  "Tulane",
  "Wake Forest",
  "Northwestern State",
  "Virginia",
  "UL Monroe",
  "Western Carolina",
  "East Tennessee State",
  "UCF",
  "UAB",
  "Middle Tennessee",
  "South Florida",
  "Troy",
  "Florida Atlantic",
  "Florida International",
  "Florida A&M",
  "Old Dominion",
  "South Alabama",
  "Georgia State",
  "Georgia Southern",
  "Charlotte",
  "Coastal Carolina",
  "Liberty",
  "James Madison",
  "Jacksonville State",
  "Kennesaw State"
)

init <- sp1978 |>
  select(team, conference) |>
  left_join(sp1977, by = join_by(team)) |>
  arrange(desc(aggregate_rating)) |>
  mutate(
    region = case_when(
      team %in% east ~ "east",
      team %in% central ~ "central",
      team %in% west ~ "west",
      .default = NA
    )
  ) |>
  group_by(region) |>
  mutate(
    rank = rank(-aggregate_rating, ties.method = "random"),
    tier = ((rank - 1) %/% 8) + 1
  ) |>
  group_by(region, tier) |>
  mutate(prev_rank = rank(-aggregate_rating, ties.method = "random")) |>
  select(team, region, tier, prev_rank) |>
  arrange(tier, region, prev_rank) |>
  ungroup()

bind_rows(
  tibble(team = east, region = "east"),
  tibble(team = west, region = "west"),
  tibble(team = central, region = "central")
) |>
  write_csv("simulation_world/data/regions.csv")


write_csv(init, "simulation_world/data/initial_tiers.csv")
