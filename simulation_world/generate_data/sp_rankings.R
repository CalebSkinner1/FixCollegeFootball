library("cfbfastR")
library("tidyverse")

# start in year 1978
sp_rankings <- map_dfr(1973:2025, ~ cfbd_ratings_sp(year = .x)) |>
  select(year, team, conference, rating, ranking) |>
  filter(!str_detect(team, "nationalAverages")) |>
  mutate(
    rating = case_when(
      # for some reason UNLV in 1995 and Arkansas State in 1997 don't have an sp+, I impute a reasonable sp+ given
      # their records
      team == "UNLV" & year == 1995 ~ -20,
      team == "Arkansas State" & year == 1997 ~ -25,
      .default = rating
    )
  )

write_csv(sp_rankings, "simulation_world/data/sp_rankings.csv")
