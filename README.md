
# FixCollegeFootball

College football needs a new system. I propose a format that divides
teams into three regions and forms a relegation system among those three
regions. I detail and support the solution in [this
article](fix_college_football.pdf).

In this Github Repository, I implement this system via simulation
beginning in 1978. Each simulation requires prespecified region
assignments for each team, rivalry game preferences for each team, and
[SP+ rankings](https://collegefootballdata.com/sp/trends) for each team.
Of course, each simulation generates new results, but below is one such
simulation:

``` r
source("simulation_world/simulation_workflow/summarize_results.R")

# load simulation specified in example_simulation.R
results_list <- readRDS(
  file = "simulation_world/data/example_results.rds"
)
```

## CFP Champions

``` r
# print cfp champions
cfp_champs(results_list)$year_by_year
```

    ## # A tibble: 48 × 3
    ##    winner     runner_up   year
    ##    <chr>      <chr>      <dbl>
    ##  1 Ohio State Miami       2025
    ##  2 Texas      SMU         2024
    ##  3 Oregon     Ole Miss    2023
    ##  4 Michigan   Georgia     2022
    ##  5 Georgia    Ohio State  2021
    ##  6 Ohio State Utah        2020
    ##  7 Alabama    LSU         2019
    ##  8 Georgia    Alabama     2018
    ##  9 Auburn     Oklahoma    2017
    ## 10 Penn State Washington  2016
    ## # ℹ 38 more rows

``` r
cfp_champs(results_list)$champs_total
```

    ## # A tibble: 22 × 3
    ##    winner        championships most_recent
    ##    <chr>                 <int>       <dbl>
    ##  1 Ohio State                7        2025
    ##  2 Florida State             5        2013
    ##  3 Georgia                   4        2021
    ##  4 Texas                     4        2024
    ##  5 Alabama                   3        2019
    ##  6 LSU                       3        2009
    ##  7 Penn State                3        2016
    ##  8 Georgia Tech              2        2000
    ##  9 Miami                     2        2001
    ## 10 USC                       2        1988
    ## # ℹ 12 more rows

## Playoff Appearances

``` r
# print teams by playoff appearances
playoff_app(results_list)
```

    ## # A tibble: 70 × 6
    ##    team          appearances round_2 semis finals championships
    ##    <chr>               <int>   <int> <int>  <int>         <int>
    ##  1 Ohio State             31      24    12      9             7
    ##  2 Florida State          23      20    16      9             5
    ##  3 Georgia                22      17     9      6             4
    ##  4 Texas                  20      14     7      5             4
    ##  5 Alabama                29      23    12      7             3
    ##  6 Penn State             22      18    11      5             3
    ##  7 LSU                    15      12     6      5             3
    ##  8 Miami                  25      21     9      4             2
    ##  9 Washington             14      10     6      4             2
    ## 10 USC                    18      11     5      4             2
    ## # ℹ 60 more rows

## Tier Journey - Baylor University

``` r
tier_journey(results_list, "Baylor", interactive = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Win Percentage - Baylor University

``` r
win_percentage(results_list, "Baylor")
```

    ## $opponents
    ## # A tibble: 94 × 4
    ##    opponent        wins losses games
    ##    <chr>          <dbl>  <int> <int>
    ##  1 Texas Tech        12     28    40
    ##  2 Texas             14     23    37
    ##  3 Texas A&M         10     26    36
    ##  4 TCU               16     18    34
    ##  5 Houston           22      8    30
    ##  6 Oklahoma State     7     13    20
    ##  7 SMU               10      9    19
    ##  8 Utah               4     12    16
    ##  9 Kansas             9      6    15
    ## 10 Stanford           7      8    15
    ## # ℹ 84 more rows
    ## 
    ## $game_type
    ## # A tibble: 7 × 4
    ##   type                  wins losses games
    ##   <chr>                <dbl>  <int> <int>
    ## 1 conference             159    177   336
    ## 2 rivalry                 55     89   144
    ## 3 inter-region            52     42    94
    ## 4 relegation-defend       16     10    26
    ## 5 relegation-challenge    11      8    19
    ## 6 consolation              3      0     3
    ## 7 makeup                   0      2     2
    ## 
    ## $year
    ## # A tibble: 48 × 6
    ##     year region  tier  wins losses games
    ##    <dbl> <chr>  <dbl> <dbl>  <int> <int>
    ##  1  1978 west       3     7      6    13
    ##  2  1979 west       3    10      3    13
    ##  3  1980 west       2     9      4    13
    ##  4  1981 west       2     4      9    13
    ##  5  1982 west       2     4      9    13
    ##  6  1983 west       3     6      7    13
    ##  7  1984 west       3     8      5    13
    ##  8  1985 west       2     6      7    13
    ##  9  1986 west       3     7      6    13
    ## 10  1987 west       3     7      6    13
    ## # ℹ 38 more rows
    ## 
    ## $tier
    ## # A tibble: 5 × 5
    ## # Groups:   region, tier [5]
    ##   region  tier  wins losses games
    ##   <chr>  <dbl> <dbl>  <int> <int>
    ## 1 west       1    26     39    65
    ## 2 west       2    71     72   143
    ## 3 west       3   115    119   234
    ## 4 west       4    72     84   156
    ## 5 west       5    12     14    26
