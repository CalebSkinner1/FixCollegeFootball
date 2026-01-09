
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
tier_journey(results_list, "Baylor")
```

<div class="plotly html-widget html-fill-item" id="htmlwidget-c4615f58dfd6d58a585c" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-c4615f58dfd6d58a585c">{"x":{"data":[{"x":[1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025,2026],"y":[3,3,2,2,2,3,3,2,3,3,3,4,3,3,3,2,3,2,3,3,4,3,4,4,5,5,4,4,4,4,4,4,4,4,3,3,2,2,1,1,1,2,1,1,2,2,3,3,3],"text":["year: 1978<br />tier: 3","year: 1979<br />tier: 3","year: 1980<br />tier: 2","year: 1981<br />tier: 2","year: 1982<br />tier: 2","year: 1983<br />tier: 3","year: 1984<br />tier: 3","year: 1985<br />tier: 2","year: 1986<br />tier: 3","year: 1987<br />tier: 3","year: 1988<br />tier: 3","year: 1989<br />tier: 4","year: 1990<br />tier: 3","year: 1991<br />tier: 3","year: 1992<br />tier: 3","year: 1993<br />tier: 2","year: 1994<br />tier: 3","year: 1995<br />tier: 2","year: 1996<br />tier: 3","year: 1997<br />tier: 3","year: 1998<br />tier: 4","year: 1999<br />tier: 3","year: 2000<br />tier: 4","year: 2001<br />tier: 4","year: 2002<br />tier: 5","year: 2003<br />tier: 5","year: 2004<br />tier: 4","year: 2005<br />tier: 4","year: 2006<br />tier: 4","year: 2007<br />tier: 4","year: 2008<br />tier: 4","year: 2009<br />tier: 4","year: 2010<br />tier: 4","year: 2011<br />tier: 4","year: 2012<br />tier: 3","year: 2013<br />tier: 3","year: 2014<br />tier: 2","year: 2015<br />tier: 2","year: 2016<br />tier: 1","year: 2017<br />tier: 1","year: 2018<br />tier: 1","year: 2019<br />tier: 2","year: 2020<br />tier: 1","year: 2021<br />tier: 1","year: 2022<br />tier: 2","year: 2023<br />tier: 2","year: 2024<br />tier: 3","year: 2025<br />tier: 3","year: 2026<br />tier: 3"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(0,0,0,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[2026,2025,2024,2023,2022,2021,2020,2019,2018,2017,2016,2015,2014,2013,2012,2011,2010,2009,2008,2007,2006,2005,2004,2003,2002,2001,2000,1999,1998,1997,1996,1995,1994,1993,1992,1991,1990,1989,1988,1987,1986,1985,1984,1983,1982,1981,1980,1979,1978],"y":[3,3,3,2,2,1,1,2,1,1,1,2,2,3,3,4,4,4,4,4,4,4,4,5,5,4,4,3,4,3,3,2,3,2,3,3,3,4,3,3,3,2,3,3,2,2,2,3,3],"text":["year: 2026<br />tier: 3<br />Record: unplayed","year: 2025<br />tier: 3<br />Record: 7-6","year: 2024<br />tier: 3<br />Record: 6-7","year: 2023<br />tier: 2<br />Record: 3-10","year: 2022<br />tier: 2<br />Record: 10-3","year: 2021<br />tier: 1<br />Record: 7-6","year: 2020<br />tier: 1<br />Record: 5-8","year: 2019<br />tier: 2<br />Record: 9-4","year: 2018<br />tier: 1<br />Record: 5-8","year: 2017<br />tier: 1<br />Record: 2-11","year: 2016<br />tier: 1<br />Record: 7-6","year: 2015<br />tier: 2<br />Record: 11-2","year: 2014<br />tier: 2<br />Record: 9-4","year: 2013<br />tier: 3<br />Record: 9-4","year: 2012<br />tier: 3<br />Record: 5-8","year: 2011<br />tier: 4<br />Record: 10-3","year: 2010<br />tier: 4<br />Record: 6-7","year: 2009<br />tier: 4<br />Record: 6-7","year: 2008<br />tier: 4<br />Record: 5-8","year: 2007<br />tier: 4<br />Record: 4-9","year: 2006<br />tier: 4<br />Record: 5-8","year: 2005<br />tier: 4<br />Record: 6-7","year: 2004<br />tier: 4<br />Record: 6-7","year: 2003<br />tier: 5<br />Record: 7-6","year: 2002<br />tier: 5<br />Record: 5-8","year: 2001<br />tier: 4<br />Record: 2-11","year: 2000<br />tier: 4<br />Record: 3-10","year: 1999<br />tier: 3<br />Record: 0-13","year: 1998<br />tier: 4<br />Record: 9-4","year: 1997<br />tier: 3<br />Record: 4-9","year: 1996<br />tier: 3<br />Record: 7-6","year: 1995<br />tier: 2<br />Record: 4-9","year: 1994<br />tier: 3<br />Record: 6-7","year: 1993<br />tier: 2<br />Record: 2-11","year: 1992<br />tier: 3<br />Record: 7-6","year: 1991<br />tier: 3<br />Record: 7-6","year: 1990<br />tier: 3<br />Record: 7-6","year: 1989<br />tier: 4<br />Record: 10-3","year: 1988<br />tier: 3<br />Record: 5-8","year: 1987<br />tier: 3<br />Record: 7-6","year: 1986<br />tier: 3<br />Record: 7-6","year: 1985<br />tier: 2<br />Record: 6-7","year: 1984<br />tier: 3<br />Record: 8-5","year: 1983<br />tier: 3<br />Record: 6-7","year: 1982<br />tier: 2<br />Record: 4-9","year: 1981<br />tier: 2<br />Record: 4-9","year: 1980<br />tier: 2<br />Record: 9-4","year: 1979<br />tier: 3<br />Record: 10-3","year: 1978<br />tier: 3<br />Record: 7-6"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,0,0,1)","opacity":1,"size":0.37795275590551186,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(0,0,0,1)"}},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":23.305936073059364,"r":7.3059360730593621,"b":37.260273972602747,"l":31.415525114155255},"paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[1975.5999999999999,2028.4000000000001],"tickmode":"array","ticktext":["1980","1990","2000","2010","2020"],"tickvals":[1980,1990,2000,2010,2020],"categoryorder":"array","categoryarray":["1980","1990","2000","2010","2020"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0,"zeroline":false,"anchor":"y","title":{"text":"year","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.80000000000000004,5.2000000000000002],"tickmode":"array","ticktext":["1","2","3","4","5"],"tickvals":[1,2,3,4,5],"categoryorder":"array","categoryarray":["1","2","3","4","5"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0,"zeroline":false,"anchor":"x","title":{"text":"tier","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","layer":"below","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"103842ae96d71":{"x":{},"y":{},"type":"scatter"},"1038443102bb9":{"x":{},"y":{},"text":{}}},"cur_data":"103842ae96d71","visdat":{"103842ae96d71":["function (y) ","x"],"1038443102bb9":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

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
