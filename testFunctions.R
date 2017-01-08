for(i in 1:nrow(t.venues.home)){
  row <- t.venues.home[i,]
  row.s <- as.numeric(row['season'])
  row.t <- as.character(row['team'])
  row.v <- as.character(row['venue'])
  
  home <-t.match.bat.stats %>%
    filter_(~season == row.s,
            ~venue == row.v,
            ~team == row.t) %>%
    group_by(season, team) %>%
    select(-match.id) %>%
    summarise(balls = sum(balls),
                  runs = sum(runs),
                  bat.4s = sum(bat.4s),
                  bat.6s = sum(bat.6s)) %>%
    mutate(strike.rate = runs/balls * 100)
  
  away <-t.match.bat.stats %>%
    filter_(~season == row.s,
            ~venue != row.v,
            ~team == row.t) %>%
    group_by(season, team) %>%
    select(-match.id) %>%
    summarise(balls = sum(balls),
                  runs = sum(runs),
                  bat.4s = sum(bat.4s),
                  bat.6s = sum(bat.6s)) %>%
    mutate(strike.rate = runs/balls * 100)
  
}
