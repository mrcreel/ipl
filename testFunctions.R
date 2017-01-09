res <- data.frame()

for(i in 1:nrow(t.venues.home)){
  row <- t.venues.home[i,]
  row.s <- as.numeric(row['season'])
  row.t <- as.character(row['team'])
  row.v <- as.character(row['venue'])
  
  
  home.ids <-t.match.bat.stats %>%
    filter_(~season == row.s,
            ~venue == row.v,
            ~team == row.t) %>%
    select(match.id)
  
  home.results <- t.match.bat.stats %>%
    filter(match.id %in% home.ids$match.id) %>%
    summarise(n = n(),
              balls = sum(balls),
              runs = sum(runs),
              home.4s = sum(bat.4s),
              home.6s = sum(bat.6s)) %>%
    mutate(strike.rate = runs/balls * 100)
  
  
  away.ids <-t.match.bat.stats %>%
     filter_(~season == row.s,
             ~venue != row.v,
             ~team == row.t) %>%
    select(match.id)
  
  away.results <- t.match.bat.stats %>%
    filter(match.id %in% away.ids$match.id) %>%
    summarise(n = n(),
              balls = sum(balls),
              runs = sum(runs),
              away.4s = sum(bat.4s),
              away.6s = sum(bat.6s)) %>%
    mutate(strike.rate = runs/balls * 100)
  
  res <- bind_rows(res, data.frame(season = row.s, venue = row.v, 
                                   home.g = as.integer(home.results$n/2),
                                   away.g = away.results$n/2,
                                   home.balls = home.results$balls,
                                   home.runs = home.results$runs,
                                   home.4s = home.results$home.4s,
                                   home.6s = home.results$home.6s,
                                   away.balls = away.results$balls,
                                   away.runs = away.results$runs,
                                   away.4s = away.results$away.4s,
                                   away.6s = away.results$away.6s)
  )
}

res <- res %>%
  group_by(season, venue) %>%
  summarise_all(sum) %>%
  mutate(home.strike.rate = home.runs/home.balls * 100,
         away.strike.rate = away.runs/away.balls * 100,
         park.factor = home.strike.rate/away.strike.rate * 100)
  

