for(i in 1:nrow(t.venues.home)){
  row <- t.venues.home[i,]
  row.s <- as.numeric(row['season'])
  row.t <- as.character(row['team'])
  row.v <- as.character(row['venue'])
  
  t.match.bat.stats %>%
    filter_(~season == row.s,
            ~venue == row.v,
            ~team == row.t)
}
