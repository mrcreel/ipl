# # 1. Load packages ------------------------------------------------------------
# library(tidyverse)
# library(lubridate)
# 
# # 2. Load raw data files ------------------------------------------------------
# raw.matches <- read.csv("./data/matches.csv", stringsAsFactors = FALSE)
# raw.deliveries <- read.csv("./data/deliveries.csv", stringsAsFactors = FALSE)

# 3. Trim down raw.matches  dataframe------------------------------------------
t.matches <- raw.matches %>%
  select(match.id = id, 
         match.date = date,
         match.team1 = team1, 
         match.team2 = team2,
         match.venue = venue,
         match.result = result) %>%
  mutate(match.date = ymd(match.date))

# 4. Set round of match -------------------------------------------------------
## An ugly way to set round of match (group stage vs knockout)
## 2008 & 2009 had 3 playoff matches a year.
## The other seasons have 4.
playoff.matches <- bind_rows(
  t.matches %>%
    group_by(season = year(match.date)) %>%
    filter(year(match.date) > 2009) %>%
    top_n(4, wt = match.date) %>%
    mutate(match.stage = "knockout") %>%
    ungroup(),
  t.matches %>%
    group_by(season = year(match.date)) %>%
    filter(year(match.date) <= 2009) %>%
    top_n(3, wt = match.date) %>%
    mutate(match.stage = "knockout") %>%
    ungroup()
)

t.matches <- bind_rows(
  t.matches %>%
    filter(!match.id %in% playoff.matches$match.id) %>%
    mutate(match.stage = "group",
           match.date = ymd(match.date),
           season = year(match.date)),
  playoff.matches) 

# ployoff.matches no longer needed
rm(playoff.matches) 

# 5. Use games played at venue to determine home teams for season -------------
threshhold.match <- 4

t.venues.home <- bind_rows(
  t.matches %>%
    group_by(season = year(match.date),
             team = match.team1,
             venue = match.venue) %>%
    filter(match.stage == "group"),
  t.matches %>%
    group_by(season = year(match.date),
             team = match.team2,
             venue = match.venue) %>%
    filter(match.stage == "group")) %>%
  group_by(season, team, venue) %>%
  summarise(team.games = n()) %>%
  filter(team.games >= threshhold.match)

# 6. Get match batting results for park factor computation --------------------
t.match.bat.stats <- raw.deliveries %>%
  group_by(match.id = match_id,
           team = batting_team) %>%
  summarise(balls = sum(wide_runs == 0),
            runs = sum(batsman_runs),
            bat.4s = sum(batsman_runs == 4),
            bat.6s = sum(batsman_runs == 6)) %>%
  merge(t.matches %>%
          select(match.id, season, venue = match.venue))

# 7. Calculate Park Factor ------------------------------------------------
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
  

g <-ggplot(res, aes(season, park.factor)) + 
  geom_point() + 
  geom_smooth() +
  ylim(c(75,150)) +
  facet_wrap(~venue)
g
