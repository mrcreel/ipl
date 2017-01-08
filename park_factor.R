# 1. Load packages --------------------------------------------------------

library(tidyverse)
library(lubridate)


# 2. Load raw data files --------------------------------------------------

raw.matches <- read.csv("./data/matches.csv")
raw.deliveries <- read.csv("./data/deliveries.csv")


# 3. Select and add columns ----------------------------------------------

t.matches <- raw.matches %>%
  select(match.id = id, 
         match.date = date,
         match.team1 = team1, 
         match.team2 = team2,
         match.venue = venue,
         match.result = result) %>%
  mutate(match.date = ymd(match.date))

# 4. Set round of match ---------------------------------------------------
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

rm(playoff.matches) 

# 5. Use games played at venue to determine home teams for season --------------

threshhold.match <- 3

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


# 6. Get match batting results for park factor computation ---------------------
t.match.stats <- raw.deliveries %>%
  group_by(match.id = match_id,
           team = batting_team)