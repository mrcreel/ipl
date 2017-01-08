# 1. Load packages --------------------------------------------------------

library(tidyverse)
library(lubridate)


# 2. Load raw data files --------------------------------------------------

# raw.matches <- read.csv("./data/matches.csv")
# raw.deliveries <- read.csv("./data/deliveries.csv")


# 3. IPL host cities by country ----------------------------------------------

india <- c("Mumbai", "Kolkata", "Bangalore", "Delhi", "Chennai", "Chandigarh",
           "Hyderabad", "Jaipur", "Pune", "Ahmedabad", "Visakhapatnam", 
           "Dharamsala", "Cuttack", "Ranchi", "Raipur", "Kochi", "Rajkot",
           "Nagpur", "Kanpur", "Indore")
south.africa <- c("Durban", "Centurion", "Johannesburg", "Cape Town",
                  "Port Elizabeth", "East London", "Kimberley", "Bloemfontein")
uae <- c("Abu Dhabi", "Sharjah")


# 4. Select and add columns ----------------------------------------------

t.matches <- raw.matches %>%
  select(match.id = id, 
         match.date = date,
         match.team1 = team1, 
         match.team2 = team2,
         match.tossWinner = toss_winner,
         match.venue = venue,
         venue.city = city,
         match.umpire1 = umpire1,
         match.umpire2 = umpire2,
         match.result = result) %>%
  mutate(match.date = ymd(match.date))


# 5. Add games abandoned before toss -------------------------------------

t.matches <- bind_rows(t.matches,tibble(
  match.id = as.numeric(10001, 10002,
                        10003, 10004,
                        10005, 10006,
                        10007),
  match.date = ymd(c("2008-05-22", "2009-04-25", 
                     "2009-04-21", "2011-04-19", 
                     "2012-04-24", "2012-04-25",
                     "2015-04-26")),
  match.team1 = c("Delhi Daredevils", "Chennai Super Kings",
                  "Rajasthan Royals", "Royal Challengers Bangalore",
                  "Kolkata Knight Riders", "Royal Challengers Bangalore",
                  "Kolkata Knight Riders"),
  match.team2 = c("Kolkata Knight Riders", "Kolkata Knight Riders",
                  "Mumbai Indians", "Rajasthan Royals",
                  "Deccan Chargers", "Chennai Super Kings",
                  "Rajasthan Royals"),
  match.tossWinner = "",
  match.venue = c("Feroz Shah Kotla", "Newlands",
                  "Kingsmead", "M Chinnaswamy Stadium",
                  "Eden Gardens", "M Chinnaswamy Stadium",
                  "Eden Gardens"),
  match.umpire1 = "",
  match.umpire2 = "",
  match.result = "no result"))

t.matches <- t.matches %>% mutate(
  venue.country = ifelse(
    venue.city %in% india, "India", ifelse(
      venue.city %in% south.africa, "South Africa", "UAE")))

# 6. Set round of match ---------------------------------------------------
## An ugly way to set round of match (round-robin v playoff)
## 2008 & 2009 had 3 playoff matches a year.
## The other seasons have 4.

playoff.matches <- bind_rows(
  t.matches %>%
    group_by(season = year(match.date)) %>%
    filter(year(match.date) > 2009) %>%
    top_n(4, wt = match.date) %>%
    mutate(match.stage = "playoffs") %>%
    ungroup(),
  t.matches %>%
    group_by(season = year(match.date)) %>%
    filter(year(match.date) <= 2009) %>%
    top_n(3, wt = match.date) %>%
    mutate(match.stage = "playoffs") %>%
    ungroup()
)

t.matches <- bind_rows(
  t.matches %>%
    filter(!match.id %in% playoff.matches$match.id) %>%
    mutate(match.stage = "group",
           match.date = ymd(match.date),
           season = year(match.date)),
  playoff.matches) 


# 7. Initialize Variables -------------------------------------------------

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


# 8. Get match batting results for park factor ---------------------------
t.match.stats <- raw.deliveries %>%
  group_by(match.id = match_id,
           team = batting_team) %>%
  summarise(balls = sum(wide_runs == 0))



