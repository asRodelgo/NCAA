### Want to try a new approach
# Classify teams according to their stats and performance during regular season
# Explore how each team does against each type of opponent: net rating, win/loss ratio, etc.
# See whether this plays a role once at the tourney
###
# Cluster teams: I can start with k-means
#
library(tidyverse)
#
# Read data
teams <- read.csv("data/data2019/Teams.csv", stringsAsFactors = FALSE)
regSeason <- read.csv("data/data2019/RegularSeasonCompactResults.csv")
thisSeason <- filter(regSeason, Season == 2019)
thisSeasonTeams <- unique(thisSeason$WTeamID)
tourneyResults <- read.csv("data/data2019/NCAATourneyCompactResults.csv")
tourneySeeds <- read.csv("data/data2019/NCAATourneySeeds.csv")
thisTourneySeeds <- filter(tourneySeeds, Season == 2019)
selectedTeams <- thisTourneySeeds$Team
#
# Read players and events as well
players <- read.csv("data/data2019/Players_2018.csv", stringsAsFactors = FALSE)
#events <- read.csv("data/data2019/Events_2018.csv", stringsAsFactors = FALSE)
team_stats <- read.csv("data/data2019/RegularSeasonDetailedRollup_v2.csv", stringsAsFactors = FALSE)
#
# k-means clusters
team_stats_18 <- filter(team_stats, Season == "2018")
# add percent of home wins
loc_factor <- filter(regSeason, Season == "2018") %>%
  select(WTeamID, WLoc) %>%
  group_by(WTeamID) %>%
  mutate(WinTotal = n()) %>%
  filter(WLoc == "H") %>%
  add_tally() %>%
  #group_by(WTeamID, WLoc) %>%
  mutate(HomeWinPerc = n/WinTotal) %>%
  #filter(WLoc == "H") %>%
  distinct(WTeamID, HomeWinPerc)
#
team_stats_18 <- left_join(team_stats_18, loc_factor, by = c("TeamID" = "WTeamID")) %>%
  mutate(HomeWinPerc = ifelse(is.na(HomeWinPerc),0,HomeWinPerc))

set.seed(456)
teamCluster <- kmeans(team_stats_18[,-c(1,2)], 5, nstart = 10, iter.max = 20)
team_stats_18 <- bind_cols(team_stats_18, cluster = teamCluster$cluster)
#
# global variables
mu <- mean(regSeason$WScore)
sigma <- sd(regSeason$WScore)

# Compute stats per team: For pts and Avg pts
teamsW <- select(thisSeason, team_id = WTeamID, pts = WScore, ptsAg = LScore)
teamsL <- select(thisSeason, team_id = LTeamID, pts = LScore, ptsAg = WScore)

team_stats <- bind_rows(teamsW,teamsL) %>%
  arrange(team_id) %>%
  group_by(team_id) %>%
  mutate(Off_rating = mean(pts), Def_rating = mean(ptsAg)) %>%
  select(team_id, Off_rating, Def_rating) %>%
  distinct(team_id, .keep_all=TRUE) %>%
  as.data.frame()




