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
# add clusters to regSeason
regSeason_18 <- filter(regSeason, Season == "2018") %>%
  left_join(select(team_stats_18, TeamID, WTeam_cluster = cluster), by = c("WTeamID"="TeamID")) %>%
  left_join(select(team_stats_18, TeamID, LTeam_cluster = cluster), by = c("LTeamID"="TeamID")) 
# global variables
mu <- mean(regSeason_18$WScore)
sigma <- sd(regSeason_18$WScore)
# Compute stats per team: For pts and Avg pts
teamsW <- select(regSeason_18, team_id = WTeamID, team_cluster = WTeam_cluster, pts = WScore, ptsAg = LScore, opp_cluster = LTeam_cluster)
teamsL <- select(regSeason_18, team_id = LTeamID, team_cluster = LTeam_cluster, pts = LScore, ptsAg = WScore, opp_cluster = WTeam_cluster)
#
# Compute stats grouping by clusters
team_stats_cluster <- bind_rows(teamsW,teamsL) %>%
  arrange(team_id) %>%
  group_by(team_id, opp_cluster) %>%
  mutate(Off_rating = mean(pts), Def_rating = mean(ptsAg)) %>%
  select(team_id, team_cluster, Off_rating, Def_rating, opp_cluster) %>%
  distinct(team_id, opp_cluster, .keep_all=TRUE) %>%
  ungroup() %>%
  as.data.frame()
# build a matrix
team_stats_cluster2 <- mutate(team_stats_cluster, net_rating = Off_rating - Def_rating) %>%
  select(contains("team"), net_rating, opp_cluster) %>%
  spread(opp_cluster, net_rating)
# summarize cluster vs cluster
cluster_v_cluster <- group_by(team_stats_cluster2, team_cluster) %>%
  select(-team_id) %>%
  summarise_if(is.numeric, funs(avg_net_rtg = mean, sd_net_rtg = sd) , na.rm = TRUE)
#

