# NCAA 2017 ------------------
#
library(dplyr)
library(tidyr)
#
# Read data
teams <- read.csv("data/Teams.csv", stringsAsFactors = FALSE)
regSeason <- read.csv("data/RegularSeasonCompactResults.csv")
thisSeason <- filter(regSeason, Season == 2016)
thisSeasonTeams <- unique(thisSeason$Wteam)
tourneyResults <- read.csv("data/TourneyCompactResults.csv")
#
# global variables
mu <- mean(regSeason$Wscore)
sigma <- sd(regSeason$Wscore)

# Compute stats per team: For pts and Avg pts
teamsW <- select(thisSeason, team_id = Wteam, pts = Wscore, ptsAg = Lscore)
teamsL <- select(thisSeason, team_id = Lteam, pts = Lscore, ptsAg = Wscore)

team_stats <- bind_rows(teamsW,teamsL) %>%
  arrange(team_id) %>%
  group_by(team_id) %>%
  mutate(Off_rating = mean(pts), Def_rating = mean(ptsAg)) %>%
  select(team_id, Off_rating, Def_rating) %>%
  distinct(team_id, .keep_all=TRUE) %>%
  as.data.frame()

# Compute Historical NCAA tournament strength per team
# round of 64 > 1 point; round of 32 > 2 pts; ... ; champs > 7 pts.
teamsW_tourney <- select(tourneyResults, Season, team_id = Wteam) %>%
  mutate(win = 1)
teamsL_tourney <- select(tourneyResults, Season, team_id = Lteam) %>%
  mutate(win = 0)

tourney_stats <- bind_rows(teamsW_tourney,teamsL_tourney) %>%
  arrange(team_id) %>%
  group_by(Season, team_id) %>%
  mutate(tPts = sum(win) + 1) %>%
  group_by(team_id) %>%
  mutate(tourneyPts = log(sum(tPts))) %>%
  select(team_id, tourneyPts) %>%
  distinct(team_id, .keep_all=TRUE) %>%
  as.data.frame()

# Put it all together
team_stats <- merge(team_stats, tourney_stats, by = "team_id") %>%
  filter(team_id %in% thisSeasonTeams)

# Compute means for all possible game combinations
# muA = Off_ratingA + Def_ratingB - mu + tourneyPtsA - tourneyPtsB
predict_mu <- data.frame()
k <- 1
for (i in 1:(length(thisSeasonTeams)-1)) {
  for (j in (i+1):length(thisSeasonTeams)) {
    thisTeamA <- filter(team_stats, team_id == thisSeasonTeams[i])
    thisTeamB <- filter(team_stats, team_id == thisSeasonTeams[j])
    if (nrow(thisTeamA)*nrow(thisTeamB) > 0){
      muA <- thisTeamA$Off_rating + thisTeamB$Def_rating - mu + thisTeamA$tourneyPts - thisTeamB$tourneyPts
      muB <- thisTeamB$Off_rating + thisTeamA$Def_rating - mu + thisTeamB$tourneyPts - thisTeamA$tourneyPts
      predict_mu[k,1] <- thisSeasonTeams[i]
      predict_mu[k,2] <- thisSeasonTeams[j]
      predict_mu[k,3] <- muA
      predict_mu[k,4] <- muB
      k <- k + 1
    }
  }
}
names(predict_mu) <- c("team_A", "team_B", "muA", "muB")

# Probability of N(muA,sigma) > N(muB,sigma) = 
# Simulate
# hist(rnorm(100,predict_mu$muA[1]-predict_mu$muB[1],sqrt(2)*sigma))
# length(which(rnorm(10000,predict_mu$muA[1]-predict_mu$muB[1],sqrt(2)*sigma)>0))
# Analytically
# 1-pnorm(0,mean = predict_mu$muA[1]-predict_mu$muB[1],sd = sqrt(2)*sigma)

predict_mu <- mutate(predict_mu, prob = 1-pnorm(0,muA-muB,sqrt(2)*sigma))
predict_mu2 <- inner_join(predict_mu, teams, by = c("team_A" = "Team_Id")) %>%
  inner_join(teams, by = c("team_B" = "Team_Id")) %>%
  select(everything(), teamName_A = Team_Name.x, teamName_B = Team_Name.y)


