# NCAA 2017 ------------------
#
library(dplyr)
library(tidyr)
#
# Read data
teams <- read.csv("data/Teams.csv", stringsAsFactors = FALSE)
regSeason <- read.csv("data/RegularSeasonCompactResults.csv")
thisSeason <- filter(regSeason, Season == 2017)
thisSeasonTeams <- unique(thisSeason$Wteam)
tourneyResults <- read.csv("data/TourneyCompactResults.csv")
tourneySeeds <- read.csv("data/TourneySeeds.csv")
thisTourneySeeds <- filter(tourneySeeds, Season == 2017)
selectedTeams <- thisTourneySeeds$Team
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
team_stats <- left_join(team_stats, tourney_stats, by = c("team_id" = "team_id")) %>%
  filter(team_id %in% selectedTeams) %>%
  inner_join(thisTourneySeeds, by=c("team_id" = "Team")) %>%
  mutate(Seed = as.numeric(substr(Seed,2,3)), tourneyPts = ifelse(is.na(tourneyPts), 0, tourneyPts)) %>%
  select(-Season) %>%
  as.data.frame()

# Compute means for all possible game combinations
# muA = Off_ratingA + Def_ratingB - mu + tourneyPtsA - tourneyPtsB
predict_mu <- data.frame()
k <- 1
for (i in 1:(length(selectedTeams)-1)) {
  for (j in (i+1):length(selectedTeams)) {
    thisTeamA <- filter(team_stats, team_id == selectedTeams[i])
    thisTeamB <- filter(team_stats, team_id == selectedTeams[j])
    if (nrow(thisTeamA)*nrow(thisTeamB) > 0){
      muA <- thisTeamA$Off_rating + thisTeamB$Def_rating - mu + thisTeamA$tourneyPts - thisTeamB$tourneyPts
      muB <- thisTeamB$Off_rating + thisTeamA$Def_rating - mu + thisTeamB$tourneyPts - thisTeamA$tourneyPts
      predict_mu[k,1] <- selectedTeams[i]
      predict_mu[k,2] <- selectedTeams[j]
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

submission <- arrange(predict_mu,team_A,team_B) %>%
  left_join(teams,by=c("team_A"="Team_Id")) %>%
  left_join(teams,by=c("team_B"="Team_Id")) %>%
  mutate(Id = ifelse(team_A > team_B, paste("2017",team_B,team_A,sep = "_"),paste("2017",team_A,team_B,sep = "_")),
                     Pred = ifelse(team_A > team_B,1-prob,prob)) %>%
  mutate(Pred = ifelse(Pred >=.9,.999,ifelse(Pred<=.1,0.001,Pred))) %>%
  #select(Id,Pred,Team_Name_A = Team_Name.x, Team_Name_B = Team_Name.y) %>%
  select(Id,Pred) %>%
  arrange(Id)


# write.csv(submission, "data/ncaa2017_muyayo2.csv",row.names = FALSE)

predict_mu2 <- inner_join(predict_mu, teams, by = c("team_A" = "Team_Id")) %>%
  inner_join(teams, by = c("team_B" = "Team_Id")) %>%
  select(everything(), teamName_A = Team_Name.x, teamName_B = Team_Name.y) %>%
  group_by(team_A) %>%
  mutate(pred_rankA = sum(ifelse(prob < .5, 1, 0))) %>%
  group_by(team_B) %>%
  mutate(pred_rankB = sum(ifelse(prob > .5, 1, 0))) %>%
  as.data.frame()

ranksA <- group_by(predict_mu2,team_A) %>% select(team_A, pred_rankA) %>% distinct(team_A,.keep_all=TRUE) %>% as.data.frame()
ranksB <- group_by(predict_mu2,team_B) %>% select(team_B, pred_rankB) %>% distinct(team_B,.keep_all=TRUE) %>% as.data.frame()
ranks <- full_join(ranksA, ranksB, by = c("team_A" = "team_B")) %>%
  mutate(pred_rankA = ifelse(is.na(pred_rankA),0,pred_rankA),
         pred_rankB = ifelse(is.na(pred_rankB),0,pred_rankB),
         final_rank = pred_rankA + pred_rankB + 1) %>% 
  left_join(teams, by = c("team_A" = "Team_Id")) %>%
  arrange(Team_Name) %>%
  select(-pred_rankA, -pred_rankB) %>%
  as.data.frame()
  
team_stats <- inner_join(team_stats, ranks, by=c("team_id"="team_A")) %>%
  arrange(final_rank)

# Check out specific match probabilities:
teamA <- "Oregon"
teamB <- "South Carol"
filter(predict_mu2, (grepl(teamA,teamName_A) & grepl(teamB,teamName_B)) | 
         (grepl(teamA,teamName_B) & grepl(teamB,teamName_A)))

#filter(predict_mu2,(teamName_A == "Maryland"  & prob < .5) | (teamName_B == "Maryland" & prob > .5))
