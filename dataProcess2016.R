#############################################
# NCAA Prediction Competition kaggle.com
#
# alberto.sanchez.rodelgo@gmail.com
#############################################
require(plyr)
require(dplyr)
require(tidyr)
require(caret)
require(tsne)
require(bit64)
library(doMC) # use parallel processing on this machine through "foreach"
registerDoMC(2) # As far as I know my MAC works on 2 cores

########### DATA PREPARATION PART STARTS HERE ##############

# loading the data (I previously downloaded into a local folder)
regSeason <- read.csv(file = "data/RegularSeasonCompactResults.csv")
teams <- read.csv(file = "data/Teams.csv")
seasons <- read.csv(file = "data/Seasons.csv")
tourney <- read.csv(file = "data/TourneyCompactResults.csv")
tourSeeds <- read.csv(file = "data/TourneySeeds.csv")
regSeasonDet <- read.csv(file = "data/RegularSeasonDetailedResults.csv")
tourneyDet <- read.csv(file = "data/TourneyDetailedResults.csv")

# Regular season stats by team

# 1 Team A stats (For stats)
# 1.1 duplicate file and mutate grouping by team id
# win teams
rsWinTeams <- select(regSeasonDet, -starts_with("L"), teamOp = Lteam)
rsWinTeams <- select(rsWinTeams, everything(), team = Wteam)
rsWinTeams <- mutate(rsWinTeams, loc = Wloc)
rsWinTeams <- select(rsWinTeams, -Wloc)
rsWinTeams$loc <- as.character(rsWinTeams$loc)
names(rsWinTeams) <- gsub("W","",names(rsWinTeams),fixed=TRUE)
# lose teams
rsLoseTeams <- select(regSeasonDet, -starts_with("W"), teamOp = Wteam, Wloc)
rsLoseTeams <- mutate(rsLoseTeams, loc = ifelse(as.character(Wloc) == "H","A",ifelse(as.character(Wloc) == "A", "H","N")))
rsLoseTeams <- select(rsLoseTeams, everything(), team = Lteam, -Wloc)
names(rsLoseTeams) <- gsub("L","",names(rsLoseTeams),fixed=TRUE)
# all together
rsTeams <- rbind(rsWinTeams,rsLoseTeams)
# 1.2 Calculate stats by team per Season
rsTeamStats <- rsTeams %>%
  group_by(Season,team) %>%
  mutate(avgScore = mean(score), sdScore = sd(score), avgNumot = mean(Numot),
         avgFgm = mean(fgm), sdFgm = sd(fgm), avgFga = mean(fga), sdFga = sd(fga),
         avgFgm3 = mean(fgm3), sdFgm3 = sd(fgm3), avgFga3 = mean(fga3), sdFga3 = sd(fga3),
         avgFtm = mean(ftm), sdFtm = sd(ftm), avgFta = mean(fta), sdFta = sd(fta),
         avgOr = mean(or), sdOr = sd(or), avgDr = mean(dr), sdDr = sd(dr),
         avgAst = mean(ast), sdAst = sd(ast), avgTo = mean(to), sdTo = sd(to),
         avgStl = mean(stl), sdStl = sd(stl), avgBlk = mean(blk), sdBlk = sd(blk),
         avgPf = mean(pf), sdPf = sd(pf)) %>%
  select(Season, team, starts_with("Avg"), starts_with("Sd")) %>%
  distinct(Season,team)
rsTeamA <- as.data.frame(rsTeamStats)
rsTeamA$team <- as.factor(rsTeamA$team)
# ------------------------------------
# 2 Team B stats (Against stats)
# 2.1 duplicate file and mutate grouping by team id
# win teams
rsWinTeams <- select(regSeasonDet, -starts_with("W"), Wteam, teamOp = Lteam, Wloc)
rsWinTeams <- select(rsWinTeams, everything(), team = Wteam)
rsWinTeams <- mutate(rsWinTeams, loc = Wloc)
rsWinTeams <- select(rsWinTeams, -Wloc)
rsWinTeams$loc <- as.character(rsWinTeams$loc)
names(rsWinTeams) <- gsub("L","",names(rsWinTeams),fixed=TRUE)
# lose teams
rsLoseTeams <- select(regSeasonDet, -starts_with("L"), Lteam, teamOp = Wteam)
rsLoseTeams <- mutate(rsLoseTeams, loc = ifelse(as.character(Wloc) == "H","A",ifelse(as.character(Wloc) == "A", "H","N")))
rsLoseTeams <- select(rsLoseTeams, everything(), team = Lteam, -Wloc)
names(rsLoseTeams) <- gsub("W","",names(rsLoseTeams),fixed=TRUE)
# all together
rsTeams <- rbind(rsWinTeams,rsLoseTeams)

# 2.2 Calculate stats by team per Season
rsTeamStats <- rsTeams %>%
  group_by(Season,team) %>%
  mutate(avgScore = mean(score), sdScore = sd(score), avgNumot = mean(Numot),
         avgFgm = mean(fgm), sdFgm = sd(fgm), avgFga = mean(fga), sdFga = sd(fga),
         avgFgm3 = mean(fgm3), sdFgm3 = sd(fgm3), avgFga3 = mean(fga3), sdFga3 = sd(fga3),
         avgFtm = mean(ftm), sdFtm = sd(ftm), avgFta = mean(fta), sdFta = sd(fta),
         avgOr = mean(or), sdOr = sd(or), avgDr = mean(dr), sdDr = sd(dr),
         avgAst = mean(ast), sdAst = sd(ast), avgTo = mean(to), sdTo = sd(to),
         avgStl = mean(stl), sdStl = sd(stl), avgBlk = mean(blk), sdBlk = sd(blk),
         avgPf = mean(pf), sdPf = sd(pf)) %>%
  select(Season, team, starts_with("Avg"), starts_with("Sd")) %>%
  distinct(Season,team)
rsTeamB <- as.data.frame(rsTeamStats)
rsTeamB$team <- as.factor(rsTeamB$team)
# --------------------
# 3. Merge For and Against stats in 1 file per team
rsTeamStats <- merge(rsTeamA, rsTeamB, by=c("Season","team"))

# --------------------
# 4. Add other variables: Name of team, Region + Seed (from tourSeeds)
#rsTeamStats <- merge(rsTeamStats, teams, by.x = "team", by.y = "team_id")
rsTeamStats <- merge(rsTeamStats, tourSeeds, by.x = c("Season","team"), by.y = c("Season","Team"), all.x = TRUE)
rsTeamStats <- merge(rsTeamStats, teams, by.x = "team", by.y = "Team_Id")
# numbersFor/numbersAg
rsTeamStats <- rsTeamStats %>%
  group_by(Season) %>%
  mutate(scoreR = avgScore.x/avgScore.y, fgmR = avgFgm.x/avgFgm.y,
         fgpR = (avgFgm.x/avgFga.x)/(avgFgm.y/avgFga.y),fgm3R = avgFgm3.x/avgFgm3.y,
         fgp3R = (avgFgm3.x/avgFga3.x)/(avgFgm3.y/avgFga3.y), ftmR = avgFtm.x/avgFtm.y,
         ftpR = (avgFtm.x/avgFta.x)/(avgFtm.y/avgFta.y), orR = avgOr.x/avgOr.y,
         drR = avgDr.x/avgDr.y, astR = avgAst.x/avgAst.y, toR = avgTo.x/avgTo.y, 
         stlR = avgStl.x/avgStl.y, blkR = avgBlk.x/avgBlk.y, pfR = avgPf.x/avgPf.y) %>%
  select(-contains("avg"), -contains("sd"))
  
# --------------------
# 5. Add outcome variables (y), i.e., tourney score difference
# win teams
trWinTeams <- select(tourneyDet, Season, team = Wteam, teamOp = Lteam, Wscore, Lscore)
trWinTeams <- mutate(trWinTeams, y_score = Wscore-Lscore)
trWinTeams <- select(trWinTeams, -Lscore,-Wscore)
# lose teams
trLoseTeams <- select(tourneyDet, Season, team = Lteam, teamOp = Wteam, Wscore, Lscore)
trLoseTeams <- mutate(trLoseTeams, y_score = Lscore-Wscore)
trLoseTeams <- select(trLoseTeams, -Lscore,-Wscore)
# all together
trTeams <- rbind(trWinTeams,trLoseTeams)
trTeams <- rename(trWinTeams,teamA = team, teamB = teamOp)

# define even/odd numbers
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

# feature engineering:
# featuresTeamA - featuresTeamB
training <- data.frame()
for (s in unique(trTeams$Season)){
  thisSeason <- trTeams[trTeams$Season==s,]
  thisSeasonRS <- rsTeamStats[rsTeamStats$Season==s,]
  for (i in 1:nrow(thisSeason)){
    auxA <- thisSeasonRS[thisSeasonRS$team==thisSeason$teamA[i],5:ncol(thisSeasonRS)]
    auxB <- thisSeasonRS[thisSeasonRS$team==thisSeason$teamB[i],5:ncol(thisSeasonRS)]
    if (is.wholenumber(i/2)){ # balance results to model differences in scores not margin of wins
      auxA_B <- auxA - auxB
      aux_All <- c(s,paste0(thisSeason$teamA[i],"_",thisSeason$teamB[i]),
                   as.numeric(substr(as.character(thisSeasonRS[thisSeasonRS$team==thisSeason$teamB[i],]$Seed),2,3))-
                     as.numeric(substr(as.character(thisSeasonRS[thisSeasonRS$team==thisSeason$teamA[i],]$Seed),2,3)),
                   auxA_B,thisSeason$y_score[i])
    } else {
      auxA_B <- auxB - auxA
      aux_All <- c(s,paste0(thisSeason$teamB[i],"_",thisSeason$teamA[i]),
                   as.numeric(substr(as.character(thisSeasonRS[thisSeasonRS$team==thisSeason$teamA[i],]$Seed),2,3))-
                     as.numeric(substr(as.character(thisSeasonRS[thisSeasonRS$team==thisSeason$teamB[i],]$Seed),2,3)),
                   auxA_B,-thisSeason$y_score[i])
    }
    names(aux_All)[1] <- "Season"
    names(aux_All)[2] <- "teamA_teamB"
    names(aux_All)[3] <- "seedDiff"
    names(aux_All)[length(aux_All)] <- "y_score"
    training <- rbind(training,as.data.frame(aux_All))
  }
}

# head(select(training, Season,teamA_teamB,avgScore.x,avgScore.y,y_score),50)


###################################### 
# trTeams <- trTeams %>%
#   group_by(Season) %>%
#   arrange(team, teamOp) %>%
#   distinct(team,teamOp)
# 
# now prepare my training dataset: for each tourney matchup, prepare the x,y pairs.
# x is reg season, y is y_score 
# trainingA <- merge(rsTeamStats,trTeams, by.x = c("Season","team"),by.y = c("Season","teamA"))
# trainingA <- rename(trainingA, teamOp = teamB)
# #trainingA <- rename(trainingA,teamA = team)
# #names(trainingA) <- gsub(".x","A_For",names(trainingA),fixed = TRUE)
# #names(trainingA) <- gsub(".y","A_Ags",names(trainingA),fixed = TRUE)
# trainingB <- merge(rsTeamStats,trTeams, by.x = c("Season","team"),by.y = c("Season","teamB"))
# trainingB <- rename(trainingB, teamOp = teamA)
# trainingC <- rbind(trainingA,trainingB)
# training <- data.frame()
# i <- 1
# for (s in trainingC$Season){
#   trainingC2 <- filter(trainingC, Season==s)
#   for (tA in trainingC2$team){
#     #for (tB in unique(trainingC$teamOp)){
#       #if (!(tA==tB)){
#     trainC3 <- filter(trainingC2, team==tA)
#     tB <- trainC3$teamOp[1]
#     trainC3Op <- filter(trainingC2, team==tB)
#     trainC4 <- filter(trainC3,teamOp == tB)
#     training[i,1] <- trainC3$Season[1]
#     training[i,2] <- paste(tA,"_",tB)
#     for (j in 3:(ncol(trainC3)-4)){
#       training[i,j] <- trainC3$team[1] - trainC3Op$team[1]
#     }
#     training[i,j+1] <- trainC4$y_score
#     i <- i + 1
#       #}
#     #}
#   }
# }
# 
# training <- merge(trainingA,trainingB, by=c("Season","team"))
# training <- training %>%
#   group_by(Season) %>%
#   mutate()




# # Calculate the differences W - L
# regSeaDiff <- regSeasonDet %>%
#   mutate(Dteam = paste0(Wteam,"_",Lteam), Dscore = Wscore - Lscore, Dfga = Wfga - Lfga,
#          Dfgp = Wfgm/Wfga - Lfgm/Lfga, Dfga3 = Wfga3 - Lfga3, Dfgp3 = Wfgm3/Wfga3 - Lfgm3/Lfga3, 
#          Dfta = Wfta - Lfta, Dftp = Wftm/Wfta - Lftm/Lfta, Dor = Wor - Lor,
#          Ddr = Wdr - Ldr, Dast = Wast - Last, Dto = Wto - Lto, Dstl = Wstl - Lstl,
#          Dblk = Wblk - Lblk, Dpf = Wpf - Lpf
#          ) %>%
#   select(Season, Daynum, Wloc, Numot, starts_with("D"))
# # create dummy variables
# Dum <- dummyVars(~ Wloc, data=regSeaDiff)
# dummy <- predict(Dum,newdata=regSeaDiff)
# dummy <- as.data.frame(dummy) # takes about 8 minutes
# regSeaDiffDum <- cbind(regSeaDiff,dummy)
# # now remove dummified variables
# training <- select(regSeaDiffDum, -Wloc, -Dteam)
# op <- par(mfrow = c(3, 6))
# for (i in 1:ncol(training)){
#   hist(training[,i], main=names(training)[i])
# }
