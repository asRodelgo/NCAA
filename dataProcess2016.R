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


regSeasonDetT <- filter(regSeasonDet, Season == 2016)
regSeasonDet <- filter(regSeasonDet, Season < 2016)

# ---------------------------------------------
# Historical seasons for the training data
# ---------------------------------------------

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


# --------------------------------------------
# Testing data: regSeason 2015-2016
# ---------------------------------------------

# Regular season stats by team
# 1 Team A stats (For stats)
# 1.1 duplicate file and mutate grouping by team id
# win teams
rsWinTeams <- select(regSeasonDetT, -starts_with("L"), teamOp = Lteam)
rsWinTeams <- select(rsWinTeams, everything(), team = Wteam)
rsWinTeams <- mutate(rsWinTeams, loc = Wloc)
rsWinTeams <- select(rsWinTeams, -Wloc)
rsWinTeams$loc <- as.character(rsWinTeams$loc)
names(rsWinTeams) <- gsub("W","",names(rsWinTeams),fixed=TRUE)
# lose teams
rsLoseTeams <- select(regSeasonDetT, -starts_with("W"), teamOp = Wteam, Wloc)
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
rsWinTeams <- select(regSeasonDetT, -starts_with("W"), Wteam, teamOp = Lteam, Wloc)
rsWinTeams <- select(rsWinTeams, everything(), team = Wteam)
rsWinTeams <- mutate(rsWinTeams, loc = Wloc)
rsWinTeams <- select(rsWinTeams, -Wloc)
rsWinTeams$loc <- as.character(rsWinTeams$loc)
names(rsWinTeams) <- gsub("L","",names(rsWinTeams),fixed=TRUE)
# lose teams
rsLoseTeams <- select(regSeasonDetT, -starts_with("L"), Lteam, teamOp = Wteam)
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
trTeams <- filter(tourSeeds, Season == 2016)

# define even/odd numbers
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

# feature engineering:
# featuresTeamA - featuresTeamB
testing <- data.frame()
thisSeasonRS <- rsTeamStats

for (i in 1:nrow(trTeams)) {
  for (j in i+1:nrow(trTeams)) {
      auxA <- thisSeasonRS[thisSeasonRS$team==trTeams$Team[i],5:ncol(thisSeasonRS)]
      auxB <- thisSeasonRS[thisSeasonRS$team==trTeams$Team[j],5:ncol(thisSeasonRS)]
      if (nrow(auxA)==1 & nrow(auxB)==1) {
        if (is.wholenumber(i/2)){ # balance results to model differences in scores not margin of wins
        auxA_B <- auxA - auxB
        aux_All <- c(s,paste0(trTeams$Team[i],"_",trTeams$Team[j]),
                     as.numeric(substr(as.character(thisSeasonRS[thisSeasonRS$team==trTeams$Team[j],]$Seed),2,3))-
                       as.numeric(substr(as.character(thisSeasonRS[thisSeasonRS$team==trTeams$Team[i],]$Seed),2,3)),
                     auxA_B)
      } else {
        auxA_B <- auxB - auxA
        aux_All <- c(s,paste0(trTeams$Team[j],"_",trTeams$Team[i]),
                     as.numeric(substr(as.character(thisSeasonRS[thisSeasonRS$team==trTeams$Team[i],]$Seed),2,3))-
                       as.numeric(substr(as.character(thisSeasonRS[thisSeasonRS$team==trTeams$Team[j],]$Seed),2,3)),
                     auxA_B)
      }
      names(aux_All)[1] <- "Season"
      names(aux_All)[2] <- "teamA_teamB"
      names(aux_All)[3] <- "seedDiff"
      testing <- rbind(testing,as.data.frame(aux_All))
    }
  }
}  



