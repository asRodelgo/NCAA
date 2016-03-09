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

training <- data.frame()
for (s in unique(trTeams$Season)){
  thisSeason <- trTeams[trTeams$Season==s,]
  thisSeasonRS <- rsTeamStats[rsTeamStats$Season==s,]
  for (i in 1:nrow(thisSeason)){
    auxA <- thisSeasonRS[thisSeasonRS$team==thisSeason$teamA[i],3:(ncol(thisSeasonRS)-2)]
    auxB <- thisSeasonRS[thisSeasonRS$team==thisSeason$teamB[i],3:(ncol(thisSeasonRS)-2)]
    if (is.wholenumber(i/2)){ # balance results to model differences in scores not margin of wins
      auxA_B <- auxA - auxB
      aux_All <- c(s,paste0(thisSeason$teamA[i],"_",thisSeason$teamB[i]),auxA_B,thisSeason$y_score[i])
    } else {
      auxA_B <- auxB - auxA
      aux_All <- c(s,paste0(thisSeason$teamB[i],"_",thisSeason$teamA[i]),auxA_B,-thisSeason$y_score[i])
    }
    names(aux_All)[1] <- "Season"
    names(aux_All)[2] <- "teamA_teamB"
    names(aux_All)[length(aux_All)] <- "y_score"
    training <- rbind(training,as.data.frame(aux_All))
  }
}

head(select(training, Season,teamA_teamB,avgScore.x,avgScore.y,y_score))


###################################### 
########### estimate the impact of Home court advantage during regular season
home <- rsTeams %>%
  filter(loc=="H") %>%
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
home <- as.data.frame(home)
#
away <- rsTeams %>%
  filter(loc=="A") %>%
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
away <- as.data.frame(away)
# compare by a given variable:
compare_var <- "avgScore"
compareHA <- merge(home[,c("Season","team",compare_var)],away[,c("Season","team",compare_var)], by=c("Season","team"))
plot(compareHA[,3],compareHA[,4])

compareHAmeans <- compareHA %>%
  group_by(Season) %>%
  mutate(meanA = mean(avgScore.x), meanH = mean(avgScore.y), meanT = (meanA + meanH)/2) %>%
  distinct(Season)
compareHAmeans
##############
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




# ---------------------------------
.tableCompare <- function(teamA,teamB,period){
  
  data <- filter(rsTeamStats, Team_Name %in% c(teamA,teamB) & Season == period)
  
}
table2way <- .tableCompare("Wisconsin","Duke",2015)
t(table2way)
# ---------------------------------
# Play with tSNE. Let's cluster
.tSNE_compute <- function(num_iter, max_num_neighbors, period, type){
  
  data <- select(rsTeamStats, team, Season, contains(type), Seed, Team_Name)
  data <- filter(data, Season == period, !(is.na(Seed)))
  data_noLabels <- data[,-c(1,2,ncol(data)-1,ncol(data))]
  set.seed(456) # reproducitility
  tsne_points <- tsne(data_noLabels, 
                      max_iter=as.numeric(num_iter), 
                      perplexity=as.numeric(max_num_neighbors), 
                      epoch=num_iter/10)
  
  plot(tsne_points,t='n', axes=FALSE, frame.plot = FALSE, xlab = "",ylab = ""); 
  graphics::text(tsne_points,labels=as.character(data$Team_Name))#, col=as.factor(substr(as.character(rsTeamStats$Seed),1,1)))
  
}
.tSNE_compute(1000,10,2015,".y")


                     

                     
                     
                     
s1TeamStats <- filter(rsTeamStats, Season == 2015)
s1TeamStats <- select(s1TeamStats, - Season)
#head(arrange(s1TeamStats, desc(avgScore)))

s1Clusters <- hclust(dist(s1TeamStats))
plot(s1Clusters)  
  
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
# ####### Correlation
# #
# # identifying correlated predictors. Remove those with high correlation
# require(corrplot)
# #descrCor <- pairs(training)
# summary(descrCor[upper.tri(descrCor)])
# # remove linearly correlated variables > corr_cutoff
# highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
# training <- training[,-highlyCorDescr]
# # corrplot(descrCor)
# #
# ####### Collinearity
# #
# comboInfo <- findLinearCombos(training)
# comboInfo
# if (is.null(comboInfo$remove)==FALSE) {
#   training <- training[, -comboInfo$remove]
# }
# #
# # Partition the data
# set.seed(3456)
# trainIndex <- createDataPartition(training$Dscore, p = .8,
#                                   list = FALSE,
#                                   times = 1)
# train <- training[ trainIndex,]
# valid  <- training[-trainIndex,]
# 
# ## models
# model <- "lm"
# 
# # without parallelization this takes about an hour to run
# set.seed(1)
# lmTune <- train(Dscore ~., data=train, 
#                  method="lm",
#                  metric="RMSE", 
#                  verbose=FALSE)
# #######
# # The results
# 
# lmPred <- predict(lmTune, train)
# str(lmPred)
# validScores <- valid$Dscore
# valid2 <- select(valid,-Dscore)
# lmPred <- predict(lmTune, newdata = valid2)
# compare <- cbind(validScores, lmPred)
# 
# # variable importance
# lmImp <- varImp(lmTune, scale = TRUE)
# lmImp
# #gbmImp <- arrange(gbmImp$importance, -Overall)
# lmImp <- data.frame(variable = rownames(lmImp$importance), 
#                      importance = lmImp$importance$Overall)
# lmImp <- arrange(lmImp, -importance)
