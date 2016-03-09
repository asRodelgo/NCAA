#############################################
# NCAA Prediction Competition kaggle.com
#
# alberto.sanchez.rodelgo@gmail.com
#############################################
require(plyr)
require(dplyr)
require(tidyr)
########### DATA PREPARATION PART STARTS HERE ##############

# loading the data (I previously downloaded into a local folder)
regSeason <- read.csv(file = "data/RegularSeasonCompactResults.csv")
teams <- read.csv(file = "data/Teams.csv")
seasons <- read.csv(file = "data/Seasons.csv")
tourney <- read.csv(file = "data/TourneyCompactResults.csv")
tourSeeds <- read.csv(file = "data/TourneySeeds.csv")
regSeasonDet <- read.csv(file = "data/RegularSeasonDetailedResults.csv")
tourneyDet <- read.csv(file = "data/TourneyDetailedResults.csv")
############### PARAMETERS ################################

logloss <- data.frame(NA) # evaluation data.frame
submit <- data.frame(NA) # submission file
superFinal <- data.frame(NA) # final file

# Select factor: (adjusting middle probabilities limits)
factor <- 0.9 # probs greater than factor will be taken to the extreme value
extreme <- 0.001 # how far from prob=0 or =1
# error factor: std of results per team, measures the risk of predicting probs.
errFactor <- 1 # weight for stdev. stdev < 1 empowers extreme probs, 
# > 1 becomes more conservative
seedPen <- 0.024 # % penalty associated to difference in seed
# which seasons to include in simulations
seasons <- c("S")
# c("A","B","C","D","E","F","G","H","I","J",
#           "K","L","M","N","O","P","Q","R","S")
###########################################################

############### RUNNING SIMULATION ################################

for (season in seasons){ # loop through selected seasons
  regSeasonA <- regSeason[regSeason$season==season,] # prepare my data frame
  regSeasonA$dscore <- regSeasonA$wscore - regSeasonA$lscore
  mydata <- data.frame(teamA=regSeasonA$wteam,scoreA=regSeasonA$wscore,
                       teamB=regSeasonA$lteam, scoreB=regSeasonA$lscore,
                       wloc=regSeasonA$wloc,dscore=regSeasonA$dscore,
                       daynum=regSeasonA$daynum)
  nrows <- nrow(mydata) # will have all teams' games as rows, i.e., duplicate regSeasonA
  x <- nrows + 1
  y <- 2*nrows
  mydata[x:y,1] <- mydata[1:nrows,3]
  mydata[x:y,2] <- mydata[1:nrows,4]
  mydata[x:y,3] <- mydata[1:nrows,1]
  mydata[x:y,4] <- mydata[1:nrows,2]
  mydata[x:y,5] <- mydata[1:nrows,5]
  mydata[x:y,6] <- mydata[1:nrows,6]*(-1)
  mydata[x:y,7] <- mydata[1:nrows,7]
  # now order by team ID on teamA
  mydata <- mydata[do.call(order,mydata),]
  
  # Calculate overall values (including all teams and all regular seasons)
  # to serve as benchmark for gauging performance
  ########### Or should I calculate these numbers per season???
  # avg scoring
  avgTot <- (sum(regSeason[,4]) + sum(regSeason[,6]))/(2*nrow(regSeason))
  # stdev scoring
  aux <- nrow(regSeason) + 1
  auxRows <- nrow(regSeason)*2
  z <- data.frame()
  z[1:nrow(regSeason),1] <- regSeason[,4]
  z[aux:auxRows,1] <- regSeason[,6]
  stdTot <- sd(z[,])
  # limits for offensive/defensive teams: These limits will have to be adjusted
  goodOff <- avgTot + 0.2*stdTot
  poorOff <- avgTot - 0.2*stdTot
  # Initiate new colums in mydata. I need to compute the following for every team:
  mydata$off <- NA
  mydata$def <- NA
  mydata$offcons <- NA
  mydata$defcons <- NA
  sdOff <- c(NA)
  sdDef <- c(NA)
  
  for (i in 501:856){ # stdev for this particular season, don't expect to vary much 
    # from that of all seasons
    sdOff[i] <- sd(mydata[mydata[,1]==i,2])
    sdDef[i] <- sd(mydata[mydata[,1]==i,4])
  }
  avgStdOff <- mean(sdOff,na.rm=TRUE)
  avgStdDef <- mean(sdDef,na.rm=TRUE)
  stdStdOff <- sd(sdOff,na.rm=TRUE)
  stdStdDef <- sd(sdDef,na.rm=TRUE)
  # as expected they have similar values, but I take their average:
  avgStd <- (avgStdOff+avgStdDef)/2
  stdStd <- (stdStdOff+stdStdDef)/2
  goodCons <- avgStd - stdStd 
  poorCons <- avgStd + stdStd
  for (i in 501:856){ # add off, def, offcons, defcons to mydata
    if (is.na(mean(mydata[mydata[,1]==i,2])) == FALSE){
      mydata$off[mydata[,1]==i] <- mean(mydata[mydata[,1]==i,2])/avgTot
      mydata$def[mydata[,1]==i] <- mean(mydata[mydata[,1]==i,4])/avgTot  
      mydata$offcons[mydata[,1]==i] <- sdOff[i]/avgStd
      mydata$defcons[mydata[,1]==i] <- sdDef[i]/avgStd  
    }
  }
  
  ## LOOP IT
  # calculate team indicators for all teams:
  myteams <- data.frame() # initialize my final data file
  for (s in 1:356){ # loop through all the teams
    t <- s + 500
    if (is.na(mean(mydata[mydata[,1]==t,2])) == FALSE){ # whether such team played at all
      team1 <- mydata[mydata$teamA == t,] # auxiliar local team data.frame
      v <- c() # auxiliar local vector to store the required data
      v[1:19] <- 0 # initialize to zero
      v[1] <- mean(team1[,2])
      v[2] <- sd(team1[,2])
      v[3] <- mean(team1[,4])
      v[4] <- sd(team1[,4])
      numWins <- nrow(team1[team1[,6]>0,]) # number of wins
      v[5] <- numWins/nrow(team1) # win perc
    # I'm commenting all this piece of code as proved irrelevant in some 
    # earlier tests I run
#       if (numWins > 0){ # otherwise teams with 0 wins would have infinite percent.
#         # win perc scoring high
#         v[6] <- nrow(team1[(team1[,6]>0) & (team1[,2]>v[1]),])/numWins
#         # win perc against low scoring
#         v[7] <- nrow(team1[(team1[,6]>0) & (team1[,4]<v[3]),])/numWins
#         # win perc scoring low
#         v[8] <- nrow(team1[(team1[,6]>0) & (team1[,2]<v[1]),])/numWins
#         # win perc against high scoring 
#         v[9] <- nrow(team1[(team1[,6]>0) & (team1[,4]>v[3]),])/numWins
#       } else v[6] <- v[7] <- v[8] <- v[9] <- 0
#       # calculate v10 - v19
#       n <- nrow(team1) # games played by this team1
#       c <- c() # auxiliar local vector to count things
#       c[1:19] <- 0 # initialize to zero
#       #c18 <- c19 <- 0 # initialize games played against good defense/offense
#       for (j in 1:n){ # loop through number of games played by team1
#         if (mydata[mydata[,1]==mydata[j,3],9][1] > 1.1){
#           c[10] <- c[10] + 1
#           if (team1[,6][j] > 0) {
#             v[10] <- v[10] + 1 # win and opponent has not consistent offense
#           }
#         }
#         if (mydata[mydata[,1]==mydata[j,3],10][1] > 1.1){
#           c[11] <- c[11] + 1
#           if (team1[,6][j] > 0) {  
#             v[11] <- v[11] + 1 # win and opponent has not consistent defense
#           }
#         }  
#         if (mydata[mydata[,1]==mydata[j,3],9][1] < 0.9){
#           c[12] <- c[12] + 1
#           if (team1[,6][j] > 0){
#             v[12] <- v[12] + 1 # win and opponent has consistent offense
#           }
#         }
#         if (mydata[mydata[,1]==mydata[j,3],10][1] < 0.9){
#           c[13] <- c[13] + 1
#           if (team1[,6][j] > 0){
#             v[13] <- v[13] + 1 # win and opponent has consistent defense
#           }
#         }
#         if (mydata[mydata[,1]==mydata[j,3],7][1] > 1.1){
#           c[14] <- c[14] + 1
#           if (team1[,6][j] > 0){
#             v[14] <- v[14] + 1 # win and opponent has good offense
#           }
#         }
#         if (mydata[mydata[,1]==mydata[j,3],8][1] > 0.9){
#           c[15] <- c[15] + 1
#           if (team1[,6][j] > 0){
#             v[15] <- v[15] + 1 # win and opponent has not good defense
#           }
#         }
#         if (mydata[mydata[,1]==mydata[j,3],7][1] < 1.1){
#           c[16] <- c[16] + 1
#           if (team1[,6][j] > 0){
#             v[16] <- v[16] + 1 # win and opponent has not good offense
#           }
#         }
#         if (mydata[mydata[,1]==mydata[j,3],8][1] < 0.9){
#           c[17] <- c[17] + 1
#           if (team1[,6][j] > 0){
#             v[17] <- v[17] + 1 # win and opponent has good defense
#           }
#         }
#         if (mydata[mydata[,1]==mydata[j,3],8][1] < 0.9){
#           v[18] <- v[18] + team1[,2][j]  # points against an opponent with good defense
#           c[18] <- c[18] + 1
#         }
#         if (mydata[mydata[,1]==mydata[j,3],7][1] > 1.1){
#           v[19] <- v[19] + team1[,2][j]  # points against an opponent with good offense
#           c[19] <- c[19] + 1  
#         }
#       }
#       for (i in 10:17){
#         if (c[i]>0){
#           v[i] <- v[i]/c[i]
#         }
#       }
#       if (c[18] > 0){
#         v[18] <- v[18]/c[18]  
#       }
#       if (c[19] > 0){
#         v[19] <- v[19]/c[19]  
#       }
#       
      myteams[s,1] <- team1$teamA[1]
      myteams[s,2] <- team1$off[1]
      myteams[s,3] <- team1$def[1]
      myteams[s,4] <- team1$offcons[1]
      myteams[s,5] <- team1$defcons[1]
      for (m in 1:19){
        myteams[s,m+5] <- v[m]
      }
      
    }
  }
  myteams[,25] <- myteams[,2]/myteams[,3]
  myteams[,26] <- (myteams[,4]+myteams[,5])/2
  #change names of variables in myteams
  colnames(myteams) <- c("teamID","off","def","offcons","defcons","avgPoints",
                         "stdPoints","avgPointsA","stdPointsA","winPerc","winPercHiScore",
                         "winPercAloScore","winPercloScore","winPercAhiScore",
                         "winPercApoorConsOff","winPercApoorConsDef","winPercAgoodConsOff",
                         "winPercAgoodConsDef","winPercAgoodOff","winPercAnogoodDef",
                         "winPercAnogoodOff","winPercAgoodDef","avgPointsAgoodDef",
                         "avgPointsAgoodOff","ratioOffDef", "error")
  ####################################################
  ## Description of variables:
  #  1. teamID
  #  2. off: 1 if team scores more points than average + some coefficient, else 0 
  #  3. def: 1 if team gets scored less points than average - some coefficient, else 0
  #  4. offcons: 1 if team's points stdev is less than average
  #  5. defcons: 1 if team's points against stdev is less than average
  #  25. off/def
  #  26. error: (offcons + defcons)/2
  ###########################################################
  # Adding tourney and seed data
  myteams <- myteams[complete.cases(myteams[,1]),] # eliminate NAs
  # filter myteams by ncaa tourney seeded teams and their seed
  tourA <- tourney[tourney$season==season,] # ncaa teams
  
  seedA <- tourSeeds[tourSeeds$season==season,] # ncaa seeds
  colnames(seedA) <- c("season","seed","teamID")
  colnames(teams) <- c("teamID","teamName")
  seedA <- merge(seedA,teams,by="teamID") 
  seedA2 <- seedA
  seedA2$seed <- substr(seedA$seed,2,3) # seed and conference apart
  seedA2$region <- substr(seedA$seed,1,1)
  seedA2 <- seedA2[order(seedA2[,3]),] # sort asc by team ID
  # if predicting the current season, tourA will be empty so tourA2 too
  tourA2 <- data.frame()
  tourA2[1:(2*(nrow(tourA))),1] <- tourA[1:nrow(tourA),1] # season
  tourA2[1:nrow(tourA),2] <- tourA[1:nrow(tourA),3] # list of teams (wteam + lteam)
  tourA2[(nrow(tourA)+1):(2*(nrow(tourA))),2] <- tourA[1:nrow(tourA),5]
  tourA2 <- tourA2[order(tourA2[,2]),] # sort asc by team ID
  tourA2 <- tourA2[!duplicated(tourA2),] # remove duplicates
  colnames(tourA2) <- c("season","teamID")
  
  myteams <- merge(myteams,seedA2,by="teamID")
  myteams <- myteams[!duplicated(myteams),]
  #myteams <- merge(myteams,seedA2,by="teamID")
  myteams$seed <- as.numeric(myteams$seed)
  ###############################################################
  # Calculate final data.frame with base probabilities (which I later on adjust)
  final <- data.frame()
  k <- 1
  for (i in 1:nrow(myteams)) {
    for (j in i+1:nrow(myteams)) {
      if ((is.na(myteams[i,1]) == FALSE) && (is.na(myteams[j,1]) == FALSE)) {
        final[k,1] <- paste(season,"_",myteams[i,1],"_",myteams[j,1])
        final[k,1] <- gsub(" ","",final[k,1])
        if ((myteams[i,25] > myteams[j,25])) {
          final[k,2] <- myteams[i,25]/myteams[j,25] - 0.5
        }
        else {
          final[k,2] <- 1.5 - myteams[j,25]/myteams[i,25]
        }
        final[k,3] <- (myteams[i,26] + myteams[j,26])/2
        final[k,4] <- myteams$seed[i] - myteams$seed[j]
        final[k,5] <- paste(myteams$teamName[i],"v.",myteams$teamName[j])
        k <- k+1
      }
    }
  }
  ## Tourney data
  
  ## Model: probFin <- prob - seedDif*seedPen +- consistency adjustment
  for (i in 1:nrow(final)) {
    final$probFin[i] <- final[i,2] - final[i,4]*seedPen
    if (final$probFin[i] < 0.5){
      final$probFin[i] <- final$probFin[i] - errFactor*((1-final[i,3])/10)
    } else final$probFin[i] <- final$probFin[i] + errFactor*((1-final[i,3])/10)
    if (final$probFin[i] < 0){
      final$probFin[i] <- 0
    }  
    if (final$probFin[i] > 1){
      final$probFin[i] <- 1
    }
  }
  for (i in 1:nrow(final)) {
    final$probFin2[i] <- final$probFin[i] 
    if (final$probFin[i] < (1-factor)){
      final$probFin2[i] <- extreme
    }  
    if (final$probFin[i] > factor){
      final$probFin2[i] <- (1-extreme)
    }
  }
  for (i in 1:nrow(final)) { # another approach, try to minimize errors around 0.5
    final$probFin3[i] <- final$probFin[i] 
    if ((final$probFin[i] < 0.65) && (final$probFin[i] > 0.5)){
        if ((final[,4] < 6) && (final[,4] > 0)){
          final$probFin3[i] <- 0.5
        }
        if ((final[,4] > -6) && (final[,4] <= 0)){
          final$probFin3[i] <- 0.65
        }
    }
    if ((final$probFin[i] > 0.35) && (final$probFin[i] < 0.5)){
      if ((final[,4] < 6) && (final[,4] > 0)){
        final$probFin3[i] <- 0.35
      }
      if ((final[,4] > -6) && (final[,4] <= 0)){
        final$probFin3[i] <- 0.5
      }
    }
  }
  ##################################################
  # merge final with tourA containing winners & losers
  # only if tourA is not empty, that is, contains historical data
  mytour <- data.frame()
  for (i in 1:nrow(tourA)) {
    if (tourA[i,5] < tourA[i,3]){
      mytour[i,1] <- paste(paste(season,"_",tourA[i,5],"_",tourA[i,3]))
      mytour[i,1] <- gsub(" ","",mytour[i,1])
      mytour[i,2] <- 0
    } else {  
      mytour[i,1] <- paste(paste(season,"_",tourA[i,3],"_",tourA[i,5]))
      mytour[i,1] <- gsub(" ","",mytour[i,1])
      mytour[i,2] <- 1 # teams are already arranged by winners in the 3rd column
    }
  }
  mytour <- merge(mytour,final,by="V1")
  ###################### EVALUATION PART ############################
  ## Calculate the LogLoss function to evaluate submissions:
  success <- 0
  for (i in 1:nrow(mytour)) {
    if (mytour[i,2]==0){
      mytour$log1[i] <- log(1-mytour[i,3])
      mytour$log2[i] <- log(1-mytour[i,6])
      mytour$log3[i] <- log(1-mytour[i,7])
      mytour$log4[i] <- log(1-mytour[i,8])
      if (mytour$probFin[i] < 0.5) {
        success <- success + 1
      }
        
    } else {
      mytour$log1[i] <- log(mytour[i,3]) 
      mytour$log2[i] <- log(mytour[i,6])
      mytour$log3[i] <- log(mytour[i,7])
      mytour$log4[i] <- log(mytour[i,8])
      if (mytour$probFin[i] > 0.5) {
        success <- success + 1
      }
    }
  }
  # accumulate mytour for selected seasons:
  rowsize <- nrow(superFinal)
  if (rowsize == 1){
    for (i in 1:11){
      superFinal[rowsize:(nrow(mytour)+rowsize-1),i] <- mytour[,i]
    }  
  } else {
    for (i in 1:11){
      superFinal[(rowsize+1):(nrow(mytour)+rowsize),i] <- mytour[,i]
    }
  }
  #   a <- superFinal[abs(superFinal$result - superFinal$prob) > 0.6,4] 
  #   b <- superFinal[abs(superFinal$result - superFinal$prob) <= 0.4,4]
  #   mean(a)
  #   mean(b)
  
  # Calculate logloss for 1,2,3
  rowsize3 <- nrow(logloss)
  if (rowsize3 == 1){
    logloss[2,1] <- season
    logloss[2,2] <- -sum(mytour$log1)/nrow(mytour)
    logloss[2,3] <- -sum(mytour$log2)/nrow(mytour)
    logloss[2,4] <- -sum(mytour$log3)/nrow(mytour)
    logloss[2,5] <- -sum(mytour$log4)/nrow(mytour)
    logloss[2,6] <- factor
    logloss[2,7] <- errFactor
    logloss[2,8] <- seedPen
    logloss[2,9] <- success/nrow(mytour)
  } else {
    logloss[rowsize3+1,1] <- season
    logloss[rowsize3+1,2] <- -sum(mytour$log1)/nrow(mytour)
    logloss[rowsize3+1,3] <- -sum(mytour$log2)/nrow(mytour)
    logloss[rowsize3+1,4] <- -sum(mytour$log3)/nrow(mytour)
    logloss[rowsize3+1,5] <- -sum(mytour$log4)/nrow(mytour)
    logloss[rowsize3+1,6] <- factor
    logloss[rowsize3+1,7] <- errFactor
    logloss[rowsize3+1,8] <- seedPen
    logloss[rowsize3+1,9] <- success/nrow(mytour)
  }      
} # end of loop

# those incorrectly assigned are put out into a file for further study
sF <- superFinal[abs(superFinal$result - superFinal$probFin1) > 0.5,]
write.csv(sF,file="incorrect.csv", row.names=FALSE)

######################## SUBMISSION PART ###########################
  # create file for submission: # 5 without the final adjustment for extremes (0,1)
  # Every run calcultes 3 types of probabilites:
  # Test which is best for the final submission
  # 1 final[,2] : probabilites from ratioOff/Def
  # 2 final[,5] : probabilites from Model
  # 3 final[,6] : probabilites from Model + adjustment on extreme probs
  rowsize2 <- nrow(submit)
  if (rowsize2 == 1){
    submit[rowsize2:(nrow(final)+rowsize2-1),1] <- final[,1]
    submit[rowsize2:(nrow(final)+rowsize2-1),2] <- final[,7] # select prob 1 ,2 ,3
  } else {
    submit[(rowsize2+1):(nrow(final)+rowsize2),1] <- final[,1]
    submit[(rowsize2+1):(nrow(final)+rowsize2),2] <- final[,7]# select prob 1 ,2 ,3
  }

# tidy datasets by naming their columns
colnames(superFinal) <- c("id","result","prob","stdev","seedDiff","probFin1",
                          "probFin2","log1","log2","log3")
colnames(submit) <- c("id","pred")
colnames(logloss) <- c("season","prob1","prob2","prob3","prob4","factor","errFactor",
                       "seePen", "successPerc")
# write logloss output files:
mean(logloss[,2],na.rm=TRUE)
mean(logloss[,3],na.rm=TRUE)
mean(logloss[,4],na.rm=TRUE)
mean(logloss[,5],na.rm=TRUE)
# write final file for current season
write.csv(final,file=paste("outputFiles/final.csv"),
          row.names=FALSE)

write.csv(logloss,file=paste("outputFiles/logloss",
                             factor,"_",errFactor,"_",seedPen,".csv"), row.names=FALSE)
# write the output submission file:
write.csv(submit,file="submit2.csv", row.names=FALSE)