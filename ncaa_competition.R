#############################################
# NCAA Prediction Competition kaggle.com
#
# alberto.sanchez.rodelgo@gmail.com
#############################################

########### DATA PREPARATION PART STARTS HERE ##############

# loading the data
regSeason <- read.csv(file = "/Users/asanchez3/Desktop/Data Analysis/NCAA/regSeason.csv")
teams <- read.csv(file = "/Users/asanchez3/Desktop/Data Analysis/NCAA/teams.csv")
seasons <- read.csv(file = "/Users/asanchez3/Desktop/Data Analysis/NCAA/seasons.csv")
tourney <- read.csv(file = "/Users/asanchez3/Desktop/Data Analysis/NCAA/tourney_results.csv")
tourSeeds <- read.csv(file = "/Users/asanchez3/Desktop/Data Analysis/NCAA/tourney_seeds.csv")

## my hypothesis: points scored and allowed follow Binomial -> Normal distributions.
# compare with scores distributions:
#wscore <- table(regSeason$wscore)
#lscore <- table(regSeason$lscore)
#plot(wscore)
#plot(lscore)
## all scores together
#all_scores <- c(regSeason$wscore,regSeason$lscore)
#ascore <- table(all_scores)
#plot(ascore)
## The idea: number of points and wins per team depends on what?.
# I need to load regular_season_results.csv
# teams.csv to merge and identify each team
# seasons.csv to test my prediction model per season.
# Start with the first season and then replicate and extend to the rest
############################################
## Aprox. running time: 6.5 minutes
# evaluation data.frame
logloss <- data.frame(NA)
# submission file:
submit <- data.frame(NA)
# final file
superFinal <- data.frame(NA)
# Select Season:
season <- "A"
# Select factor: (adjusting middle probabilities limits)
factor <- 0.8 # probs greater than factor will be taken to the extreme value
# error factor: std of results per team, measures the risk of predicting probs.
errFactor <- 1 # 1 is average. < 1 smaller error, > 1 bigger error
seedPen <- 0.015 # % penalty associated to difference in seed
############################################
# Results, unless specified: fact: 0.68, errF:0.9 
# A: 0.58
# B: 0.61
# C: Inf/Inf(0.69)/Inf(0.7)
# D: 0.56
# E: Inf/Inf(0.69)/Inf(0.7)
# F: 0.57
# G: Inf/Inf(0.7)/Inf(0.7)
# H: 0.60
# J: 0.57
# K: 0.63
# L: 0.57
# M: 0.54
# N: 0.58
# O: 0.60
# P: Inf/0.66(0.7)
# Q: 0.63
# R: 0.63/0.60(err=1.1)
seasons <- c("N","O","P","Q","R")
for (season in seasons){
  regSeasonA <- regSeason[regSeason$season==season,] # prepare my data frame
  regSeasonA$dscore <- regSeasonA$wscore - regSeasonA$lscore
  mydata <- data.frame(teamA=regSeasonA$wteam,scoreA=regSeasonA$wscore,
                       teamB=regSeasonA$lteam, scoreB=regSeasonA$lscore,
                       wloc=regSeasonA$wloc,dscore=regSeasonA$dscore)
  nrows <- nrow(mydata) # will have all teams' games as rows, i.e., duplicate regSeasonA
  x <- nrows + 1
  y <- 2*nrows
  mydata[x:y,1] <- mydata[1:nrows,3]
  mydata[x:y,2] <- mydata[1:nrows,4]
  mydata[x:y,3] <- mydata[1:nrows,1]
  mydata[x:y,4] <- mydata[1:nrows,2]
  mydata[x:y,5] <- mydata[1:nrows,5]
  mydata[x:y,6] <- mydata[1:nrows,6]*(-1)
  # now order by team ID on teamA
  mydata <- mydata[do.call(order,mydata),]
  
  # Calculate overall values (including all teams and all regular seasons)
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
  ##############################################################################
  ############# PARAMETER: Level of offense coefficient ################
  ##############################################################################
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
      if (numWins > 0){ # otherwise teams with 0 wins would have infinite percent.
        # win perc scoring high
        v[6] <- nrow(team1[(team1[,6]>0) & (team1[,2]>v[1]),])/numWins
        # win perc against low scoring
        v[7] <- nrow(team1[(team1[,6]>0) & (team1[,4]<v[3]),])/numWins
        # win perc scoring low
        v[8] <- nrow(team1[(team1[,6]>0) & (team1[,2]<v[1]),])/numWins
        # win perc against high scoring 
        v[9] <- nrow(team1[(team1[,6]>0) & (team1[,4]>v[3]),])/numWins
      } else v[6] <- v[7] <- v[8] <- v[9] <- 0
      # calculate v10 - v19
      n <- nrow(team1) # games played by this team1
      c <- c() # auxiliar local vector to count things
      c[1:19] <- 0 # initialize to zero
      #c18 <- c19 <- 0 # initialize games played against good defense/offense
      for (j in 1:n){ # loop through number of games played by team1
        if (mydata[mydata[,1]==mydata[j,3],9][1] > 1.1){
          c[10] <- c[10] + 1
          if (team1[,6][j] > 0) {
            v[10] <- v[10] + 1 # win and opponent has not consistent offense
          }
        }
        if (mydata[mydata[,1]==mydata[j,3],10][1] > 1.1){
          c[11] <- c[11] + 1
          if (team1[,6][j] > 0) {  
            v[11] <- v[11] + 1 # win and opponent has not consistent defense
          }
        }  
        if (mydata[mydata[,1]==mydata[j,3],9][1] < 0.9){
          c[12] <- c[12] + 1
          if (team1[,6][j] > 0){
            v[12] <- v[12] + 1 # win and opponent has consistent offense
          }
        }
        if (mydata[mydata[,1]==mydata[j,3],10][1] < 0.9){
          c[13] <- c[13] + 1
          if (team1[,6][j] > 0){
            v[13] <- v[13] + 1 # win and opponent has consistent defense
          }
        }
        if (mydata[mydata[,1]==mydata[j,3],7][1] > 1.1){
          c[14] <- c[14] + 1
          if (team1[,6][j] > 0){
            v[14] <- v[14] + 1 # win and opponent has good offense
          }
        }
        if (mydata[mydata[,1]==mydata[j,3],8][1] > 0.9){
          c[15] <- c[15] + 1
          if (team1[,6][j] > 0){
            v[15] <- v[15] + 1 # win and opponent has not good defense
          }
        }
        if (mydata[mydata[,1]==mydata[j,3],7][1] < 1.1){
          c[16] <- c[16] + 1
          if (team1[,6][j] > 0){
            v[16] <- v[16] + 1 # win and opponent has not good offense
          }
        }
        if (mydata[mydata[,1]==mydata[j,3],8][1] < 0.9){
          c[17] <- c[17] + 1
          if (team1[,6][j] > 0){
            v[17] <- v[17] + 1 # win and opponent has good defense
          }
        }
        if (mydata[mydata[,1]==mydata[j,3],8][1] < 0.9){
          v[18] <- v[18] + team1[,2][j]  # points against an opponent with good defense
          c[18] <- c[18] + 1
        }
        if (mydata[mydata[,1]==mydata[j,3],7][1] > 1.1){
          v[19] <- v[19] + team1[,2][j]  # points against an opponent with good offense
          c[19] <- c[19] + 1  
        }
      }
      for (i in 10:17){
        if (c[i]>0){
          v[i] <- v[i]/c[i]
        }
      }
      if (c[18] > 0){
        v[18] <- v[18]/c[18]  
      }
      if (c[19] > 0){
        v[19] <- v[19]/c[19]  
      }
      
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
  #  6. avgPoints
  #  7. stdPoints
  #  8. avgPointsA: avg points received
  #  9. stdPointsA: std dev of points received
  #  10. winPerc: perc of wins 
  #  11. winPercHiScore: perc of wins when scoring more than avg + coeff points
  #  12. winPercAloScore: perc of wins against teams with low score average
  #  13. winPercloScore: perc of wins when scoring less than avg + coeff points
  #  14. winPercAhiScore: perc of wins against teams with high score average
  #  15. winPercApoorConsOff
  #  16. winPercApoorConsDef
  #  17. winPercAgoodConsOff
  #  18. winPercAgoodConsDef
  #  19. winPercAgoodOff
  #  20. winPercAnogoodDef
  #  21. winPercAnogoodOff
  #  22. winPercAgoodDef
  #  23. avgPointsAgoodDef
  #  24. avgPointsAgoodOff
  #  25. off/def
  #  26. error: (offcons + defcons)/2
  
  ########### ANALYSIS PART STARTS HERE ##############
  ## Will try a binary outcome linear regression
  ## glm with binomial family. Percentage of win versus the rest of the variables
  ## quasibinomial avoids warnings as the variables values are not integers.
  ## on the other hand, choosing binomial makes the fit more conservative
#   library(corrgram)
#   library(MASS)
#   # model to fit win Percentage
#   mymodel.glm <- glm(winPerc ~ ratioOffDef + error,
#                  family="binomial" ,data=myteams)
#   summary(mymodel.glm)
#   corrgram(myteams)
  
  ####################################################
  myteams <- myteams[complete.cases(myteams[,1]),] # eliminate NAs
  # filter myteams by ncaa tourney seeded teams and their seed
  tourA <- tourney[tourney$season==season,] # ncaa teams
  
  seedA <- tourSeeds[tourSeeds$season==season,] # ncaa seeds
  colnames(seedA) <- c("season","seed","teamID")
  seedA2 <- seedA
  seedA2$seed <- substr(seedA$seed,2,3) # seed and conference apart
  seedA2$region <- substr(seedA$seed,1,1)
  
  tourA2 <- data.frame()
  tourA2[1:(2*(nrow(tourA))),1] <- tourA[1:nrow(tourA),1] # season
  tourA2[1:nrow(tourA),2] <- tourA[1:nrow(tourA),3] # list of teams (wteam + lteam)
  tourA2[(nrow(tourA)+1):(2*(nrow(tourA))),2] <- tourA[1:nrow(tourA),5]
  tourA2 <- tourA2[order(tourA2[,2]),] # sort asc by team ID
  tourA2 <- tourA2[!duplicated(tourA2),] # remove duplicates
  colnames(tourA2) <- c("season","teamID")
  myteams <- merge(myteams,tourA2,by="teamID")
  myteams <- myteams[!duplicated(myteams),]
  myteams <- merge(myteams,seedA2,by="teamID")
  myteams$seed <- as.numeric(myteams$seed)
  #############################################
  # Regression analysis fits right here. myteams is the comprehensive dataset
#   mymodel.glm <- glm(winPerc ~ seed,
#                      family="quasibinomial" ,data=myteams)
#   summary(mymodel.glm)
#   corrgram(myteams)
  #############################################
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
        k <- k+1
      }
    }
  }
  ## Tourney data

  ## Method A: probFin <- prob - seedDif*seedPen +- consistency adjustment
  for (i in 1:nrow(final)) {
    final$probFin[i] <- final[i,2] - final[i,4]*seedPen
    if (final$probFin[i] < 0.5){
      final$probFin[i] <- final$probFin[i] - ((1-final[i,3])/10)
    } else final$probFin[i] <- final$probFin[i] + ((1-final[i,3])/10)
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
      final$probFin2[i] <- 0.01
    }  
    if (final$probFin[i] > factor){
      final$probFin2[i] <- 0.99
    }
  }  
  ## Method B: Adjust probabilites based on seeds and stdev of off and def
#   for (i in 1:nrow(final)) {
#     if ((final[i,2]>=factor) && (final[i,2]<=0)){ # extreme factor
#       final[i,2] <- 1
#     }  
#     if ((final[i,2]<=(1-factor)) && (final[i,2]>=0)){ # extreme factor
#       final[i,2] <- 0
#     }
#     # adjust according to seed difference: from regression: 0.047 factor in winPerc
#     if ((final[i,2]<=0.4) && (final[i,2]>=0.3)){ # extreme factor
#       if (final[i,4]>5){
#         final[i,2] <- 0.3
#       }
#       else if (final[i,4]>= 0){
#         final[i,2] <- 0.4
#       }
#       else if (final[i,4]>= -5){
#         final[i,2] <- 0.45
#       }
#       else if (final[i,4]< -5){
#         final[i,2] <- 0.5
#       }
#     }
#     if ((final[i,2]<=0.5) && (final[i,2]>0.4)){ # extreme factor
#       if (final[i,4]>5){
#         final[i,2] <- 0.4
#       }
#       else if (final[i,4]>= 0){
#         final[i,2] <- 0.45
#       }
#       else if (final[i,4]>= -5){
#         final[i,2] <- 0.55
#       }
#       else if (final[i,4]< -5){
#         final[i,2] <- 0.6
#       }
#     }
#     if ((final[i,2]>=0.6) && (final[i,2]<=0.7)){ # extreme factor
#       if (final[i,4]<-5){
#         final[i,2] <- 0.7
#       }
#       else if (final[i,4]<= 0){
#         final[i,2] <- 0.6
#       }
#       else if (final[i,4]<= 5){
#         final[i,2] <- 0.55
#       }
#       else if (final[i,4]> 5){
#         final[i,2] <- 0.5
#       }
#     }
#     if ((final[i,2]>=0.5) && (final[i,2]<0.6)){ # extreme factor
#       if (final[i,4]<-5){
#         final[i,2] <- 0.6
#       }
#       else if (final[i,4]<= 0){
#         final[i,2] <- 0.55
#       }
#       else if (final[i,4]<= 5){
#         final[i,2] <- 0.45
#       }
#       else if (final[i,4]> 5){
#         final[i,2] <- 0.4
#       }
#     }
    # adjust the middle probabilites with stdev behaviour 
    # where mistakes are very risky
#     if ((mytour[i,3] > 0.5) && (mytour[i,3] < factor) && (mytour[i,4] > errFactor)) {
#       mytour[i,3] <- mytour[i,3] - mytour[i,4] + 1
#       if (mytour[i,3] < 0.6) {
#         mytour[i,3] <- 0.6
#       }
#     }
#     if ((mytour[i,3] < 0.5) && (mytour[i,3] > (1-factor)) && (mytour[i,4] > errFactor)) {
#       mytour[i,3] <- mytour[i,3] + mytour[i,4] - 1
#       if (mytour[i,3] > 0.4) {
#         mytour[i,3] <- 0.4
#       }
#     }  
#  }
  ##################################################
  # Calculate logloss from final merging with tourA containing winners & losers
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
  ## Calculate the LogLoss function to evaluate submissions:
  #mytour$log <-NA
  for (i in 1:nrow(mytour)) {
    if (mytour[i,2]==0){
      mytour$log1[i] <- log(1-mytour[i,3])
      mytour$log2[i] <- log(1-mytour[i,6])
      mytour$log3[i] <- log(1-mytour[i,7])
    } else {
      mytour$log1[i] <- log(mytour[i,3]) 
      mytour$log2[i] <- log(mytour[i,6])
      mytour$log3[i] <- log(mytour[i,7])
    }
  }
  # accumulate mytour for selected seasons:
  rowsize <- nrow(superFinal)
  if (rowsize == 1){
    for (i in 1:9){
      superFinal[rowsize:(nrow(mytour)+rowsize-1),i] <- mytour[,i]
    }  
  } else {
    for (i in 1:9){
      superFinal[(rowsize+1):(nrow(mytour)+rowsize),i] <- mytour[,i]
    }
  }
  # create file for submission: # 5 without the final adjustment for extremes (0,1)
  rowsize2 <- nrow(submit)
  if (rowsize2 == 1){
    submit[rowsize2:(nrow(final)+rowsize2-1),1] <- final[,1]
    submit[rowsize2:(nrow(final)+rowsize2-1),2] <- final[,6] 
  } else {
    submit[(rowsize2+1):(nrow(final)+rowsize2),1] <- final[,1]
    submit[(rowsize2+1):(nrow(final)+rowsize2),2] <- final[,6]
  }
#   for (i in 1:nrow(mytour)) {
#     if (mytour[i,2]==0){
#       mytour$log <- log(1-mytour[i,3])
#     } else mytour[i,5] <- log(mytour[i,3]) 
#   }
  rowsize3 <- nrow(logloss)
  if (rowsize3 == 1){
    logloss[2,1] <- -sum(mytour$log1)/nrow(mytour)
    logloss[2,2] <- -sum(mytour$log2)/nrow(mytour)
    logloss[2,3] <- -sum(mytour$log3)/nrow(mytour)
  } else {
    logloss[rowsize3+1,1] <- -sum(mytour$log1)/nrow(mytour)
    logloss[rowsize3+1,2] <- -sum(mytour$log2)/nrow(mytour)
    logloss[rowsize3+1,3] <- -sum(mytour$log3)/nrow(mytour)
  }    

  ##################################################
  # analyze in detail big errors: logs less than -0.9
  ##################################################
  # remove NAs from tourA
#   myteams <- myteams[complete.cases(myteams[,1]),]
#   analyze <- data.frame()
#   j <- 1
#   for (i in 1:nrow(mytour)) {
#     if (mytour[i,5] < -0.9){
#       analyze[j,1] <- substr(mytour[i,1],3,5)
#       analyze[j,2] <- myteams[myteams$teamID == analyze[j,1],2]
#       analyze[j,3] <- myteams[myteams$teamID == analyze[j,1],3]
#       analyze[j,4] <- myteams[myteams$teamID == analyze[j,1],4]
#       analyze[j,5] <- myteams[myteams$teamID == analyze[j,1],5]
#       analyze[j,6] <- myteams[myteams$teamID == analyze[j,1],6]
#       analyze[j,7] <- myteams[myteams$teamID == analyze[j,1],7]
#       analyze[j,8] <- myteams[myteams$teamID == analyze[j,1],8]
#       analyze[j,9] <- myteams[myteams$teamID == analyze[j,1],9]
#       analyze[j,10] <- substr(mytour[i,1],7,9)
#       analyze[j,11] <- myteams[myteams$teamID == analyze[j,10],2]
#       analyze[j,12] <- myteams[myteams$teamID == analyze[j,10],3]
#       analyze[j,13] <- myteams[myteams$teamID == analyze[j,10],4]
#       analyze[j,14] <- myteams[myteams$teamID == analyze[j,10],5]
#       analyze[j,15] <- myteams[myteams$teamID == analyze[j,10],6]
#       analyze[j,16] <- myteams[myteams$teamID == analyze[j,10],7]
#       analyze[j,17] <- myteams[myteams$teamID == analyze[j,10],8]
#       analyze[j,18] <- myteams[myteams$teamID == analyze[j,10],9]
#       analyze[j,19] <- mytour[i,2]
#       analyze[j,20] <- mytour[i,3]
#       analyze[j,21] <- myteams[myteams$teamID == analyze[j,1],25]
#       analyze[j,22] <- myteams[myteams$teamID == analyze[j,10],25]
#       analyze[j,23] <- myteams[myteams$teamID == analyze[j,1],26]
#       analyze[j,24] <- myteams[myteams$teamID == analyze[j,10],26]
#       j <- j + 1
#     } 
#   }
#   colnames(analyze) <- c("teamA","offA","defA","offconsA","defconsA","avgPointsA",
#                          "stdPointsA","avgPointsAA","stdPointsAA",
#                          "teamB","offB","defB","offconsB","defconsB","avgPointsB",
#                          "stdPointsB","avgPointsAB","stdPointsAB",
#                          "winA","pWin","powerA","powerB","errorA","errorB")
#   ## index of randomnes for a team (or error measure):
#   ## (offcons + defcons)/2 > the bigger the more unpredictable the result (error)
#   logloss

}
colnames(superFinal) <- c("id","result","prob","seedDiff","probFin1","probFin2",
                          "log1","log2","log3")
colnames(submit) <- c("id","pred")
write.csv(submit,file="submit.csv", row.names=FALSE)