# Some helper functions
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

# --------------------------
# clusters
s1TeamStats <- filter(rsTeamStats, Season == 2015)
s1TeamStats <- select(s1TeamStats, - Season)
#head(arrange(s1TeamStats, desc(avgScore)))

s1Clusters <- hclust(dist(s1TeamStats))
plot(s1Clusters)  

# --------------------------
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
#compareHA <- merge(home[,c("Season","team",compare_var)],away[,c("Season","team",compare_var)], by=c("Season","team"))
compareHA <- merge(home,away, by=c("Season","team"))
plot(compareHA[,3],compareHA[,4])

compareHAmeans <- compareHA %>%
  group_by(Season) %>%
  #mutate(meanA = mean(avgScore.x), meanH = mean(avgScore.y), meanT = (meanA + meanH)/2) %>%
  mutate_each(funs(mean(.))) %>%
  distinct(Season)
compareHAmeans[,c("avgFga.x","avgFga.y")]
write.csv(compareHAmeans, "compareHome_Away_means.csv",row.names=FALSE)
##############
######################################

