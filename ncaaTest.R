require(dplyr)
entry1 <- read.csv("/Users/asanchez3/Desktop/submit2_names.csv")
names(entry1) <- c("id","pred2","match")
entry2 <- read.csv("/Users/asanchez3/Desktop/submit1_2015.csv")
entries <- merge(entry1,entry2,by="id")
write.csv(entries,"model2015.csv",row.names=FALSE)

remain <- grepl("Kentucky|Maryland|Notre Dame|Kansas|Michigan St.|Oklahoma|Louisville|NC State")

mean(c(regSeason$Wscore,regSeason$Lscore)) 
plot(regSeason$Wscore,regSeason$Lscore)


require(ggplot2)
ggplot(regSeason) +
  geom_bar(regSeason, aes(x=Wscore),fill="red",stat="identity") +
  geom_bar(regSeason, aes(x=Lscore),fill="blue",stat="identity")
