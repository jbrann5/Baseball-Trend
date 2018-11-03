setwd("~/Desktop/core")
Pitching <- read.csv("Pitching.csv")

#Only use data from 2010 and 2017
Pitching.17 <- subset(Pitching, yearID == 2007 | yearID == 2017)

#Define WHIP
Pitching.17$XBH <- with(Pitching.17, H + BB)
Pitching.17$IP <- with(Pitching.17,IPouts *3)
Pitching.17$WHIP <- with(Pitching.17, XBH/IP)
              

#Merge subsets so that players in across years are on the same line
Pitching07 <- subset(Pitching.17, yearID==2007)
Pitching17 <- subset(Pitching.17, yearID==2017)
merged.Pitching <- merge(Pitching07, Pitching17, by="playerID")

#minimum of 30 Innings Pitched
min.pitching <- subset(merged.Pitching, 
                       IP.x >= 30 & IP.y >=30)

#Differnce
min.pitching$Difference <- with(min.pitching, WHIP.y-WHIP.x)
ggplot(min.pitching) + geom_histogram(aes(x=Difference))
mean(min.pitching$Difference)
summary(min.pitching$Difference)

#Plot
with(min.pitching, 
     plot(WHIP.x, WHIP.y,
          xlab="2007 WHIP", ylab="2017 WHIP", 
          main="WHIP of MLB Pitchers With Minimum 30 Innings"))
fit1 <- lm(WHIP.y ~ WHIP.x, data=min.pitching)
abline(fit1, lwd=3, col="red")

#Histogram of both 2016 and 2017's WHIP
gplot(min.pitching) + geom_histogram(aes(x= WHIP.x), bins = 25)
ggplot(min.pitching) + geom_histogram(aes(x= WHIP.y), bins=25)



