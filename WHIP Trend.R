setwd("~/Desktop/core")
Pitching <- read.csv("Pitching.csv")

#Only use data from the last 2 years
Pitching.17 <- subset(Pitching, yearID == 2016 | yearID == 2017)

#Define WHIP
Pitching.17$XBH <- with(Pitching.17, H + BB)
Pitching.17$IP <- with(Pitching.17,IPouts *3)
Pitching.17$WHIP <- with(Pitching.17, XBH/IP)
              

#Merge subsets so that players in across years are on the same line
Pitching16 <- subset(Pitching.17, yearID==2016)
Pitching17 <- subset(Pitching.17, yearID==2017)
merged.Pitching <- merge(Pitching16, Pitching17, by="playerID")

#minimum of 40 Innings Pitched
min.pitching <- subset(merged.Pitching, 
                       IP.x >= 40 & IP.y >=40)

#Plot
with(min.pitching, 
     plot(WHIP.x, WHIP.y,
          xlab="2016 WHIP", ylab="2017 WHIP", 
          main="Increase of WHIP of MLB Pitchers With Minimum 40 Innings"))
fit1 <- lm(WHIP.y ~ WHIP.x, data=min.pitching)
abline(fit1, lwd=3, col="red")

#Histogram of both 2016 and 2017's WHIP
ggplot(min.pitching) + geom_histogram(aes(x= WHIP.x), bins =25)
ggplot(min.pitching) + geom_histogram(aes(x= WHIP.y), bins=25)

#summary statistics
summary(fit1)
