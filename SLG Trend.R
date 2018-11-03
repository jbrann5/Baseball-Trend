#lahman data 
setwd("~/Desktop/core")
Batting <- read.csv("Batting.csv")
library(plyr)

#Only use data from 2007 and 2017
Bat.17 <- subset(Batting, yearID == 2007 | yearID == 2017)

#Define SLG
Bat.17$X1B <- with(Bat.17, H - X2B - X3B - HR)
Bat.17$SLG <- with(Bat.17,
                   (X1B + 2 * X2B + 3 * X3B + 4 * HR) / AB)

#Merge subsets so that players in across years are on the same line
Bat07 <- subset(Bat.17, yearID==2007)
Bat17 <- subset(Bat.17, yearID==2017)
merged.Bat <- merge(Bat07, Bat17, by="playerID")

#minimum AB of 300
min.bat <- subset(merged.Bat, 
                  AB.x >= 300 & AB.y >=300)

#Differnce
min.bat$Difference <- with(min.bat, SLG.y - SLG.x)
ggplot(min.bat) + geom_histogram(aes(x=Difference), bins = 25)
mean(min.bat$Difference)
summary(min.bat$Difference)

#PLot
with(min.bat, 
     plot(SLG.x, SLG.y,
          xlab="2007 Slugging Percentage", ylab="2017 Slugging Percentage", 
          main="SLG of MLB Batters With Minimum 300 AB"))
fit <- lm(SLG.y ~ SLG.x, data=min.bat)
abline(fit, lwd=3, col="red")


#Histogram of SLG of 2007 and 2017
ggplot(min.bat) + geom_histogram(aes(x= SLG.x), bins = 25)
ggplot(min.bat) + geom_histogram(aes(x= SLG.y), bins = 25)

