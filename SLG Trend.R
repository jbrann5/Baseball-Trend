#lahman data 
setwd("~/Desktop/core")
Batting <- read.csv("Batting.csv")
library(plyr)

#Only use data from 2016 and 2017
Bat.17 <- subset(Batting, yearID == 2016 | yearID == 2017)

#Define SLG
Bat.17$X1B <- with(Bat.17, H - X2B - X3B - HR)
Bat.17$SLG <- with(Bat.17,
                   (X1B + 2 * X2B + 3 * X3B + 4 * HR) / AB)

#Merge subsets so that players in across years are on the same line
Bat16 <- subset(Bat.17, yearID==2016)
Bat17 <- subset(Bat.17, yearID==2017)
merged.Bat <- merge(Bat16, Bat17, by="playerID")

#minimum AB of 300
min.bat <- subset(merged.Bat, 
                  AB.x >= 300 & AB.y >=300)

#plot of difference 
with(min.bat, 
     plot(SLG.x, SLG.y,
          xlab="2016 Slugging Percentage", ylab="2017 Slugging Percentage", 
          main="Increase in SLG of MLB Batters With Minimum 300 AB"))
fit <- lm(SLG.y ~ SLG.x, data=min.bat)
abline(fit, lwd=3, col="red")

#Histogram of difference over a year
ggplot(min.bat) + geom_histogram(aes(x= SLG.x))
ggplot(min.bat) + geom_histogram(aes(x= SLG.y))

#summary statistics of 2016
summary(fit)
