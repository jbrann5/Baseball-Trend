strikeouts <- read.csv("strikeouts.csv")

str(strikeouts)
names(strikeouts)
sum(is.na(strikeouts))

trend1 <- glm(K.~ G + IP + ERA + FIP + xFIP + AVG + BB. + Swing. + Contact. + GB. + LD. + FB., strikeouts, family = gaussian("identity"))
summary(trend1)

trend2 <- update(trend1, .~. -G -FIP -AVG -GB. -LD. -FB.)
summary(trend2)

trend3 <- update(trend2, .~.-ERA)
summary(trend3)

trend4 <- update(trend3, .~. -IP)
summary(trend4)

trend5 <- update(trend4, .~. -Swing.)
summary(trend5)

final <- lm(K.~ xFIP + BB. + Contact., strikeouts)
summary(final)

Predict <- predict(trend5, strikeouts, type = "response")

ggplot(strikeouts) + 
  geom_point(aes(x= Predict, y=X2ndHalfK.)) +
  labs(x= "Predicted Strikeout Rate", 
       y= "Actual Strikeout Rate", 
       title = "Correlation beteween Predicted and Actual Strikeout Rate")

accuracy <- lm(X2ndHalfK. ~ Predict, strikeouts)
summary(accuracy)

Residual <- strikeouts$X2ndHalfK. - Predict
ggplot(strikeouts, aes(x= Predict, y=Residual)) + 
  geom_point() +
  labs(x= "Predicted Strikeout Rate", 
       y= "Residual", 
       title = "Residual Plot")