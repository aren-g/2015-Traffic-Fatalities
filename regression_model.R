
accident <- read.csv("accident.csv", header = TRUE)


accident_hour <- filter(accident, HOUR <24)
accident_hour1 <- accident_hour[, c(1,10,11,15,18,20,51,52)]


#fit multiple regression model
model1 <- lm(accident_hour1$FATALS ~ accident_hour1$STATE+accident_hour1$COUNTY+accident_hour1$CITY+
               accident_hour1$DAY_WEEK+accident_hour1$NHS+accident_hour1$FUNC_SYS+accident_hour1$DRUNK_DR)


summary(model1)
#Multiple R-Squared of only 0.004924, suggesting that these variables only account for 0.4% of 
# the variability of fatalities

plot(model1)
pairs(accident_hour1[1:7])