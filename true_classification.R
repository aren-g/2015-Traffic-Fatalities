accident <- read.csv("accident.csv", header = TRUE)
library(dplyr)
accident_hour <- filter(accident, HOUR <24)
accident_hour1 <- accident_hour[, c(1,10,11,15,18,20,51,52)]
accident_hour1 <- na.omit(accident_hour1)
attach(accident_hour1)

ind<- sample(2, nrow(accident_hour1), replace = T, prob = c(0.7, 0.3))
#1 for training, 2 for testing

train.data <- accident_hour1[ind==1,]
test.data <- accident_hour1[ind==2,]

myf <- factor(FATALS) ~ STATE+
  COUNTY+
  CITY+
  DAY_WEEK+
  NHS+
  FUNC_SYS+
  DRUNK_DR

crash_ctree <- ctree(myf, data = train.data)
table(predict(crash_ctree), train.data$FATALS)  

plot(crash_ctree)

#counties not good because it's not an integer value, weather is categorical too 

testpred <- predict(crash_ctree, newdata = test.data)
table(testpred, test.data$FATALS)
