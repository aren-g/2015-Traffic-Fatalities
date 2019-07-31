
#load data
accident <- read.csv("accident.csv", header = TRUE)
str(accident)
table(accident$FATALS)
ggplot(accident)
table(accident$DAY_WEEK)
hist(accident$DAY_WEEK)
hist(accident$HOUR)

#number of rows
nrow(accident)

length(accident$PERSONS)
library(dplyr)

#number of accidents with x amount of persons involved 
accident1 <- tbl_df(accident)

accident1 %>% 
  group_by(PERSONS) %>%
  summarize(sum_persons = sum(PERSONS, na.rm = TRUE))

#number of deaths with x amount of persons involved 
accident2 <- tbl_df(accident)

accident2 %>% 
  group_by(FATALS) %>%
  summarize(sum_persons = sum(FATALS, na.rm = TRUE))

plot(accident2$FATALS, sum_persons = sum(FATALS))

#heatmap

unique(accident$HOUR)
unique(accident$DAY_WEEK)

#remove '99' from hour column
accident_hour <- filter(accident, HOUR <24)

heatmap(as.matrix(accident_hour$FATALS),
        scale="column",
        col=heat.colors(256)
        )
# Deaths by state 
accident3 <- tbl_df(accident)
accident3 %>% 
  group_by(STATE) %>%
  summarize(sum_persons = sum(FATALS, na.rm = TRUE))

# Show deaths in states 

accident4 <- group_by(accident_hour, STATE)
sum1<- summarize(accident4, accident5 = sum(FATALS))

sum1$STATE_NAMES <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
                          "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
                          "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
                          "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
                          "UT", "VT", "VA", "WA", "WV", "WI", "WY")

barplot(sum1$accident5, xlab = sum1$STATE_NAMES)


library(ggplot2);
# ggplot(df, aes(as.factor(x), y)) + 
#   geom_point() + 
#   labs(y = "sum1$accident5", x = "sum1$STATE_NAMES")

#  ggplot(df, aes(as.factor(x), y)) +
#  geom_bar(stat = "identity") + 
#  labs(y = "sum1$accident5", x = "sum1$accident5");

install.packages("RColorBrewer")
library("RColorBrewer")
barplot(height = sum1$accident5, names.arg = sum1$STATE_NAMES, col=brewer.pal(n=9, name = "Blues"))

#barplot(desc(height = sum1$accident5), xtabs(~sum1$STATE_NAMES))

# arrange from most to least deaths 

require("ggplot2")
ggplot(sum1, aes(x = reorder(sum1$STATE_NAMES, -sum1$accident5), y = sum1$accident5, )) +
  geom_bar(stat = "identity") +
  xlab("States") +
  ylab("Deaths")


# ggplot(sum1, aes(x = reorder(sum1$STATE_NAMES, -sum1$accident5), y = sum1$accident5, color = abs(rnorm(51)) )) +
#  geom_bar(stat = "identity") +
#  scale_color_gradient(low = 'yellow', high = 'green')

#weather

accident6 <- group_by(accident_hour, WEATHER)
sum2 <- summarize(accident6, accident7 = sum(FATALS))

sum2$weather_pattern <- c('Clear', 
                          'Rain',
                          'Sleet, Hail', 
                          'Snow',
                          'Fog, Smog, Smoke',
                          'Severe Crosswinds', 
                          'Blowing Sand, Soil, Dirt', 
                          'Other', 
                          'Cloudy',
                          'Blowing Snow', 
                          'Freezing Rain or Drizzle', 
                          'Not Reported',
                          'Unknown')

barplot(sum2$accident7, xlab = sum2$weather_pattern)

barplot(height = sum2$accident7, names.arg = sum2$weather_pattern, col=brewer.pal(n=9, name = "Blues"))

ggplot(sum2, aes(x = reorder(sum2$weather_pattern, -sum2$accident7), y = sum2$accident7, )) +
  geom_bar(stat = "identity") +
  xlab("Weather Pattern") +
  ylab("Deaths")


# classification


install.packages("party")
library(party)

set.seed(1234)
accident_hour1 <- accident_hour[, c(1,10,11,15,18,20,51,52)]
accident_hour1 <- na.omit(accident_hour1)

ind<- sample(2, nrow(accident_hour1), replace = T, prob = c(0.7, 0.3))
#1 for training, 2 for testing

train.data <- accident_hour1[ind==1,]
test.data <- accident_hour1[ind==2,]

myf <- factor(accident_hour1$FATALS) ~ accident_hour1$STATE+
  accident_hour1$COUNTY+
  accident_hour1$CITY+
  accident_hour1$DAY_WEEK+
  accident_hour1$NHS+
  accident_hour1$FUNC_SYS+
  accident_hour1$DRUNK_DR

crash_ctree <- ctree(myf, data = train.data)
table(predict(crash_ctree), FATALS)  
  
#another method

library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

s <- sample(2, nrow(accident_hour1), replace = T, prob = c(0.7, 0.3))


train.data1 <- accident_hour1[ind==1,]
test.data1 <- accident_hour1[ind==2,]

dtm <- rpart(accident_hour1$FATALS~
               accident_hour1$STATE+
               accident_hour1$COUNTY+
               accident_hour1$CITY+
               accident_hour1$DAY_WEEK+
               accident_hour1$NHS+
               accident_hour1$FUNC_SYS+
               accident_hour1$DRUNK_DR
             , train.data1, method = "class" )
dtm
rpart.plot(dtm, type=4, extra =101)

p <- predict(dtm, test.data1, type = "class")
table(test.data1[,7],p)

#distract

distract <- read.csv("distract.csv", header = TRUE)

