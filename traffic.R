
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

accident2 <- tbl_df(accident)

#number of deaths with x amount of persons involved 

accident2 %>% 
  group_by(FATALS) %>%
  summarize(sum_persons = sum(FATALS, na.rm = TRUE))

