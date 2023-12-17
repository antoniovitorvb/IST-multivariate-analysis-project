library(readr)
library(dplyr)
library(stringr)

fitdata <- read_csv("fitness_class_2212.csv")

# DATA EXAMINATION
str(fitdata)
summary(fitdata)

# TIDYING THE DATA

fitdata$days_before <- fitdata$days_before %>%
     strsplit(split = " ") %>% # split some strings that had more than the number of days
     sapply(function(x) x[1]) %>% # gets the 1st element of the split strings
     as.integer() # converts to integer

unique(fitdata$day_of_week)

fitdata$day_of_week <- fitdata$day_of_week %>%
     tolower() %>% # converts to lower case
     substr(1, 3) %>% # gets only the 3 first elements of each string
     factor() # Converts to factor to be used as categorical variable

cc
