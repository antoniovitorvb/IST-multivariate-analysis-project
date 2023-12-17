library(readr)
library(dplyr)
library(tidyr)
library(stringr)

fitdata <- read_csv("fitness_class_2212.csv")

# DATA EXAMINATION
str(fitdata)
summary(fitdata)

sapply(fitdata, function(x) sum(is.na(x)))

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

unique(fitdata$time)
fitdata$time <- ifelse(fitdata$time == "AM", 0, 1) %>%
     factor()

table(fitdata$category)

fitdata <- fitdata %>%
     mutate(category = na_if(category, "-")) #%>% # Replace "-" with NA
     # drop_na(category) # Remove the rows in fitdata$category that is NA
fitdata$category <- factor(fitdata$category)

fitdata$attended <- factor(fitdata$attended)

sapply(fitdata, function(x) sum(is.na(x)))

get_na <- function(df) {
     na_rows <- lapply(df, function(x) which(is.na(x))) %>% # get indices of NA values in each column
          unlist() %>%
          unique() # get the unique indices of all columns
     return(df[na_rows, , drop = FALSE])
}

fit_na <- get_na(fitdata)

# PRELIMINARY ANALYSIS

