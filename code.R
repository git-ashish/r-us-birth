library(tidyverse)
library(lubridate)
library(readr)

#import datasets
US_births_1994_2003 <- read_csv("data/US_births_1994-2003_CDC_NCHS - US_births_1994-2003_CDC_NCHS.csv.csv")
US_births_2000_2014 <- read_csv("data/US_births_2000-2014_SSA - US_births_2000-2014_SSA.csv.csv")

# Join both datasets to create single table
us_births <- rbind(US_births_1994_2003, US_births_2000_2014)

# 1. Lets see yearly trends
summary_by_year <- us_births %>% group_by(year) %>% summarise(mean = mean(births), total = sum(births), sd = sd(births) )
ggplot(data = summary_by_year, aes(x = year)) +
geom_smooth(mapping = aes(y = total))


# create a date column
us_births <- us_births %>% mutate( date = ymd(paste(year, month, date_of_month, sep = '-')) )

# 2. See monthly trends
us_births %>%
ggplot(aes(x = ymd(date), y = births)) +
geom_smooth(aes(color = month(ymd(date), label = T, abbr = F)), se = FALSE) +
labs(
color = 'Month'
)

#3. See Weekday trends
us_births %>%
ggplot(aes(x = ymd(date), y = births)) +
geom_smooth(aes(color = wday(ymd(date), label = T, abbr = F)), se = FALSE) +
labs(
color = 'Weekday'
)

#4. See all months, week wise
