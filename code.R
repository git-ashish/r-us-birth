library(tidyverse)
library(lubridate)
library(readr)

#import datasets
US_births_1994_2003 <- read_csv("data/US_births_1994-2003_CDC_NCHS - US_births_1994-2003_CDC_NCHS.csv.csv")
US_births_2000_2014 <- read_csv("data/US_births_2000-2014_SSA - US_births_2000-2014_SSA.csv.csv")

# Join both datasets to create single table
# - Selecting data range 2000 - 2003 only from a single source
us_births <- rbind(US_births_1994_2003, US_births_2000_2014 %>% filter(year > 2003))

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
color = 'Month',
x = 'Year',
y = 'Births / Year'
)

# 2. ii) Change colors
us_births %>%
  ggplot(aes(x = ymd(date), y = births)) +
  geom_smooth(aes(color = month(ymd(date), label = T, abbr = F)), se = FALSE) +
  labs(
    color = 'Month',
    x = 'Year',
    y = 'Births / Year'
  ) + 
  scale_color_manual(values = c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000')) + 
  theme_minimal()

#3. See Weekday trends
us_births %>%
  ggplot(aes(x = ymd(date), y = births)) +
  geom_smooth(aes(color = wday(ymd(date), label = T, abbr = F)), se = FALSE) +
  labs(
    color = 'Month',
    x = 'Year',
    y = 'Births / Year'
  ) + 
  theme_minimal()

#4. Seeing trends with Y axis starting at 0

#4. i) Months
us_births %>%
  ggplot(aes(x = ymd(date), y = births)) +
  geom_smooth(aes(color = month(ymd(date), label = T, abbr = F)), se = FALSE) +
  labs(
    color = 'Month',
    x = 'Year',
    y = 'Births / Year'
  ) +
  scale_y_continuous(limits = c(0, 14000)) + 
  theme_minimal()

#4. ii) Yearly
ggplot(data = summary_by_year, aes(x = year)) +
  geom_smooth(mapping = aes(y = total)) +
  scale_y_continuous(limits = c(0, 4400000))