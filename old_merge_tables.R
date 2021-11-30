#### Mergin tables: metadata_CA_2015 and COVID deaths

library(tidyverse)


## load tables
metadata_2015 <- read.csv("metadata_CA_2015.csv", row.names = 1)
covid_table <- read.csv("deaths.csv")

## merge tables 
master_table <- merge(covid_table, metadata_2015, by.x = "area", by.y = "county_name")


#### covid_case ~ median_income + poverty + density + class{urban; rural} + pct_rural + PM25 + time

fit = lm(formula = cumulative_deaths ~ poverty_all + median_household_income + date, 
          #family = "guassian", 
          data = master_table)
summary(fit)
