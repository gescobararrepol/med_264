###Master Table for models

## SOURCE linear_regression.R
source("Linear_regression.R")
vartable <- read.csv("Variables.csv")

### Generate COVID tables for 2021-08-01 and last day available

## filter dates
last_day = "2021-08-01"
covid %>% filter(date == last_day) -> aug_covid
last_day = max(dates)
covid %>% filter(date == last_day) -> last_covid

### substract covid cum cases
last_covid$cumulative_cases - aug_covid$cumulative_cases

# Make Dataframe
frame_table <- data.frame(
  area = last_covid$area,
  population = last_covid$population,
  diff_cases = last_covid$cumulative_cases - aug_covid$cumulative_cases,
  diff_incidence = (last_covid$cumulative_cases - aug_covid$cumulative_cases)/last_covid$population,
  diff_deaths = last_covid$cumulative_deaths - aug_covid$cumulative_deaths,
  diff_mortality = (last_covid$cumulative_deaths - aug_covid$cumulative_deaths)/last_covid$population,
  diff_test = last_covid$cumulative_total_tests - aug_covid$cumulative_total_tests,
  diff_ntest = (last_covid$cumulative_total_tests - aug_covid$cumulative_total_tests)/last_covid$population
)

## frame_table to make linear models
master_table <- merge(frame_table, vartable, by = "area")

source("Vaccine_table.R")

master_table <- merge(frame_vacc, master_table, by = "area")

write.csv(master_table, "Master_Table.csv")





