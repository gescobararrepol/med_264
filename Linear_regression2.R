### LINEAR REGRESSION 2

## SOURCE linear_regression.R
source("Linear_regression.R")

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
  diff_cases = last_covid$cumulative_cases - aug_covid$cumulative_cases,
  diff_incidence = (last_covid$cumulative_cases - aug_covid$cumulative_cases)/last_covid$population
)

## frame_table to make linear models
master_table <- merge(frame_table, hhold, by = "area")
gsub("%", "", master_table$`% of State Median HH Income`) %>% as.numeric() -> master_table$`% of State Median HH Income`
gsub("\\$", "", master_table$`Median Household Income (2019)`) %>% gsub("\\,", "", .) %>% as.numeric() -> master_table$`Median Household Income (2019)`
## model 

tfit1 <- lm(master_table$diff_incidence ~ master_table$`Median Household Income (2019)`)
summary(tfit1)

plot(master_table$`Median Household Income (2019)`,master_table$diff_incidence)
abline(tfit1)

tfit2 <- lm(master_table$diff_incidence ~ master_table$'2020')
summary(tfit2)

plot(master_table$`Median Household Income (2019)`,master_table$diff_incidence)
abline(tfit2)



