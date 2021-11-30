###Table for vaccines

# load libraries
library(tidyverse)
library(ggplot2)

#Load tables for covid outcome per county in CA
vaccine <- read.csv("https://data.chhs.ca.gov/dataset/e283ee5a-cf18-4f20-a92c-ee94a2866ccd/resource/130d7ba2-b6eb-438d-a412-741bde207e1c/download/covid19vaccinesbycounty.csv")

## Only max date
dates <- vaccine$administered_date %>% table %>% names() ## all dates

last_day = max(dates)
vaccine %>% filter(administered_date == last_day) -> last_vaccine

# Make Dataframe for vaccine
frame_vacc <- data.frame(
  area = last_vaccine$county,
  fullyvacc = last_vaccine$cumulative_fully_vaccinated
)
