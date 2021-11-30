### combine covid_table with SES

# load libraries
library(tidyverse)
library(ggplot2)
library(zoo) 

# load data
covid <- read.csv("deaths.csv")
ses   <- read.csv("county household income.csv", header = TRUE, sep = ";") 

 
## new columns 
ses$hhicm <- ses$Textbox20 %>% gsub(pattern = "\\$", replacement = "")
ses$county <- ses$name %>% gsub(pattern = " County, CA", replacement = "") %>% 
  gsub(pattern = " County/city, CA", replacement = "") 

## merge data
covid <- merge(covid, ses[,c("county", "hhicm")], by.x = "area", by.y = "county")

## plot stuff 

## pre-process
covid$hhicm <- gsub(pattern = ",", replacement = "", covid$hhicm)
covid$hhicm <- as.numeric(covid$hhicm)

## deaths per day 
covid %>% filter(area != "California" & area != "Out of state" & area != "Unknown") %>% 
  na.omit() %>% 
  ggplot() +
  aes(x = as.Date(date), 
      y = reorder(area, hhicm),
      fill = cut(deaths/(population/100000), 
                 c(-Inf, 0,
                   0.5,
                   1,
                   1.5,
                   2.0,
                   2.5,
                   Inf), 
                 include.lowest = TRUE) 
  ) +
  geom_tile() + 
  scale_fill_brewer(palette = "YlOrRd") +
  theme_classic() + labs( fill = "deaths per 10^5")

## cases per day 
covid %>% filter(area != "California" & area != "Out of state" & area != "Unknown") %>% 
  na.omit() %>% 
  ggplot() +
  aes(x = as.Date(date), 
      y = reorder(area, hhicm),
      fill = cut(cases/(population/100000), 
                 c(-Inf, 0,
                   10,
                   20,
                   30,
                   40,
                   50,
                   Inf), 
                 include.lowest = TRUE) 
  ) +
  geom_tile() + 
  scale_fill_brewer(palette = "YlOrRd") +
  theme_classic() + labs( fill = "cases per 10^5")

### per month 
### plot modified table 
covid %>% filter(area != "California" & area != "Out of state" & area != "Unknown") %>% 
  na.omit() %>% 
  ggplot() +
  aes(x = as.yearmon(as.Date(date)), 
      y = reorder(area, hhicm),
      fill = cut(deaths/(population/100000), 
                 c(-Inf, 0,
                   0.5,
                   1,
                   1.5,
                   2.0,
                   2.5,
                   Inf), 
                 include.lowest = TRUE) 
  ) +
  geom_tile(colour="white",size=0.5) + 
  scale_fill_brewer(palette = "YlOrRd") +
  theme_classic() + labs( fill = "deaths per 10^5")

### table hhicm
ses[,c("county", "hhicm")]



