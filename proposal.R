### Idea

# load data
deaths <- read.csv("deaths.csv")
# load libraries
library(tidyverse)
library(ggplot2)
library(zoo)   ### as.yearmon()
#deaths %>% select(date,area, deaths) -> deaths2

deaths$area %>% table %>% dim

deaths$deaths %>% hist(breaks = 100)

deaths %>% filter(area == "San Diego") %>% select(deaths)%>% as.numeric() #%>% hist(breaks = 100)

deaths[deaths$area == "San Diego",]$deaths %>% hist()

### grafico de barra
deaths %>% filter(area == "San Diego") %>% 
  ggplot() +
    aes(x = as.Date(date), y = deaths) + geom_col() + theme_classic()

## grafico de linea
deaths %>% filter(area == "San Diego") %>% 
  ggplot() +
  aes(x = as.Date(date), y = deaths, group = 1) + geom_line() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + theme_classic()

## grafico de linea para todos los counties
deaths %>% filter(area != "California") %>% 
  ggplot() +
  aes(x = as.Date(date), y = deaths, group = area) + geom_line(aes(color = area)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + theme_classic()

## Tiles per day Cases/Population log2 
deaths %>% filter(area != "California" & area != "Out of state" & area != "Unknown") %>% 
  ggplot() +
  aes(x = as.Date(date), y = area, fill = log2((cases+1)/population) ) + geom_tile() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  scale_fill_distiller(palette = "Spectral") +
  theme_classic()

## Tiles per day Deaths/Population log2 
deaths %>% filter(area != "California" & area != "Out of state" & area != "Unknown") %>% 
  ggplot() +
  aes(x = as.Date(date), y = area, fill = deaths/(population/10000)) + geom_tile() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  scale_fill_distiller(palette = "Spectral") +
  theme_classic()

## Bin months 
deaths %>% filter(area != "California" & area != "Out of state" & area != "Unknown") %>% 
  ggplot() +
  aes(x = as.yearmon(as.Date(date)), 
      y = area, 
      #fill = deaths) +
      fill = deaths/(population/100000)) +
  geom_tile() + 
  #scale_x_yearmon() +
  #scale_x_continuous(breaks=as.numeric(as.Date(deaths$date)), labels=format(as.Date(deaths$date),"%m")) +
  #scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  scale_fill_distiller(palette = "Spectral") +
  theme_classic()

### bin colors 
## Bin months with NAs 
deaths %>% filter(area != "California" & area != "Out of state" & area != "Unknown") %>% 
  na.omit() %>% 
  ggplot() +
  aes(x = as.yearmon(as.Date(date)), 
      y = area, 
      #fill = deaths) +
      fill = cut(deaths/(population/100000), c(0,0.5,1,1.5,2,2.5,Inf)) 
      ) +
  geom_tile() + 
  #scale_x_yearmon() +
  #scale_x_continuous(breaks=as.numeric(as.Date(deaths$date)), labels=format(as.Date(deaths$date),"%m")) +
  #scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  scale_fill_viridis_d() +
  theme_classic()


### plot modified table 
deaths %>% filter(area != "California" & area != "Out of state" & area != "Unknown") %>% 
  na.omit() %>% 
  ggplot() +
  aes(x = as.yearmon(as.Date(date)), 
      y = area,
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

## per day 
deaths %>% filter(area != "California" & area != "Out of state" & area != "Unknown") %>% 
  na.omit() %>% 
  ggplot() +
  aes(x = as.Date(date), 
      y = area,
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


###### make functions 

## plot per day 
perday_plot <- function(df){
  df %>% filter(area != "California" & area != "Out of state" & area != "Unknown") %>% 
    na.omit() %>% 
    ggplot() +
    aes(x = as.Date(date), 
        y = area,
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
    theme_classic() + labs( fill = paste("deaths per 10^5")) -> p
  p
}
## plotpermonth
permon_plot <- function(df){
  df %>% filter(area != "California" & area != "Out of state" & area != "Unknown") %>% 
    na.omit() %>% 
    ggplot() +
    aes(x = as.yearmon(as.Date(date)), 
        y = area,
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
    theme_classic() + labs( fill = "deaths per 10^5") -> p
  p
}
## 

perday_plot(deaths) ### need to relativize Z

permon_plot(deaths)


#### cases
## per day 
deaths %>% filter(area != "California" & area != "Out of state" & area != "Unknown") %>% 
  na.omit() %>% 
  ggplot() +
  aes(x = as.Date(date), 
      y = area,
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





