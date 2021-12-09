### How is the pandemic changing overtime at the level county in CA?

# load data
deaths <- read.csv("https://data.chhs.ca.gov/dataset/f333528b-4d38-4814-bebb-12db1f10f535/resource/046cdd2b-31e5-4d34-9ed3-b48cdbc4be7a/download/covid19cases_test.csv")
# load libraries
library(tidyverse)
library(ggplot2)
library(zoo)   ### as.yearmon()
#deaths %>% select(date,area, deaths) -> deaths2

deaths$area %>% table %>% dim

deaths$deaths %>% hist(breaks = 100)

deaths %>% filter(area == "San Diego") %>% select(deaths)%>% as.numeric() #%>% hist(breaks = 100)

deaths[deaths$area == "San Diego",]$deaths %>% hist()

## grafico de linea para todos los counties
deaths %>% filter(area != "California") %>% 
  ggplot() +
  aes(x = as.Date(date), y = deaths, group = area) + geom_line(aes(color = area)) + 
  scale_x_date(date_breaks = "2 month", date_labels = "%b") + 
  xlab("Date") +
  ylab("Deaths by COVID-19") +
  theme(legend.position = "none",
        axis.text.x = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        panel.background = element_rect(fill = "white",
                                        colour = "black"))
          

deaths$date <- as.Date(deaths$date)

deaths %>% filter(date >= as.Date("2021-08-01")) -> deaths2

## grafico de linea para todos los counties for new wave
deaths2 %>% filter(area != "California") %>% 
  ggplot() +
  aes(x = as.Date(date), y = deaths, group = area) + geom_line(aes(color = area)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  xlab("Date") +
  ylab("Deaths by COVID-19") +
  theme(
        axis.text.x = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        panel.background = element_rect(fill = "white",
                                        colour = "black"))  

