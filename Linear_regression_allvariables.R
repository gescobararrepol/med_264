###Incidence and Mortality for different demographic variables overtime

##Load libraries
library(tidyverse)
library(ggplot2)

##Load dataset
covid <- read.csv("https://data.chhs.ca.gov/dataset/f333528b-4d38-4814-bebb-12db1f10f535/resource/046cdd2b-31e5-4d34-9ed3-b48cdbc4be7a/download/covid19cases_test.csv")
#covid %>% filter(area != "California")
covid <- covid %>% filter(area != "California")

vartable <- read.csv("Variables.csv")
vartable <- vartable %>% filter(area != "California")

#Load functions
source("time_series_maker.R")

##For incidence
new_time <- make_time()
plot_time(new_time)


## loop per each variable per incidence (cumulative cases)
table_list = list()
for (v in colnames(vartable)[-c(1,2)]) {
  table_list[[v]] = make_time(vdp = v)
}

##
#plot_time(table_list[[1]], adjv = 10) ## unemployment
#plot_time(table_list[[2]], adjv = 1000) ## perc_income
#plot_time(table_list[[4]], adjv = 10) ## RUC_Code
#plot_time(table_list[[5]], adjv = 10) ## poverty_all
#plot_time(table_list[[7]], adjv = 10) ## education

## combine tables
#### add name to each data.frame
for (v in names(sapply(table_list, function(x) colnames(names(x))))){
  table_list[[v]]$var <- names(table_list[v])
}
## combine
ctable <- do.call("rbind", table_list)


### big ggplot
plot_time(ctable, adjv = 10) + facet_grid(. ~ var) + geom_hline(yintercept = -log10(0.05), color = "red", linetype = "dashed")
ggsave(last_plot(), filename = "Incidence_per_variable.png", height = 4, width = 8, dpi = 300)


## For Mortality

## loop per each variable per mortality (cumulative cases)
table_list = list()
for (v in colnames(vartable)[-c(1,2)]) {
  table_list[[v]] = make_time(vdp = v, vind = "cumulative_deaths")
}

## combine tables
#### add name to each data.frame
for (v in names(sapply(table_list, function(x) colnames(names(x))))){
  table_list[[v]]$var <- names(table_list[v])
}
## combine
ctable <- do.call("rbind", table_list)


### big ggplot
plot_time(ctable, adjv = 100) + facet_grid(. ~ var) + geom_hline(yintercept = -log10(0.05), color = "red", linetype = "dashed")
ggsave(last_plot(), filename = "Mortality_per_variable.png", height = 4, width = 8, dpi = 300)

