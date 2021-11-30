###Linear model

# load libraries
library(tidyverse)
library(ggplot2)

#Load tables for covid outcome per county in CA
covid <- read.csv("https://data.chhs.ca.gov/dataset/f333528b-4d38-4814-bebb-12db1f10f535/resource/046cdd2b-31e5-4d34-9ed3-b48cdbc4be7a/download/covid19cases_test.csv")

#Load table for household income (as of 26NOV21)
hhold <- read.csv("UnemploymentReport_ge.csv", 
                  sep = ";", 
                  skip = 2, #remove first 2 rows
                  header = TRUE, check.names = FALSE)

## pre-processing
hhold <- hhold[1:59,] #filter empty rows
(hhold$Name %>% # are they the same county names?
    gsub(pattern = " County, CA", replacement = "") %>% 
    gsub(pattern = " County/city, CA", replacement = "")
  ) %in% (covid$area %>% table %>% names())

## make new name
hhold$area <- hhold$Name %>% 
  gsub(pattern = " County, CA", replacement = "") %>% 
  gsub(pattern = " County/city, CA", replacement = "")

## get covid accumulated data
covid %>% filter(date == max(covid$date)) %>% glimpse()
covid_last <- covid %>% filter(date == max(covid$date))

#Create Master Table
master_table1 <- merge(covid_last, hhold, by.x = "area", by.y = "area")
gsub("%", "", master_table1$`% of State Median HH Income`) %>% as.numeric() -> master_table1$`% of State Median HH Income`
gsub("\\$", "", master_table1$`Median Household Income (2019)`) %>% gsub("\\,", "", .) %>% as.numeric() -> master_table1$`Median Household Income (2019)`

#create cumulative deaths per Populations
master_table1$cumulative_deaths/master_table1$population

master_table1$deaths_N <- master_table1$cumulative_deaths/master_table1$population * 100

#linear model mortality per %HH
fit1 <- lm(master_table1$deaths_N ~ master_table1$`% of State Median HH Income`)
summary(fit1)

#Create cumulative incidence per populations
master_table1$inc <- master_table1$cumulative_cases/master_table1$population * 100

#linear model incidence per %HH
fit2 <- lm(master_table1$inc ~ master_table1$`% of State Median HH Income`)
summary(fit2)

coef(fit2)[2] ## beta
summary(fit2)$coefficients[,4][2] ### pval

plot(master_table1$`% of State Median HH Income`, master_table1$inc)
abline(fit2)

plot(master_table1$`% of State Median HH Income`, master_table1$deaths_N)
abline(fit1)

#### time thing cum deaths

dates <- covid$date %>% table %>% names() ## all dates

### FOR LOOP
pvals = list()
betas = list()
for (t in dates) {
  #print(t)
  ## get covid accumulated data
  #covid %>% filter(date == max(covid$date)) %>% glimpse()
  covid_last <- covid %>% filter(date == t)
  #Create Master Table
  master_table <- merge(covid_last, hhold, by.x = "area", by.y = "area")
  gsub("%", "", master_table$`% of State Median HH Income`) %>% as.numeric() -> master_table$`% of State Median HH Income`
  gsub("\\$", "", master_table$`Median Household Income (2019)`) %>% gsub("\\,", "", .) %>% as.numeric() -> master_table$`Median Household Income (2019)`
  ## model 
  #create cumulative deaths per Populations
  #master_table1$cumulative_deaths/master_table1$population
  master_table$deaths_N <- master_table$cumulative_deaths/master_table$population * 100
  fit <- lm(master_table$deaths_N ~ master_table$`% of State Median HH Income`)
  summary(fit)
  ## get values
  betas[t] <- coef(fit)[2] ## beta
  pvals[t] <- summary(fit)$coefficients[,4][2] ### pval
  
}

### make dataframe
timeseries <- data.frame(
  time = names(betas),
  betas = unname(sapply(betas, function(x) unlist(x))),
  pvals = unname(sapply(pvals, function(x) unlist(x)))
)

timeseries <- timeseries[-1,]
timeseries

#'2019-12-12' < as.Date('2020-01-01') ## filter by date
# timeseries[timeseries$time > as.Date('2021-08-01'),]
timeseries %>% 
#  filter(time > as.Date('2021-08-01')) %>% 
  ggplot() + 
  aes(x = as.Date(time)) + 
  geom_point(aes(y = -log10(pvals))) + 
  geom_point(aes(y = betas*10000), col="blue") +
  scale_y_continuous(name = "-log10(p-val)", 
                     sec.axis = sec_axis(~.*1, name = "Betas")) +
  theme_classic() + theme(axis.text.x = element_text(size = 5, angle = 90))



###LOOP for incidence

### FOR LOOP
pvals = list()
betas = list()
for (t in dates) {
  print(t)
  ## get covid accumulated data
  #covid %>% filter(date == max(covid$date)) %>% glimpse()
  covid_last <- covid %>% filter(date == t)
  #Create Master Table
  master_table <- merge(covid_last, hhold, by.x = "area", by.y = "area")
  gsub("%", "", master_table$`% of State Median HH Income`) %>% as.numeric() -> master_table$`% of State Median HH Income`
  gsub("\\$", "", master_table$`Median Household Income (2019)`) %>% gsub("\\,", "", .) %>% as.numeric() -> master_table$`Median Household Income (2019)`
  ## model 
  #create cumulative deaths per Populations
  #master_table1$cumulative_deaths/master_table1$population
  master_table$deaths_N <- master_table$cumulative_cases/master_table$population * 100
  fit <- lm(master_table$deaths_N ~ master_table$`% of State Median HH Income`)
  summary(fit)
  ## get values
  betas[t] <- coef(fit)[2] ## beta
  pvals[t] <- summary(fit)$coefficients[,4][2] ### pval
  
}
### make dataframe
timeseries <- data.frame(
  time = names(betas),
  betas = unname(sapply(betas, function(x) unlist(x))),
  pvals = unname(sapply(pvals, function(x) unlist(x)))
)

timeseries <- timeseries[-1,]
timeseries

#'2019-12-12' < as.Date('2020-01-01') ## filter by date
# timeseries[timeseries$time > as.Date('2021-08-01'),]
timeseries %>% 
 #filter(time > as.Date('2021-08-01')) %>% 
  ggplot() + 
  
  aes(x = as.Date(time)) + 
  geom_point(aes(y = -log10(pvals))) + 
  geom_point(aes(y = betas*100), col="blue") +
  scale_y_continuous(name = "Significance(-log10(p-val))", 
                     sec.axis = sec_axis(~.*1, name = "Betas")) +
  theme_classic() + theme(axis.text.x = element_text(size = 5, angle = 90))


#linear model 
fit3 <- lm(master_table1$inc ~ master_table1$'2020')
summary(fit3)

plot(master_table1$'2020', master_table1$inc)
abline(fit3)

fit4 <- lm(master_table1$inc ~ master_table1$'2020' + master_table1$`Median Household Income (2019)`)
summary(fit4)




###LOOP for incidence

### FOR LOOP
pvals = list()
betas = list()
for (t in dates) {
  print(t)
  ## get covid accumulated data
  #covid %>% filter(date == max(covid$date)) %>% glimpse()
  covid_last <- covid %>% filter(date == t)
  #Create Master Table
  master_table <- merge(covid_last, hhold, by.x = "area", by.y = "area")
  gsub("%", "", master_table$'2020') %>% as.numeric() -> master_table$`% of State Median HH Income`
  gsub("\\$", "", master_table$'2020') %>% gsub("\\,", "", .) %>% as.numeric() -> master_table$'2020'
  #create cumulative deaths per Populations
  #master_table1$cumulative_deaths/master_table1$population
  master_table$deaths_N <- master_table$cumulative_cases/master_table$population * 100
  fit <- lm(master_table$deaths_N ~ master_table$'2020')
  summary(fit)
  ## get values
  betas[t] <- coef(fit)[2] ## beta
  pvals[t] <- summary(fit)$coefficients[,4][2] ### pval
  
}
### make dataframe
timeseries <- data.frame(
  time = names(betas),
  betas = unname(sapply(betas, function(x) unlist(x))),
  pvals = unname(sapply(pvals, function(x) unlist(x)))
)

timeseries <- timeseries[-1,]
timeseries

#'2019-12-12' < as.Date('2020-01-01') ## filter by date
# timeseries[timeseries$time > as.Date('2021-08-01'),]
timeseries %>% 
  #filter(time > as.Date('2021-08-01')) %>% 
  ggplot() + 
  
  aes(x = as.Date(time)) + 
  geom_point(aes(y = -log10(pvals))) + 
  geom_point(aes(y = betas*10), col="blue") +
  scale_y_continuous(name = "Significance(-log10(p-val))", 
                     sec.axis = sec_axis(~.*1, name = "Betas")) +
  theme_classic() + theme(axis.text.x = element_text(size = 5, angle = 90))

