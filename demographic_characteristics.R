###demographic characteristics

# load libraries
library(tidyverse)
library(ggplot2)

#Load master table
mtable <- read.csv("Master_Table.csv")

#normalize fully vacc by population
mtable$nfullyvacc <- mtable$fullyvacc/mtable$population

#multiply incidence and mortality by 100,000
mtable$diff_incidence <- mtable$diff_incidence*100000
mtable$diff_mortality <- mtable$diff_mortality*100000

#make binary variable for rural codes
mtable$Rural <-  ifelse(mtable$RUC_Code==1, 2,ifelse(mtable$RUC_Code==2, 2, 
                                                     ifelse(mtable$RUC_Code==3, 1,
                                                            ifelse(mtable$RUC_Code==4, 1,
                                                                   ifelse(mtable$RUC_Code==5, 1,
                                                                          ifelse(mtable$RUC_Code==6, 1,
                                                                                 ifelse(mtable$RUC_Code==7, 1,0)))))))
#sum(mtable$Rural==2)
#sum(mtable$Rural==1)
#sum(mtable$Rural==0)

#remove Imperial outlier
#mtable %>% filter(area != "Imperial") -> mtable

#Rural as categorical variable
mtable$Rural <- as.factor(mtable$Rural)

#demographics
tab1 <- data_frame(
cases = summary(mtable$diff_cases)[c(3,5)],
deaths = summary(mtable$diff_deaths)[c(3,5)],
Tests = summary(mtable$diff_test)[c(3,5)],
Vaccinated = summary(mtable$fullyvacc)[c(3,5)],
Poverty_all = summary(mtable$poverty_all)[c(3,5)],
Poverty_17 = summary(mtable$poverty_17)[c(3,5)],
Househol_income = summary(mtable$hhold_income)[c(3,5)],
High_education = summary(mtable$hi_edu)[c(3,5)])

median(mtable$diff_cases)
IQR(mtable$diff_cases)
summary(mtable$diff_cases)[c(3,5)]

write.csv(tab1, "demographic_characteristics.csv")

summary(mtable$unemployment_2020)[c(3,5)]





