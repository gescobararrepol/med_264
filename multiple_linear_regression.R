###Multiple Linear Regression 

# load libraries
library(tidyverse)
library(ggplot2)

#Load master table
mtable <- read.csv("Master_Table.csv")

#normalize fully vacc by population
mtable$nfullyvacc <- mtable$fullyvacc/mtable$population

#fit multiple linear regression incidence
fit1 <- lm(mtable$diff_incidence ~  mtable$nfullyvacc + mtable$hi_edu + mtable$poverty_17 
          + mtable$poverty_all + mtable$RUC_Code + mtable$hhold_income + mtable$perc_income 
          + mtable$unemployment_2020 +mtable$diff_ntest)
summary(fit1)

#fit multiple linear regression mortality
fit2 <- lm(mtable$diff_mortality ~  mtable$nfullyvacc + mtable$hi_edu + mtable$poverty_17 
           + mtable$poverty_all + mtable$RUC_Code + mtable$hhold_income + mtable$perc_income 
           + mtable$unemployment_2020 +mtable$diff_ntest)
summary(fit2)

#hh income is a confounder
fit2 <- lm(mtable$nfullyvacc ~ mtable$hhold_income)
summary(fit2)
plot(mtable$hhold_income , mtable$nfullyvacc)
abline(fit2)

#Rurality as categorical
#fit multiple linear regression incidence
fit1 <- lm(mtable$diff_incidence ~  mtable$nfullyvacc + mtable$hi_edu + mtable$poverty_17 
           + mtable$poverty_all + as.factor(mtable$RUC_Code) + mtable$hhold_income + mtable$perc_income 
           + mtable$unemployment_2020 +mtable$diff_ntest)
summary(fit1)

#fit multiple linear regression mortality
fit2 <- lm(mtable$diff_mortality ~  mtable$nfullyvacc + mtable$hi_edu + mtable$poverty_17 
           + mtable$poverty_all + as.factor(mtable$RUC_Code) + mtable$hhold_income + mtable$perc_income 
           + mtable$unemployment_2020 +mtable$diff_ntest)
summary(fit2)

#make binary variable for rural codes
mtable$Rural <-  ifelse(mtable$RUC_Code==1, 1, ifelse(mtable$RUC_Code==2, 1, 0))

#fit multiple linear regression incidence
fit1 <- lm(mtable$diff_incidence ~  mtable$nfullyvacc + mtable$hi_edu + mtable$poverty_17 
           + mtable$poverty_all + as.factor(mtable$Rural) + mtable$hhold_income + mtable$perc_income 
           + mtable$unemployment_2020 +mtable$diff_ntest)
summary(fit1)

#fit multiple linear regression mortality
fit2 <- lm(mtable$diff_mortality ~  mtable$nfullyvacc + mtable$hi_edu + mtable$poverty_17 
           + mtable$poverty_all + as.factor(mtable$Rural) + mtable$hhold_income + mtable$perc_income 
           + mtable$unemployment_2020 +mtable$diff_ntest)
summary(fit2)


boxplot(mtable$nfullyvacc ~ as.factor(mtable$RUC_Code))
boxplot(mtable$nfullyvacc ~ as.factor(mtable$Rural))
