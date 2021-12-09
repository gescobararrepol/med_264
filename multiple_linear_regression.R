###Multiple Linear Regression Model

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

#Rural as categorical variable
mtable$Rural <- as.factor(mtable$Rural)

#check assumptions for linear model
plot(mtable$diff_incidence , mtable$unemployment_2020) #imperial is an outlier
plot(mtable$diff_incidence ,mtable$perc_income)
plot(mtable$diff_incidence , mtable$poverty_17)
plot(mtable$diff_incidence , mtable$poverty_all)
plot(mtable$diff_incidence , mtable$hi_edu)

#remove Imperial outlier
mtable %>% filter(area != "Imperial") -> mtable

#fit multiple model with demographic characteristics
fitd <- lm(mtable$diff_incidence ~ mtable$unemployment_2020 + 
             mtable$perc_income + 
             mtable$poverty_all + 
             mtable$poverty_17 +
             mtable$hi_edu +
             as.factor(mtable$Rural))
summary(fitd)

#F test for pop density
drop1(fitd, test = "F")

#relevel to evaluate effect of rurality between high and medium(populaion density) 
mtable$Rural <- relevel(mtable$Rural, ref="1")
#fit multiple model with demographic characteristics
fitd <- lm(mtable$diff_incidence ~ mtable$unemployment_2020 + 
             mtable$perc_income + 
             mtable$poverty_all + 
             mtable$poverty_17 +
             mtable$hi_edu +
             as.factor(mtable$Rural))
summary(fitd) #high (2) and medium (1) Population are significantly difference without correction p-value = 0.00321

#Bonferroni correction for high vs medium
0.00321*3
#or
p.adjust(0.00321, method = "bonferroni", n = 3)

#### Eta squared, partial eta squared
# install.packages(lsr)
library(lsr)
etaSquared(fitd, anova=TRUE)

#summary(fitd)$coefficients


#fit multiple model with demographic characteristics
fitm <- lm(mtable$diff_mortality ~ mtable$unemployment_2020 + 
             mtable$perc_income + 
             mtable$poverty_all + 
             mtable$poverty_17 +
             mtable$hi_edu +
             as.factor(mtable$Rural))
summary(fitm)

#F test for pop density
drop1(fitm, test = "F")

#### Eta squared, partial eta squared
etaSquared(fitm, anova=TRUE)

summary(fitm)$coefficients

#plot with high edu and pop density

#fitd2 <- lm(mtable$diff_incidence ~ mtable$hi_edu + mtable$Rural)

 
# plot(mtable$diff_incidence ~ mtable$hi_edu, type="n", cex.lab=2)
# #points(mtable$diff_incidence ~ mtable$hi_edu, subset=(mtable$Rural==0), pch=16, cex=1, col=2)
# points(mtable$diff_incidence ~ mtable$hi_edu, subset=(mtable$Rural==1), pch=16, cex=1, col=7)
# points(mtable$diff_incidence ~ mtable$hi_edu, subset=(mtable$Rural==2), pch=16, cex=1, col=4)
# beta = fitd2$coefficients
# #abline(a=beta[1], b=beta[2], col=2)
# abline(a=beta[1]+beta[3], b=beta[2], col=7, pch=6)
# abline(a=beta[1]+beta[4], b=beta[2], col=4)
# beta = fitd$coefficients
# #abline(a=beta[1], b=beta[2], col=2)
# abline(a=beta[1]+beta[7], b=beta[6], col=7)
# abline(a=beta[1]+beta[8], b=beta[6], col=4)
# 
# # 
# fitd$coefficients
# 
# # fith <- lm(mtable$diff_incidence ~ mtable$hi_edu + mtable$Rural)
# # summary(fith)

     