###Multiple Linear Regression with vaccines and tests

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
mtable %>% filter(area != "Imperial") -> mtable

#Rural as categorical variable
mtable$Rural <- as.factor(mtable$Rural)

#model for vaccines
fitv <- lm(mtable$diff_incidence ~ mtable$nfullyvacc)
summary(fitv)

#model for vaccines with confounders
fitvc <- lm(mtable$diff_incidence ~ mtable$nfullyvacc +
             mtable$unemployment_2020 + 
             mtable$perc_income + 
             mtable$poverty_all + 
             mtable$poverty_17 +
             mtable$hi_edu +
             mtable$Rural +
             mtable$diff_ntest)
      
summary(fitvc)

# #plot vaccine vs incidence
# plot(mtable$nfullyvacc , mtable$diff_incidence,
#      ylab = "% Fully vaccinated population",
#      xlab = "% Incidence of COVID-19",
#      )
# abline(fitv, col = "blue", cex = 10)
# abline(fitvc, col = "lightblue")


fitv$coefficients
mtable %>% ggplot() +
  aes(x = nfullyvacc, y=diff_incidence) + 
  geom_point(col= "blue", size = 2) +
  geom_abline(aes(intercept = fitv$coefficients[1], slope = fitv$coefficients[2], 
              color = "lightblue"), size = 1) +
  geom_abline(aes(intercept = fitvc$coefficients[1], slope = fitvc$coefficients[2], 
              color = "blue"), size = 1) +
  xlab  ("% Fully vaccinated population") +
  ylab ( "Incidence of COVID-19 per 100,000" )+
  scale_color_identity(guide = "legend", labels = c("Adjusted", "Non-adjusted")) +
  theme_bw() + theme(text = element_text(size = 19), 
                     legend.position = "right") + guides(color = guide_legend("Model"))

#F test for population density
drop1(fitvc, test = "F")

#relevel to evaluate effect of rurality between high and medium(populaion density) 
mtable$Rural <- relevel(mtable$Rural, ref="1")
#fit multiple model with demographic characteristics
fitvc <- lm(mtable$diff_incidence ~ mtable$nfullyvacc +
              mtable$unemployment_2020 + 
              mtable$perc_income + 
              mtable$poverty_all + 
              mtable$poverty_17 +
              mtable$hi_edu +
              mtable$Rural +
              mtable$diff_ntest)

summary(fitvc) #high (2) and medium (1) Population are significantly difference without correction p-value = 0.00321

#Bonferroni correction for high vs medium
0.01354*3 #0.04062

#### Eta squared, partial eta squared
# install.packages(lsr)
library(lsr)
etaSquared(fitvc, anova=TRUE)

####mortality

#model for vaccines
fitv <- lm(mtable$diff_mortality ~ mtable$nfullyvacc)
summary(fitv)

#model for vaccines with confounders
fitvc <- lm(mtable$diff_mortality ~ mtable$nfullyvacc +
              mtable$unemployment_2020 + 
              mtable$perc_income + 
              mtable$poverty_all + 
              mtable$poverty_17 +
              mtable$hi_edu +
              mtable$Rural +
              mtable$diff_ntest)

summary(fitvc)

# #plot vaccine vs incidence
# plot(mtable$nfullyvacc , mtable$diff_incidence,
#      ylab = "% Fully vaccinated population",
#      xlab = "% Incidence of COVID-19",
#      )
# abline(fitv, col = "blue", cex = 10)
# abline(fitvc, col = "lightblue")


fitv$coefficients
mtable %>% ggplot() +
  aes(x = nfullyvacc, y=diff_mortality) + 
  geom_point(col= "blue", size = 2) +
  geom_abline(aes(intercept = fitv$coefficients[1], slope = fitv$coefficients[2], 
                  color = "lightblue"), size = 1) +
  geom_abline(aes(intercept = fitvc$coefficients[1], slope = fitvc$coefficients[2], 
                  color = "blue"), size = 1) +
  xlab  ("% Fully vaccinated population") +
  ylab ( "Mortality of COVID-19 per 100,000" )+
  scale_color_identity(guide = "legend", labels = c("Adjusted", "Non-adjusted")) +
  theme_bw() + theme(text = element_text(size = 19), 
                     legend.position = "right") + guides(color = guide_legend("Model"))

#F test for pop density
drop1(fitvc, test = "F")

#### Eta squared, partial eta squared
# install.packages(lsr)
library(lsr)
etaSquared(fitvc, anova=TRUE)

#plot(mtable$poverty_all, mtable$diff_incidence )
#fit <- lm(mtable$diff_incidence ~ mtable$poverty_all + mtable$nfullyvacc + mtable$diff_ntest)
#summary(fit)


#vaccine vs college
fite <- lm(mtable$nfullyvacc ~ mtable$hi_edu)
summary(fite)
fite$coefficients

mtable %>% ggplot() +
  aes(y = nfullyvacc, x=hi_edu) + 
  geom_point(col= "blue", size = 2) +
  geom_abline(aes(intercept = fite$coefficients[1], slope = fite$coefficients[2], 
                  color = "lightblue"), size = 1) +
  ylab  ("% Fully vaccinated population") +
  xlab ( "% Population with college completed" )+
  theme_bw() + theme(text = element_text(size = 30),
                     legend.position = "none") #+ guides(color = guide_legend("Model"))

#vaccine vs poverty
fite <- lm(mtable$nfullyvacc ~ mtable$poverty_all)
summary(fite)
fite$coefficients

mtable %>% ggplot() +
  aes(y = nfullyvacc, x=poverty_all) + 
  geom_point(col= "blue", size = 2) +
  geom_abline(aes(intercept = fite$coefficients[1], slope = fite$coefficients[2], 
                  color = "lightblue"), size = 1) +
  ylab  ("% Fully vaccinated population") +
  xlab ( "% Poverty" )+
  theme_bw() + theme(text = element_text(size = 30),
                     legend.position = "none") #+ guides(color = guide_legend("Model"))

mtable %>% ggplot() +
  aes(y = nfullyvacc, x=poverty_all) + 
  geom_point(col= "blue", size = 2) +
  geom_abline(aes(intercept = fite$coefficients[1], slope = fite$coefficients[2], 
                  color = "lightblue"), size = 1) +
  ylab  ("% Fully vaccinated population") +
  xlab ( "% Poverty" )+
  theme_bw() + theme(text = element_text(size = 30),
                     legend.position = "none") #+ guides(color = guide_legend("Model"))


#Boxplot for vaccine vs Pop density
mtable %>% ggplot() +
  aes(x = Rural, y = nfullyvacc, fill = Rural) +
  geom_boxplot() + 
  ylab("% Vaccinated people") + xlab("Population density") +
  theme_bw() + theme(text = element_text(size = 30), legend.position = "none")


cor.test(mtable$nfullyvacc, mtable$hi_edu, method = "pearson")
cor.test(mtable$nfullyvacc, mtable$poverty_all, method = "pearson")

#statistical test for vaccine vs population density
kruskal.test(mtable$nfullyvacc ~ mtable$Rural)

#pairewise comparison
pairwise.wilcox.test(mtable$nfullyvacc , mtable$Rural,
                     p.adjust.method = "BH")


# #model for vaccines with confounders relevel 
# mtable$Rural <- relevel(mtable$Rural, ref="2")
# fitvc <- lm(mtable$diff_incidence ~ mtable$nfullyvacc +
#               mtable$unemployment_2020 + 
#               mtable$perc_income + 
#               mtable$poverty_all + 
#               mtable$poverty_17 +
#               mtable$hi_edu +
#               mtable$Rural +
#               mtable$diff_ntest)
# 
# summary(fitvc)








