###Univariate Linear Regression 

# load libraries
library(tidyverse)
library(ggplot2)

#Load master table
mtable <- read.csv("Master_Table.csv")

#Linear regression each variable

#linear regression for fully vaccinated

#normalize fully vacc by population
mtable$nfullyvacc <- mtable$fullyvacc/mtable$population

#organize master table
mtable <- mtable %>% relocate(diff_deaths, FIPS, diff_test, .after = population)
mtable <- mtable[,-1]
## columns to be iterated
colnames(mtable)[-c(1:9)]

## Iteration for Incidence
models = list()
# LOOP
for (v in colnames(mtable)[-c(1:9)]) {
  models[[v]] <- lm(mtable[,"diff_incidence"] ~ mtable[,v])
}
## get r-sq , p-val and beta
summary(models[[1]])
coef(models[[1]])[2] ## beta
summary(models[[1]])$adj.r.squared ## r.square
summary(models[[1]])$coefficients[2,4] ## p-val

sapply(models, function(x) data.frame(
  #name = names(x),
  beta = unname(coef(x)[2]),
  rsq = unname(summary(x)$adj.r.squared),
  pval = unname(summary(x)$coefficients[2,4])
)) %>% t %>% as.data.frame -> results_inc



## Iteration for mortality
models = list()
# LOOP
for (v in colnames(mtable)[-c(1:9)]) {
  models[[v]] <- lm(mtable[,"diff_mortality"] ~ mtable[,v])
}
## get r-sq , p-val and beta
summary(models[[1]])
coef(models[[1]])[2] ## beta
summary(models[[1]])$adj.r.squared ## r.square
summary(models[[1]])$coefficients[2,4] ## p-val

sapply(models, function(x) data.frame(
  #name = names(x),
  beta = unname(coef(x)[2]),
  rsq = unname(summary(x)$adj.r.squared),
  pval = unname(summary(x)$coefficients[2,4])
)) %>% t %>% as.data.frame -> results_mort

do.call(rbind.data.frame, results_mort) -> results_mort
do.call(rbind.data.frame, results_inc) -> results_inc

write.csv(results_mort, "univariate_model_mortality.csv")
write.csv(results_inc, "univariate_model_incidence.csv")

#fit <- lm(mtable$diff_incidence ~ mtable$unemployment_2020)
#summary(fit)

#fit <- lm(mtable$diff_incidence ~ mtable$nfullyvacc)
#summary(fit)

