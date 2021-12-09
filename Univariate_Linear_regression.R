###Univariate Linear Regression v2

# load libraries
library(tidyverse)
library(ggplot2)

#Load master table
mtable <- read.csv("Master_Table.csv")

#Linear regression each variable

#linear regression for fully vaccinated

#normalize fully vacc by population
mtable$nfullyvacc <- mtable$fullyvacc/mtable$population

#multiply incidence and mortality by 100,000
mtable$diff_incidence <- mtable$diff_incidence*100000
mtable$diff_mortality <- mtable$diff_mortality*100000

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
-log10(summary(models[[1]])$coefficients[2,4]) ## p-val

sapply(models, function(x) data.frame(
  #name = names(x),
  beta = unname(coef(x)[2]),
  rsq = unname(summary(x)$adj.r.squared),
  pval = unname(-log10(summary(x)$coefficients[2,4]))
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
-log10(summary(models[[1]])$coefficients[2,4]) ## p-val

sapply(models, function(x) data.frame(
  #name = names(x),
  beta = unname(coef(x)[2]),
  rsq = unname(summary(x)$adj.r.squared),
  pval = unname(-log10(summary(x)$coefficients[2,4]))
)) %>% t %>% as.data.frame -> results_mort

do.call(rbind.data.frame, results_mort) -> results_mort
do.call(rbind.data.frame, results_inc) -> results_inc

write.csv(results_mort, "univariate_model_mortality.csv")
write.csv(results_inc, "univariate_model_incidence.csv")

##Compare incidence and rurality per density population

#make binary variable for rural codes
mtable$Rural <-  ifelse(mtable$RUC_Code==1, 2,ifelse(mtable$RUC_Code==2, 2, 
                                                     ifelse(mtable$RUC_Code==3, 1,
                                                            ifelse(mtable$RUC_Code==4, 1,
                                                                   ifelse(mtable$RUC_Code==5, 1,
                                                                          ifelse(mtable$RUC_Code==6, 1,
                                                                                 ifelse(mtable$RUC_Code==7, 1,0)))))))
sum(mtable$Rural==2)
sum(mtable$Rural==1)
sum(mtable$Rural==0)

#significant
-log10(0.05)

#Rural as categorical variable
mtable$Rural <- as.factor(mtable$Rural)

#boxplot(mtable$diff_incidence ~ mtable$Rural,xlab = "Rurality",
 #       ylab = "Incidence per 100,000")

#Boxplot for incidence vs Pop density
mtable %>% ggplot() +
  aes(x = Rural, y = diff_incidence, fill = Rural) +
  geom_boxplot() + 
  ylab("Incidence per 100,000") + xlab("Population density") +
  theme_bw() + theme(text = element_text(size = 30), legend.position = "none")
#ggsave(last_plot(), "boxplot1.png", width = 5, height = 5, device = "png")

#statistical test for incidence vs population density
kruskal.test(mtable$diff_incidence ~ mtable$Rural)

#pairewise comparison
pairwise.wilcox.test(mtable$diff_incidence , mtable$Rural,
                     p.adjust.method = "BH")

#boxplot(mtable$diff_incidence ~ mtable$Rural,xlab = "Rurality",
#       ylab = "Incidence per 100,000")


#Boxplot for mortality vs Pop density
mtable %>% ggplot() +
  aes(x = Rural, y = diff_mortality, fill = Rural) +
  geom_boxplot() + 
  ylab("Mortality per 100,000") + xlab("Population density") +
  theme_bw() + theme(text = element_text(size = 30), legend.position = "none")
#ggsave(last_plot(), "boxplot1.png", width = 5, height = 5, device = "png")

#statistical test for incidence vs population density
kruskal.test(mtable$diff_mortality ~ mtable$Rural)

#pairewise comparison
pairwise.wilcox.test(mtable$diff_mortality , mtable$Rural,
                     p.adjust.method = "BH")



