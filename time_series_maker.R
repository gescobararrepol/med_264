###Functions for time serie maker

## source
#source("Linear_regression.R")

## New table based on latest data

## new function to make timeframes
make_time <- function(cv = covid, 
                      vart = vartable, 
                      vind = "cumulative_cases", 
                      vdp = "hhold_income") {
  ## define Dates
  dates <- covid$date %>% table %>% names() ## all dates
  ### FOR LOOP
  pvals = list()
  betas = list()
  for (t in dates) {
    covid_last <- cv %>% filter(date == t)
    #Create Master Table
    master_table <- merge(covid_last, vart, by.x = "area", by.y = "area")
    ## model 
    #create independent variable
    v_i <- master_table[,vind]
    master_table$varind1 <- v_i/master_table$population * 100
    #create dependent variable 
    master_table$var_dp <- master_table[,vdp]
    fit <- lm(master_table$varind1 ~ master_table$var_dp)
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
  return(timeseries)
}

# 
# ## run function 
# new_time <- make_time(vind = "cumulative_deaths")
# 
# #New table
# 
# new_time %>% 
#   #filter(time > as.Date('2021-08-01')) %>% 
#   ggplot() + 
#   
#   aes(x = as.Date(time)) + 
#   geom_point(aes(y = -log10(pvals))) + 
#   geom_point(aes(y = betas*100000), col="blue") +
#   scale_y_continuous(name = "Significance(-log10(p-val))", 
#                      sec.axis = sec_axis(~.*1, name = "Betas")) +
#   theme_classic() + theme(axis.text.x = element_text(size = 5, angle = 90))
# 


plot_time <- function(timetable ,adjv = 100000) {
  ggplot(timetable) + 
    aes(x = as.Date(time)) + 
    geom_point(aes(y = -log10(pvals))) + 
    geom_point(aes(y = betas*adjv), col="blue") +
    scale_y_continuous(name = "Significance(-log10(p-val))", 
                       sec.axis = sec_axis(~.*1, name = "Betas")) +
    theme_classic() + theme(axis.text.x = element_text(size = 5, angle = 90))
  
}




