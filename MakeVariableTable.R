###Linear Regression 3
library(tidyverse)
###Supreme Master Table
hhold <- read.csv("UnemploymentReport_ge.csv", 
                  sep = ";", 
                  skip = 2, #remove first 2 rows
                  header = TRUE, check.names = FALSE)
poverty <- read.csv("PovertyReport.csv", sep = ";")
#rural <- read.csv("ruralurbancodes.csv", sep = ";")
education <- read.csv("EducationReport.csv", sep = ";", check.names = FALSE)
vaccine <- read.csv("https://data.chhs.ca.gov/dataset/e283ee5a-cf18-4f20-a92c-ee94a2866ccd/resource/130d7ba2-b6eb-438d-a412-741bde207e1c/download/covid19vaccinesbycounty.csv")

## pre-processing
#hhold
hhold$area <- hhold$Name %>% 
  gsub(pattern = " County, CA", replacement = "") %>% 
  gsub(pattern = " County/city, CA", replacement = "")
gsub("%", "", 
     hhold$`% of State Median HH Income`) %>% as.numeric() -> hhold$perc_income
gsub("\\$", "", 
     hhold$`Median Household Income (2019)`) %>% 
  gsub("\\,", "", .) %>% as.numeric() -> hhold$hhold_income

#poverty
poverty$Name %in% hhold$area

#education
education$Name %>% 
  gsub(pattern = ", CA        ", replacement = "") -> education$area #%in% hhold$area
education$`2015-2019` %>% gsub(pattern = "%", "", .) %>% as.numeric() -> education$`2015-2019`
#vaccine
vaccine$county %>% table %>% names


### mega table

TheMasterTable <- merge(hhold, poverty,
                        by.x = "area", by.y = "Name")
TheMasterTable <- merge(TheMasterTable, education,
                        by= "area")
TheMasterTable<-  TheMasterTable[,c("area",
                  "FIPS",
                  "2020",
                  "perc_income",
                  "hhold_income",
                  "RUC_Code",
                  "Percent_all",
                  "Percent_17",
                  "2015-2019")]
colnames(TheMasterTable)[3] <- "unemployment_2020"
colnames(TheMasterTable)[7] <- "poverty_all"
colnames(TheMasterTable)[8] <- "poverty_17"
colnames(TheMasterTable)[9] <- "hi_edu"
## write file
write.csv(TheMasterTable, "Variables.csv", row.names = FALSE)

