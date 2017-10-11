#explore health insurance

library(tidyverse)
library(readxl)
library(RSocrata)
library(data.table)

setwd('~/Documents/data_viz/health_insurance_marketplace/')

benifits <- read_csv('Benefits_Cost_Sharing_PUF.csv')

rates <- read_csv('Rate_PUF.csv')

service_area <- read_csv('Service_Area_PUF.csv')

plan_attributes <- read_csv('Plan_Attributes_PUF.CSV')

cross_walk <- read_csv('plan-id-crosswalk-puf.CSV')

network <- read_csv("Network_PUF.csv")

b_rules <- read_csv('Business_Rules_PUF_Reformat.csv')

aptc<- read_xlsx('County_Level_Demographics_2016.xlsx', sheet='APTC', skip = 2)

csr<- read_xlsx('County_Level_Demographics_2016.xlsx', sheet='CSR', skip = 2)

consumer_type <- read_xlsx('County_Level_Demographics_2016.xlsx', sheet='Consumer Type', skip = 2)

hh_income <- read_xlsx('County_Level_Demographics_2016.xlsx', sheet='Household Income', skip = 2)

race <- read_xlsx('County_Level_Demographics_2016.xlsx', sheet='Race', skip = 2)

age_group <- read_xlsx('County_Level_Demographics_2016.xlsx', sheet='Age Group', skip = 2)

to_replace = colnames(hh_income)

replace_with = gsub(" ","_",tolower(colnames(hh_income)))

hh_income <- setnames(hh_income, c(to_replace), c(replace_with))

#^loop for other datasets?
