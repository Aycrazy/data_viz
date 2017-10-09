setwd('~/Documents/data_viz/')
library(tidyverse)
library(readr)
library(stringr)
library(dplyr)
library(readxl)
library(ggmap)
library(acs)
library(RSocrata)
library(data.table)

ssl <- data.frame(read_csv('Strategic_Subject_List.csv'))

ssl_wt <- ssl %>% filter(!is.na(CENSUS.TRACT)) %>%
       filter(CENSUS.TRACT != "0")


ssl_wt_drug <- ssl_wt %>% filter(DRUG.I == 'Y')

plot1 <- ggplot(data = ssl_wt_drug, aes(x = age_curr, y = predictor_rat_narcotic_arrests, color = race_code_cd))+
        geom_boxplot()

plot1 + labs(title = "How Age and Race Impact Predict Narcotics Arrest Score", subtitle = "We will take a look at how age and race impact predicted narcotics arrest score",
             caption = "Source: City of Chicago Data") + ylab('Predicted Number of Narcotics Arrests') + xlab('Age Range of Last Arrest')


age_list <- unique(ssl$AGE.CURR)

plot2 <- ggplot(data = ssl_wt_drug, aes(x = age_curr, y = narcotics_arr_cnt, fill = narcotics_arr_cnt ))+
    geom_tile(alpha = .3)+
    coord_flip()+
    scale_alpha_manual(values= c(0.1,0.2,0.3,0.4,0.5,0.6,0.7),labels = age_list[c(1,2,3,4,5,6,7)])
    
plot2 + labs(title = "Who Gets Arrested for Drug Posession by Age Range")+ labs(subtitle = "We will take a look at where the Strategic Subject list members got arrested and how old they were at the time",
             caption = "Source: City of Chicago Data")+ labs(colour =  "Narcotics Arrest Count") + xlab('Age Range of Last Arrest') + ylab('Number of Arrests')


samhsa <- read_xlsx("bhtf.xlsx")

samhsa_batch <- samhsa[,c('street1', 'city', 'state', 'zip')]

write.csv(samhsa_batch, 'samhsa_batch.csv')

ggplot(data = samhsa)+
    stat_count(aes(x = type_facility, fill = type_facility))

samhsa_batch <- within(samhsa_batch,  addresses <- paste(street1, city, state, zip, sep=", "))

samhsa$addresses <- samhsa_batch$addresses

to_replace = colnames(ssl_wt_drug)

replace_with = gsub("[.]","_",tolower(colnames(ssl_wt_drug)))

ssl_wt_drug <- setnames(ssl_wt_drug, c(to_replace), c(replace_with))

samhsa_geocoded <- rename(read_csv('GeocodedResults.csv'), addresses = address)

ssl_wt_drug_by_ct <- ssl_wt_drug %>% 
    count(census_tract)

ssl_wt_drug_by_ct <- rename(ssl_wt_drug_by_ct, total = n)
              
ssl_wt_drug <- left_join(ssl_wt_drug, ssl_wt_drug_by_ct, by = "census_tract")

samhsa_geocoded$census_tract <- as.character(samhsa_geocoded$census_tract)

samhsa_ssl <- left_join(ssl_wt_drug, samhsa_geocoded, by ="census_tract")

samhsa_ssl_no_na <- samhsa_ssl %>% filter(!is.na(addresses))

ssl_wt_drug_by_ct_lim <- samhsa_ssl %>% filter(total > 200)

#ggplot(data = ssl_wt_drug_by_ct_lim, aes(x = census_tract, y = total))+
#    geom_point(color = "blue")+
#    geom_point(data = samhsa_ssl_no_na, aes( x = community_area, y = total), color = "red")+
#    theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot3 = ggplot(data = samhsa_ssl, aes(x = community_area, y = total, color = census_tract))+
    geom_count(shape = 21, show.legend = FALSE)+
    geom_point(data = samhsa_ssl_no_na, aes( x = community_area, y = total), shape = 23, fill = "red", show.legend = FALSE)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot3 + labs(title = "Narcotics Arrests by Community Area, and Facilities... Where are they?", subtitle = "Each bubble represents a census tract and the number of drug arrests that occcurred inside them.  There is one unique point that represents where there is any overlap between where narcots arrests occurr and where a mental health or substance use treatment facility, or Bupenaphren treatment provider is present",
             caption = "Source: City of Chicago Data") + ylab('Number of Arrests') + xlab('Community Area')
    

