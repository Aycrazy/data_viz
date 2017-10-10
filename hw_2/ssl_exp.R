setwd('~/Documents/data_viz/')
library(tidyverse)
library(readxl)
library(ggmap)
library(acs)
library(RSocrata)
library(data.table)

ssl <- data.frame(read_csv('Strategic_Subject_List.csv'))

ssl_wt <- ssl %>% filter(!is.na(CENSUS.TRACT)) %>%
       filter(CENSUS.TRACT != "0")


ssl_wt_drug <- ssl_wt %>% filter(DRUG.I == 'Y')


to_replace = colnames(ssl_wt_drug)

replace_with = gsub("[.]","_",tolower(colnames(ssl_wt_drug)))

ssl_wt_drug <- setnames(ssl_wt_drug, c(to_replace), c(replace_with))


plot1 <- ggplot(data = ssl_wt_drug, aes(x = age_curr, y = predictor_rat_narcotic_arrests, color = race_code_cd))+
        geom_boxplot()

plot1 + labs(title = "How Age and Race Impact Predict Narcotics Arrest Score", subtitle = "We will take a look at how age and race impact predicted narcotics arrest score",
             caption = "Source: City of Chicago Data", color = 'Race Code') + ylab('Predicted Number of Narcotics Arrests') + xlab('Age Range of Last Arrest')


age_list <- unique(ssl$AGE.CURR)

plot2 <- ggplot(data = ssl_wt_drug)+
    geom_bar(aes(x = narcotics_arr_cnt, fill = age_curr ), position = )

plot2 + labs(title = "Who Gets Arrested for Drug Posession by Age Range")+ labs(subtitle = "We will take a look at where the Strategic Subject list members got arrested and how old they were at the time",
             plot.caption = "Source: City of Chicago Data", fill = 'Age Range')+ xlab('Number of Arrests') + ylab('Number of People of Indicated Age with X Arrests')

samhsa <- read_xlsx("bhtf.xlsx")

samhsa_batch <- samhsa[,c('street1', 'city', 'state', 'zip')]

write.csv(samhsa_batch, 'samhsa_batch.csv')

ggplot(data = samhsa)+
    stat_count(aes(x = type_facility, fill = type_facility))

samhsa_batch <- within(samhsa_batch,  addresses <- paste(street1, city, state, zip, sep=", "))

samhsa$addresses <- samhsa_batch$addresses

#to_replace = colnames(ssl_wt_drug)

#replace_with = gsub("[.]","_",tolower(colnames(ssl_wt_drug)))

#ssl_wt_drug <- setnames(ssl_wt_drug, c(to_replace), c(replace_with))

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

plot3 = ggplot(data = samhsa_ssl_sorted, aes(x = community_area, y = total))+
    geom_count(shape = 21, show.legend = FALSE, color = 'blue')+
    geom_point(data = samhsa_ssl_no_na, aes( x = community_area, y = total), shape = 23, fill = "red", show.legend = FALSE)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))#+
    #scale_x_discrete(limits=sort(order(as.integer(samhsa_ssl$latitude))))

plot3 + labs(title = "Narcotics Arrests by Community Area, and Facilities... Where are they?", subtitle = "Each bubble represents a census tract and the number of drug arrests that occcurred\n inside them.  There is one unique point that represents where there is any overlap between where narcotics\n arrests occurr and where a mental health or substance use treatment facility, or Bupenaphren treatment\n provider is present",
             caption = "Source: City of Chicago Data") + ylab('Number of Arrests') + xlab('Community Area')
    
unique(ssl$latest_date)

colnames(samhsa_ssl)

samhsa_ssl_sorted <- samhsa_ssl[order(samhsa_ssl$latitude),]

