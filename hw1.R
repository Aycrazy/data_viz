 #Andrew Yaspan's exploratory analysis of DoT FARS data

setwd("~/Documents/data_viz")

install.packages('readr')
install.packages('haven')
install.packages('tidyr')
install.packages('stringr')
install.packages('ggplot2')
install.packages('dplyr')

library('readr')
library('haven')
library('tidyr')
library('stringr')
library('ggplot2')
library('dplyr')

acc2014 <- read_sas('accident.sas7bdat')
acc2015 <- read_csv('nhtsa_2015_dir/accident.csv')

ls()

acc2014 <- acc2014 %>% mutate(TWAY_ID2 = na_if(TWAY_ID2, ""))

dim(acc2014)
dim(acc2015)

colnames(acc2015[!colnames(acc2015) %in% colnames(acc2014)])
#"RUR_URB"  "FUNC_SYS" "RD_OWNER" missing from 2014
 colnames(acc2014[!colnames(acc2014) %in% colnames(acc2015)])
#"ROAD_FNC" missing from 2015
 
acc_combine <- bind_rows(list(acc2014,acc2015))

acc_combine %>% count(RUR_URB)

## A tibble: 6 x 2
#RUR_URB     n
#<int> <int>
#1       1 15293
#2       2 14414
#3       6   114
#4       8  2320
#5       9    25
#6      NA 30056
    
#There are 30056 NA rows because there are 30,0056 rows in acc2014, which does not have the RUR_URB variable

fips = read_csv('fips.csv')

glimpse(fips)

acc_combine$STATE <- as.character(acc_combine$STATE)

acc_combine$COUNTY <- as.character(acc_combine$COUNTY)

acc_combine <- acc_combine %>% mutate(STATE = str_pad(STATE,width=2,side='left',pad='0'))
acc_combine <-acc_combine %>% mutate(COUNTY = str_pad(COUNTY,width=3,side='left',pad='0'))
 
acc_combine <- rename(acc_combine, StateFIPSCode = STATE, CountyFIPSCode = COUNTY)

fips_acc <- left_join(acc_combine,fips)

#It seemed to make more sense to group_by StateFipsCode and YEAR, after discussing this problem
#with a classmate and reading the directions further, we saw that NA values would be expected
#and so I then grouped by StateName and Year.

agg <- fips_acc %>%
        group_by(StateName,YEAR) %>%
        summarize(TOTAL=sum(FATALS))

agg_wide <- data.frame(spread(agg, YEAR, TOTAL))

agg_wide <- rename(agg_wide, fifteen = X2015, fourteen = X2014)

# I think normally percent difference would be calculated with by 2015-2014/2015 not 2015-2014/2014
#which is what I have below (but it matches the provided answer)

agg <- agg_wide %>% 
    mutate(perc_diff = ((fifteen-fourteen)/fourteen)*100) %>%
    arrange(perc_diff) %>%
    filter(perc_diff > 15) %>%
    filter(!is.na(StateName))

glimpse(agg)

