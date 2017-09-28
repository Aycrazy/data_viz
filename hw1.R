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

 
 