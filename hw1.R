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