#explore health insurance

library(tidyverse)
library(readxl)
library(RSocrata)
library(data.table)

setwd('~/Documents/data_viz/health_insurance_marketplace/')

benifits <- read_csv('Benefits_Cost_Sharing_PUF.csv')

rates<- read_csv('Rate_PUF.csv')

service_area <- read_csv('ServiceArea_PUF.csv')

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
dframeslist <- list(age_group, aptc, b_rules, benifits,
                 consumer_type, cross_walk, csr, hh_income,
                 plan_attributes, network, race, rates, service_area)

ldframes <- lapply(dframeslist,function(d){ 
    to_replace = colnames(d)
    replace_with = gsub(" ","_",tolower(colnames(d)))
    d <-setnames(d, c(to_replace), c(replace_with))
    })

hh_income <- hh_income %>% filter(state_name %in% c('MI','WI','IL','IN'))
race <- race %>% filter(state_name %in% c('MI','WI','IL','IN'))
consumer_type <- consumer_type %>% filter(state_name %in% c('MI','WI','IL','IN'))
csr <- csr %>% filter(state_name %in% c('MI','WI','IL','IN'))
aptc <- aptc %>% filter(state_name %in% c('MI','WI','IL','IN'))
b_rules <- b_rules %>% filter(statecode %in% c('MI','WI','IL','IN'))
network <- network %>% filter(statecode %in% c('MI','WI','IL','IN'))
cross_walk <- cross_walk %>% filter(state %in% c('MI','WI','IL','IN'))
rates <- rates %>% filter(statecode %in% c('MI','WI','IL','IN'))
age_group <- age_group %>% filter(state_name %in% c('MI','WI','IL','IN'))
plan_attributes <- plan_attributes %>% filter(statecode %in% c('MI','WI','IL','IN'))
service_area <- service_area %>% filter(statecode %in% c('MI','WI','IL','IN'))
benifits <- benifits %>% filter(statecode %in% c('MI','WI','IL','IN'))

#list2env(ldframes, envir=.GlobalEnv)

#exploration of health plans in the 4 states with mental health and substance use benifits
mh_benifit_names = c(unique(benifits[grep("Mental|Substance", benifits$benefitname),]$benefitname))
mh_benifit_pids = c(unique(benifits[grep("Mental|Substance", benifits$benefitname),]$planid))
mh_benifit_states = c(unique(benifits[grep("Mental|Substance", benifits$benefitname),]$statecode))
#which plans offer to individuals only in what counties in comparison with countys with more families

colScale <- scale_colour_manual(name = "grp",values = myColors)

#plot1
benifits %>%
    filter(benefitname %in% mh_benifit_names) %>%
    ggplot(aes(x = benefitname, fill = statecode, color = isstatemandate))+
        geom_bar( position = "dodge")+
    theme(axis.text.x = element_text(angle = 90))+
    scale_color_manual(values =  c("Yes" = "#000000"))+
    coord_flip()+
    labs(title = "Mental Health and Substance Use Health Benefits by State",
         subtitle = "This plot displays the number of plans offering mental health\n and substance use beneifts and whether or not the indicated\n benefit is mandated by the state.",
         fill = "State Abreviation", color = "Mandated", y ='Number of Plans', x = 'Benifits' )
    
library("RSocrata")

csr_2015 <- read.socrata(
    "https://data.cms.gov/resource/us4s-qbb3.json",
    app_token = CMS_API,
    email     = "ayaspan@uchicago.edu",
    password  = CMS_PASSWORD)
#do facet grid and facet wrap

csr_2015 <- csr_2015 %>%
        mutate(no_csr = as.numeric(no_csr),
               yes_csr = as.numeric(yes_csr),
               total_plan_selections = as.numeric(total_plan_selections),
               year = '2015')

csr <- csr %>%
    mutate(no_csr = as.numeric(no_csr),
           yes_csr = as.numeric(yes_csr),
           total_plan_selections = as.numeric(total_plan_selections),
           year = '2016')

csr_2015 <- rename(csr_2015, state_name = state)

csr_2015 <- csr_2015 %>% filter(state_name %in% c('MI','WI','IL','IN'))

csr_diff <- csr %>% left_join(csr_2015, by = c('state_name', 'county_name'))

# rbind(csr_2015,csr) %>%
#     filter(state_name == 'IL') %>%
#     select(total_plan_selections) %>%
#     summarize('%25' = quantile(total_plan_selections, probs = .25),
#               '%50' = quantile(total_plan_selections, probs = .50),
#               '%75' = quantile(total_plan_selections, probs = .75),
#               'max' = max(total_plan_selections))
# 
# rbind(csr_2015,csr) %>%
#     filter(state_name == 'IL')%>%
#     #filter(county_name %in% c('Cook County', 'Dupage County', 'Lake County','Kane C'))
#     ggplot(size = '1/2')+
#         geom_text(aes(x = year, label = county_name, y = log(total_plan_selections)))+
#         geom_line(aes(x = year, group = county_name, y = log(total_plan_selections)))
#         #facet_wrap(~state_name)
# 
# text <- csr %>%
#     filter(state_name == 'IL')%>%
#     select(year, county_name, total_plan_selections) %>%
#     mutate(county_name =str_pad(county_name,width=18,side='left',pad=' '))
#     
# 
# il_plot <- rbind(csr_2015,csr) %>%
#     filter(state_name == 'IL')%>%
#     #filter(county_name %in% c('Cook County', 'Dupage County', 'Lake County','Kane County', 'Kankakee County', 'Henderson County', 'Hardin County')) %>%
#     ggplot(aes(x = year, y = total_plan_selections))+
#     geom_point(aes(color = total_plan_selections))+
#     geom_line(aes(x = year, group = county_name, y = total_plan_selections))+
#     geom_text(data =text, aes(x = year, label = county_name, y = total_plan_selections),
#                hjust = 0, size = 2, check_overlap = TRUE)+
#     scale_colour_gradientn(colours = terrain.colors(10))
# 
# il_plot2 <- rbind(csr_2015,csr) %>%
#     filter(state_name == 'IL') %>%
#     #filter(county_name %in% c('Cook County', 'Dupage County', 'Lake County','Kane County', 'Kankakee County', 'Henderson County', 'Hardin County')) %>%
#     ggplot(aes(x = year, y = log(total_plan_selections)))+
#     geom_point(aes(color = total_plan_selections))+
#     geom_line(aes(x = year, group = county_name, y = log(total_plan_selections)))+
#     geom_text(data =text, aes(x = year, label = county_name, y = log(total_plan_selections)),
#               hjust = 0, size = 2, check_overlap = TRUE)+
#     scale_colour_gradientn(colours = terrain.colors(10))
# 
# text2 <- rbind(csr) %>%
#     filter(state_name == 'IL')%>%
#     filter(log(total_plan_selections) > 5 &log(total_plan_selections) < 7)%>%
#     select(year, county_name, total_plan_selections) %>%
#     mutate(county_name =str_pad(county_name,width=18,side='left',pad=' '))
#     
#            
# il_plot3 <- rbind(csr_2015,csr) %>%
#     filter(state_name == 'IL')%>%
#     filter(log(total_plan_selections) > 5 &log(total_plan_selections) < 7) %>%
#     ggplot(aes(x = year, y = log(total_plan_selections)))+
#     geom_point(aes(color = total_plan_selections))+
#     geom_line(aes(x = year, group = county_name, y = log(total_plan_selections)))+
#     geom_text(data = text2, aes(x = year, label = county_name, y = log(total_plan_selections)),
#               size = 2, check_overlap = TRUE, hjust = 0)+
#     ylim(5, 7.25)+
#     scale_colour_gradientn(colours = terrain.colors(10))+
#     theme(axis.text = element_blank(),
#           axis.ticks = element_blank(),
#           panel.grid  = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_blank())
#     
# csr_diff <- csr_diff %>%
#     rename("yes_2015" = "yes_csr.x", "yes_2016" = "yes_csr.y", "no_2015" = "no_csr.x",
#            "no_2016" = "no_csr.y", "total_2015" = "total_plan_selections.x",
#            "total_2016" = "total_plan_selections.y") %>%
#     mutate(diff_yes = yes_2016 - yes_2015,
#            diff_no = no_2016 - no_2016,
#            diff_total = total_2016 - total_2015) 
# 
# wi_text <- csr %>%
#     filter(state_name == 'WI')%>%
#     select(year, county_name, total_plan_selections) %>%
#     mutate(county_name =str_pad(county_name,width=18,side='left',pad=' '))
# 
# 
# wi_plot <- rbind(csr_2015,csr) %>%
#     filter(state_name == 'WI')%>%
#     #filter(county_name %in% c('Cook County', 'Dupage County', 'Lake County','Kane County', 'Kankakee County', 'Henderson County', 'Hardin County')) %>%
#     ggplot(aes(x = year, y = total_plan_selections))+
#     geom_point(aes(color = total_plan_selections))+
#     geom_line(aes(x = year, group = county_name, y = total_plan_selections))+
#     geom_text(data =wi_text, aes(x = year, label = county_name, y = total_plan_selections),
#               hjust = 0, size = 2, check_overlap = TRUE)+
#     scale_colour_gradientn(colours = terrain.colors(10))
# 
# wi_text2 <- rbind(csr) %>%
#     filter(state_name == 'WI')%>%
#     filter(log(total_plan_selections) > 5 &log(total_plan_selections) <= 10)%>%
#     select(year, county_name, total_plan_selections) %>%
#     mutate(county_name =str_pad(county_name,width=18,side='left',pad=' '))          
# 
# wi_plot2 <- rbind(csr_2015,csr) %>%
#     filter(state_name == 'WI')%>%
#     filter(log(total_plan_selections) > 5 &log(total_plan_selections) <= 10) %>%
#     ggplot(aes(x = year, y = log(total_plan_selections)))+
#     geom_point(aes(color = total_plan_selections))+
#     geom_line(aes(x = year, group = county_name, y = log(total_plan_selections)))+
#     geom_text(data = wi_text2, aes(x = year, label = county_name, y = log(total_plan_selections)),
#               size = 2, check_overlap = TRUE, hjust = 0)+
#     ylim(5, 10)+
#     scale_colour_gradientn(colours = terrain.colors(10))+
#     theme(axis.text = element_blank(),
#           axis.ticks = element_blank(),
#           panel.grid  = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_blank())
# 
# in_text <- csr %>%
#     filter(state_name == 'IN')%>%
#     select(year, county_name, total_plan_selections) %>%
#     mutate(county_name =str_pad(county_name,width=18,side='left',pad=' '))
# 
# 
# in_plot <- rbind(csr_2015,csr) %>%
#     filter(state_name == 'IN')%>%
#     #filter(county_name %in% c('Cook County', 'Dupage County', 'Lake County','Kane County', 'Kankakee County', 'Henderson County', 'Hardin County')) %>%
#     ggplot(aes(x = year, y = total_plan_selections))+
#     geom_point(aes(color = total_plan_selections))+
#     geom_line(aes(x = year, group = county_name, y = total_plan_selections))+
#     geom_text(data = in_text, aes(x = year, label = county_name, y = total_plan_selections),
#               hjust = 0, size = 2, check_overlap = TRUE)+
#     scale_colour_gradientn(colours = terrain.colors(10))
# 
# in_text2 <- rbind(csr) %>%
#     filter(state_name == 'IN')%>%
#     filter(log(total_plan_selections) > 3 &log(total_plan_selections) <= 10)%>%
#     select(year, county_name, total_plan_selections) %>%
#     mutate(county_name =str_pad(county_name,width=18,side='left',pad=' '))          
# 
# in_plot2 <- rbind(csr_2015,csr) %>%
#     filter(state_name == 'IN')%>%
#     filter(log(total_plan_selections) > 3 &log(total_plan_selections) <= 10) %>%
#     ggplot(aes(x = year, y = log(total_plan_selections)))+
#     geom_point(aes(color = total_plan_selections))+
#     geom_line(aes(x = year, group = county_name, y = log(total_plan_selections)))+
#     geom_text(data = in_text2, aes(x = year, label = county_name, y = log(total_plan_selections)),
#               size = 2, check_overlap = TRUE, hjust = 0)+
#     ylim(3, 10)+
#     scale_colour_gradientn(colours = terrain.colors(10))+
#     theme(axis.text = element_blank(),
#           axis.ticks = element_blank(),
#           panel.grid  = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_blank())
# 
# mi_text <- csr %>%
#     filter(state_name == 'MI')%>%
#     select(year, county_name, total_plan_selections) %>%
#     mutate(county_name =str_pad(county_name,width=18,side='left',pad=' '))
# 
# 
# mi_plot <- rbind(csr_2015,csr) %>%
#     filter(state_name == 'MI')%>%
#     #filter(county_name %in% c('Cook County', 'Dupage County', 'Lake County','Kane County', 'Kankakee County', 'Henderson County', 'Hardin County')) %>%
#     ggplot(aes(x = year, y = total_plan_selections))+
#     geom_point(aes(color = total_plan_selections))+
#     geom_line(aes(x = year, group = county_name, y = total_plan_selections))+
#     geom_text(data = mi_text, aes(x = year, label = county_name, y = total_plan_selections),
#               hjust = 0, size = 2, check_overlap = TRUE)+
#     scale_colour_gradientn(colours = terrain.colors(10))
# 
# mi_text2 <- rbind(csr) %>%
#     filter(state_name == 'MI')%>%
#     filter(log(total_plan_selections) > 3 &log(total_plan_selections) <= 10)%>%
#     select(year, county_name, total_plan_selections) %>%
#     mutate(county_name =str_pad(county_name,width=18,side='left',pad=' '))          
# 
# mi_plot2 <- rbind(csr_2015,csr) %>%
#     filter(state_name == 'MI')%>%
#     filter(log(total_plan_selections) > 3 &log(total_plan_selections) <= 10) %>%
#     ggplot(aes(x = year, y = log(total_plan_selections)))+
#     geom_point(aes(color = total_plan_selections))+
#     geom_line(aes(x = year, group = county_name, y = log(total_plan_selections)))+
#     geom_text(data = mi_text2, aes(x = year, label = county_name, y = log(total_plan_selections)),
#               size = 2,  hjust = 0, check_overlap = TRUE)+
#     ylim(3, 10)+
#     scale_colour_gradientn(colours = terrain.colors(10))+
#     theme(axis.text = element_blank(),
#           axis.ticks = element_blank(),
#           panel.grid  = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_blank())


largest_counties_text <- rbind(csr) %>%
    filter(log(total_plan_selections) > 9 &log(total_plan_selections) <= 12.5)%>%
    select(year, county_name, state_name, total_plan_selections) %>%
    mutate(county_name =str_pad(county_name,width=18,side='left',pad=' ')) %>%
    mutate(county_name = paste(county_name,', ',state_name))

#plot 2
largest_plot <- rbind(csr_2015,csr) %>%
    filter(log(total_plan_selections) > 9 &log(total_plan_selections) <= 12.5) %>%
    mutate(county_name = paste(county_name,', ',state_name)) %>%
    ggplot(aes(x = year, y = log(total_plan_selections)))+
    geom_point(aes(color = total_plan_selections), size = 3)+
    geom_line(aes(x = year, group = county_name, y = log(total_plan_selections)))+
    geom_text(data = largest_counties_text, aes(x = year, label = county_name, y = log(total_plan_selections)),
              size = 3,  hjust = 0, check_overlap = TRUE)+
    ylim(8, 13)+
    scale_colour_gradientn(colours = terrain.colors(10))+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid  = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank()) + labs(title = "Largest Counties in Lake Michigan States Use of Cost Sharing Benifits",
                                                     subtitle = "This plot displays the number of consumers who were awarded and used cost sharing benifits for their\n health insurance plan",
                                                     color = "Plans Selected", y = element_blank(), x = 'Year', caption = 'Source: CMS' )

hh_income[hh_income=="."]<- 0

levels = colnames(hh_income)[5:12]

income_for_melt <- hh_income %>%
    setnames(c(4:11), c('level_1_perc', 'level_2_perc',
               'level_3_perc','level_4_perc','level_5_perc', 'level_6_perc',
               'level_7_perc', 'level_8_perc')) %>%
    mutate_at(.vars = c('level_1_perc', 'level_2_perc',
                     'level_3_perc','level_4_perc','level_5_perc', 'level_6_perc',
                     'level_7_perc', 'level_8_perc', 'total_plan_selections'), funs(as.integer)) #%>%
    group_by(state_name) %>%
    summarize_at(.vars = c('level_1_perc', 'level_2_perc',
                           'level_3_perc','level_4_perc','level_5_perc', 'level_6_perc',
                           'level_7_perc', 'level_8_perc', 'total_plan_selections'), .funs = funs(sum)) %>%
    mutate_at( .vars = c('level_1_perc', 'level_2_perc',
                         'level_3_perc','level_4_perc','level_5_perc', 'level_6_perc',
                         'level_7_perc', 'level_8_perc'), funs((./total_plan_selections)*100))
    

income_plot_ready <- income_for_melt %>%
    left_join(income_plot, income_plot, by = c("state_name", "county_name")) %>%
    melt( id.vars = c(1:14, 23,24)) %>%
    rename(income_range = variable) %>%
    group_by(state_name, income_range) %>%
    summarize_at(.vars = c('level_1_perc.x', 'level_2_perc.x',
                           'level_3_perc.x','level_4_perc.x','level_5_perc.x', 'level_6_perc.x',
                           'level_7_perc.x', 'level_8_perc.x', 'total_plan_selections.x'), .funs = funs(sum)) %>%
    mutate_at( .vars = c('level_1_perc.x', 'level_2_perc.x',
                         'level_3_perc.x','level_4_perc.x','level_5_perc.x', 'level_6_perc.x',
                         'level_7_perc.x', 'level_8_perc.x'), funs((./total_plan_selections.x)*100)) #%>%
    #group_by(state_name, income_range) %>%
    summarize_at(.vars = c('level_1_perc.x', 'level_2_perc.x',
                           'level_3_perc.x','level_4_perc.x','level_5_perc.x', 'level_6_perc.x',
                           'level_7_perc.x', 'level_8_perc.x', 'total_plan_selections.x'), .funs = funs(sum))

income_plot <- ggplot(position = identity)+
    geom_point(data = filter(income_plot_ready,income_range == 'level_1_perc.y'),
               aes(x = 'level_1_perc.y', y = level_1_perc.x, color = state_name),size = 3, alpha = .5, fill = NA, stat = 'identity')+
    geom_point(data = filter(income_plot_ready,income_range == 'level_2_perc.y'),
               aes(x = 'level_2_per.y', y = level_2_perc.x, color = state_name),size = 3, alpha = .5, fill = NA, stat = 'identity') +
    geom_point(data = filter(income_plot_ready, income_range == 'level_3_perc.y'),
               aes(x = 'level_3_per.y', y = level_3_perc.x, color = state_name),size = 3, alpha = .5, fill = NA, stat = 'identity')+
    geom_point(data = filter(income_plot_ready, income_range == 'level_4_perc.y'),
               aes(x = 'level_4_per.y', y = level_4_perc.x, color = state_name),size = 3, alpha = .5, fill = NA, stat = 'identity')+
    geom_point(data = filter(income_plot_ready, income_range == 'level_5_perc.y'),
               aes(x = 'level_5_per.y',y = level_5_perc.x, color = state_name),size = 3, alpha = .5, fill = NA, stat = 'identity')+
    geom_point(data = filter(income_plot_ready, income_range == 'level_6_perc.y'),
               aes(x = 'level_6_per.y', y = level_6_perc.x, color = state_name),size = 3, alpha = .5, fill = NA, stat = 'identity')+
    geom_point(data = filter(income_plot_ready, income_range == 'level_7_perc.y'),
               aes(x = 'level_7_per.y', y = level_7_perc.x, color = state_name),size = 3, alpha = .5, fill = NA, stat = 'identity')+
    geom_point(data = filter(income_plot_ready, income_range == 'level_8_perc.y'),
               aes(x = 'level_8_per.y',y = level_8_perc.x, color = state_name), size = 3, alpha = .5, fill = NA, stat = 'identity')+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


#plot3
income_plot2 <- ggplot(position = identity)+
    geom_bar(data = filter(income_plot_ready,income_range == 'level_1_perc.y'),
               aes(x = 'level_1_perc.y', y = level_1_perc.x, fill = state_name), position = 'dodge' , size = 3, alpha = .8, stat = 'identity')+
    geom_bar(data = filter(income_plot_ready,income_range == 'level_2_perc.y'),
               aes(x = 'level_2_perc.y', y = level_2_perc.x, fill = state_name), position = 'dodge', size = 3, alpha = .8,  stat = 'identity') +
    geom_bar(data = filter(income_plot_ready, income_range == 'level_3_perc.y'),
               aes(x = 'level_3_perc.y', y = level_3_perc.x, fill = state_name) , position = 'dodge',size = 3, alpha = .8,  stat = 'identity')+
    geom_bar(data = filter(income_plot_ready, income_range == 'level_4_perc.y'),
               aes(x = 'level_4_perc.y', y = level_4_perc.x, fill = state_name), position = 'dodge' ,size = 3, alpha = .8,  stat = 'identity')+
    geom_bar(data = filter(income_plot_ready, income_range == 'level_5_perc.y'),
               aes(x = 'level_5_per.y',y = level_5_perc.x, fill = state_name), position= 'dodge', size = 3, alpha = .8,  stat = 'identity')+
    geom_bar(data = filter(income_plot_ready, income_range == 'level_6_perc.y'),
               aes(x = 'level_6_per.y', y = level_6_perc.x, fill = state_name), position= 'dodge', size = 3, alpha = .8,  stat = 'identity')+
    geom_bar(data = filter(income_plot_ready, income_range == 'level_7_perc.y'),
               aes(x = 'level_7_per.y', y = level_7_perc.x, fill = state_name), position = 'dodge', size = 3, alpha = .8,  stat = 'identity')+
    geom_bar(data = filter(income_plot_ready, income_range == 'level_8_perc.y'),
               aes(x = 'level_8_per.y',y = level_8_perc.x
                   , fill = state_name), position = 'dodge',  size = 3, alpha = .8, stat = 'identity')+
    labs(title = "Percentage of Health Insurance Plans Selected based on Income Level",
         subtitle = "This plot displays the number of consumers who selected insurance plans through the marketplace and their income range",
         color = "State Abbreviation", x = 'Income Level', y = 'Percentage of Plans Bought By People at Income Level', caption = 'Source: CMS' )+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

 df <- data.frame(table(plan_attributes[c('statecode','metallevel')])) %>%
     mutate( pos = Freq - Freq/2)

 plan_attributes <- (left_join(plan_attributes, df[c('statecode','metallevel','pos','Freq')], by = c('statecode','metallevel')))
# 
# plot4  ALEX CAN HELP
plan_attributes %>%
    ggplot()+
    geom_bar(aes(x = 2016, y = Freq, fill = metallevel), stat = 'identity')+
    geom_text(aes( x= 2016, y = Freq-Freq/2 , label = metallevel), size= 2,hjust = 1, check_overlap = TRUE)+
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.text.x = element_blank())+
    coord_polar("y", start = 0)+
    facet_wrap(~statecode)
labs(fill = 'Metal Level', y = "Proportion of Plans Offered At Metal Level")+  scale_y_continuous(
   breaks= y.breaks,   # where to place the labels
   labels=plan_attributes)


# print(
# 
# two <- df %>%
#     ggplot()+
#         geom_text(aes(x= by, y = pos, label = ml), size= 2) +
#         coord_polar("y", start = 0)+
#         facet_grid(~sc)

#plans by county for each state

#Variable Name: CSRVariationType
#Variable Definition: Name of the cost sharing reduction options offered for a health
#insurance plan

#aptc

#plot 4 inspird by https://www.kaggle.com/ruonanding/rate-by-coverage-level-metallevel
individual_option <- rates %>%
    filter(age != 'Family Option' & individualrate < "9000") %>%
    select(c(1:16)) %>%
    mutate(planid = as.character(planid),
           planid = substr(planid, 1, 14))

benifits_option <- plan_attributes %>%
    mutate(planid = substr(planid, 1, 14),
           tehbinntier1individualmoop = gsub(",", "",tehbinntier1individualmoop),
           tehbinntier1individualmoop = gsub("\\$", "",tehbinntier1individualmoop),
           tehbinntier1individualmoop = as.numeric(tehbinntier1individualmoop),
           tehbinntier2individualmoop = gsub(",", "",tehbinntier2individualmoop),
           tehbinntier2individualmoop = gsub("\\$", "",tehbinntier2individualmoop),
           tehbinntier2individualmoop = as.numeric(tehbinntier2individualmoop),
           tehboutofnetindividualmoop = gsub(",", "",tehboutofnetindividualmoop),
           tehboutofnetindividualmoop = gsub("\\$", "",tehboutofnetindividualmoop),
           tehboutofnetindividualmoop = as.numeric(tehboutofnetindividualmoop)) %>%
    group_by(planid, metallevel) %>%
    summarise(innettier1moop=mean(tehbinntier1individualmoop),
              innettier2moop=mean(tehbinntier2individualmoop),
              outnetmoop=mean(tehboutofnetindividualmoop))

planrates <- inner_join(individual_option, benifits_option, by="planid")

planrates %>%
    select(age, metallevel, individualrate, statecode) %>%
    group_by(age, metallevel,statecode) %>%
    summarize(meanindrate= mean(individualrate),
              medianindrate = median(individualrate)) %>%
    arrange(desc(meanindrate))%>%
    ggplot( aes(x=age, y=meanindrate))+
    geom_point(stat='identity', size = 1.5, aes(color = factor(metallevel)))+
    ggtitle("Average Monthly Premium by Age")+
    labs(subtitle = 'By State in 2016', x="Age", y="Average Premium by Coverage", color = 'Metal Level',
         caption = 'Source: CMS')+
    facet_wrap(~statecode)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))

plan_attributes %>%
    filter(metallevel %in% c('Catostrophic','Gold','Silver','Platinum','Bronze'))%>%
    mutate(issueractuarialvalue = gsub("%", "", issueractuarialvalue),
           issueractuarialvalue = as.numeric(issueractuarialvalue)) %>%
    ggplot()+
    geom_count(aes( x = statecode, y = issueractuarialvalue, shape = plantype, color = metallevel), alpha = .5)+
    ggtitle('Actuarial Value of Plan Level and Type by State')+
    labs(subtitle = 'Shapes reveal the plan type, and color determines the metal level', y = 'Actuarial Value in Percentage', x = 'State', size = 'Plan Count', shape = 'Plan Type', color = 'Metal Level')
