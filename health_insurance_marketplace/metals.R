
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
    select(age, metallevel, individualrate) %>%
    group_by(age, metallevel) %>%
    summarize(meanindrate= mean(individualrate),
              medianindrate = median(individualrate)) %>%
    arrange(desc(meanindrate))%>%
    ggplot( aes(x=age, y=meanindrate))+
    geom_point(stat='identity', size = 1.5, aes(color = factor(metallevel)))+
    facet_wrap(~)
    ggtitle("Average Monthly Premium by Age")+
    labs(x="Age", y="Average Premium by Coverage")

#graph7