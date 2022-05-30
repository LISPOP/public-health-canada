getwd()
source('1_data_import.R')

#Check for date variable
look_for(partial, 'date')
library(knitr)
#Calculate 
partial %>%
  rename(., CPHA=Q64_1, PHPC=Q64_2, ASPQ=Q64_3, HPC=Q64_4, Provincial=Q64_5, Other=Q64_6) %>% 
  pivot_longer(., cols=CPHA:Other, names_to="Org", values_to="Selected") %>% 
  select(START_DATE, Org, Selected) %>% 
  filter(Selected==1) %>% 
  group_by(day=as.Date(START_DATE), Org) %>% 
  summarize(n=n()) %>% 
  kable(format='html', caption="Responses per day  in the public health survey.") %>% 
  cat(., file=here("Tables", "responses_per_day.html"))


look_for(partial, "belong")
#### Print out plot ####
partial %>%
  rename(., CPHA=Q64_1, PHPC=Q64_2, ASPQ=Q64_3, HPC=Q64_4, Provincial=Q64_5, Other=Q64_6) %>% 
  pivot_longer(., cols=CPHA:Other, names_to="Org", values_to="Selected") %>% 
  select(START_DATE, Org, Selected) %>% 
  filter(Selected==1) %>% 
  group_by(day=as.Date(START_DATE), Org) %>% 
  summarize(n=n()) %>% 
  ggplot(., aes(x=day, y=n, fill=Org))+geom_col(position = position_dodge2(width = 0.9, preserve = "single"))+scale_x_date(breaks="days", date_labels="%b %d")
ggsave(here("Plots", "responses_per_group_by_day.png"), width=8, height=4)

#### Other categories ####
table(partial$Q64_6_SP)


#### Check Provincial Distribution ####
look_for(data=partial, 'province')
partial %>% 
  group_by(S1) %>% 
  summarize(n=n()) %>% 
  as_factor() %>% 
  kable(., format='html') %>% 
  cat(., file=here("Tables", "Provincial_distribution.html"))
