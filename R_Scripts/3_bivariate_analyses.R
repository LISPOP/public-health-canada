source('R_Scripts/2_data_preparation.R')
theme_set(theme_minimal())
#### Demographic Summary ####
library(flextable)
library(gtsummary)
library(gt)

full %>% 
  select(Sample, `High Income`, Francophone1, Rural1, Degree1, Female1, Age) %>% 
tbl_summary(by=Sample, type=list(Age~"continuous"), statistic=list(Age ~ " {mean} ")) %>% 
as_gt() %>% 
  gtsave(filename=here("Tables", "cjph_demographics_comparison.html"))

#### Most Important Problem#### 
names(full)
full %>% 
  select(Sample, Obesity:Race_inequality) %>% 
 pivot_longer(-Sample) %>% 
  group_by(Sample, name, value) %>%
  as_factor() %>% 
  summarize(n=n()) %>% 
  mutate(Percent=(n/sum(n))*100) %>% 
  filter(value!="Not Selected") %>% 
  ggplot(., aes(y=fct_reorder(name, Percent), x=Percent, fill=Sample))+geom_col(position="dodge")+scale_fill_grey()+labs(title=str_wrap("Percent Selecting Issues as Most Important Public Health Problem After COVID-19", width=40), y="Issue")+guides(fill=guide_legend(reverse=T))

ggsave(here("Plots", "cjph_most_important_problem_group.png"), width=6, height=3)
####  Views on science in policy ####
lookfor(full, "policy")
ggplot(full, aes(x=as.numeric(Q30_1), fill=Sample,..scaled..))+
  geom_density(alpha=0.5)+
  labs(x="1=Policy Dictated By Best Scientific Evidence\n 7=Policy Determined By Many Factors Including Scientific Evidence")
ggsave(here("Plots", "science_policy_group.png"))

#### Views on CMOH ####
lookfor(full, "heard")
full$Q5
ggplot(full, aes(y=Sample, fill=as_factor(Q5)))+geom_bar(position="fill")+scale_fill_grey(name="Role of Chief Medical Office of Health")
ggsave(here("Plots", "cmoh_role_group.png"), width=8, height=2)

#### Show differences between genpop and public health on trade-offs ####
#Start with the dataframe
full %>% 
  #Pick the variables working with
  select(decline_economy:seniors_isolation, Sample) %>% 
  #pivot them longer, except for the Sample variable
pivot_longer(., cols=-Sample) %>% 
  #Convert to factor
  #as_factor() %>% 
  #form groups based on the variable Sample and the new variable name, which was created
  #In the pivotting process. 
  group_by(Sample, name) %>% 
  #Summarize each group by calculating the mean of value, which was also created 
  #in the pivotting process, and the sd, the sample size, and calculate the se for each
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  #an option here would be to pause this pipe, 
  # and replace the last pipe with a save out -> to some object 
  # like trade_off_Sample
  # But here we are just going right to graph. 
  ggplot(., aes(x=average, y=fct_recode(name, "Reduce Social Isoation"="social_isolation", "Reduce Seniors' Isolation"="seniors_isolation", "Keep Schools Open"="schools_open","Prevent Economic Decline"="decline_economy" ), col=Sample))+geom_point()+xlim(c(1,10))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(x="1=Stopping the spread of COVID-19 \n 10=Other considerations", y="Policy", title= str_wrap("In public health, it is often important to decide between accomplishing multiple outcomes, which outcome is more important to you?", width=60))
ggsave(here('Plots', 'trade_off_group.png'), width=6, height=2)

#### Difference Between Samples and Support For Measures
full %>% 
  select(starts_with('Q8_'), Sample) %>% 
  rename(`Mandatory Vaccines`=Q8_1, `Close Bars`=Q8_2, `Fine Non-Maskers`=Q8_3) %>% 
  pivot_longer(., cols=c(1,2,3)) %>% 
  #group_by(Sample) %>% 
  ggplot(., aes(x=name, y=value, fill=as_factor(Sample)))+geom_boxplot()+
  labs(title="Support For Interventions by Sample", x="Intervention")
ggsave(here('Plots', 'Interventions_by_sample.png'))
#### Correlation between Vaccine Severity and measures#### 

full %>% 
  select(starts_with('Q8_'), avgtotal_last7, Sample) %>% 
  pivot_longer(., cols=c(1,2,3)) %>% 
  # group_by(Sample, name) %>% 
  ggplot(., aes(x=avgtotal_last7, y=value, col=as_factor(Sample)))+geom_point()+facet_grid(~name)+geom_smooth(method="lm")

#### Trust ####
ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q32)))+geom_bar(position="fill")+labs(y='Sample')+scale_fill_grey(name="Politicians Are Ready to Lie")
ggsave(here("Plots", "trust_politicians_lie_group.png"), width=6, height=2)
ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q33)))+geom_bar(position="fill")+labs(y='Sample')+scale_fill_grey(name="Trust in Ottawa")
ggsave(here("Plots", "trust_ottawa_group.png"), width=6, height=2)
ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q34)))+geom_bar(position="fill")+labs(y='Sample')+scale_fill_grey(name="Trust in Government To Waste")
ggsave(here("Plots", "trust_taxes_group.png"), width=6, height=2)
ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q35)))+
  geom_bar(position="fill")+labs(y='Sample', title="")+scale_fill_grey(name="Government is...")
ggsave(here("Plots", "trust_interests_group.png"), width=10, height=2)
ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q36)))+
  geom_bar(position="fill")+labs(y='Sample', title="Trust in People by Sample")+scale_fill_grey(name="Trust in people")
ggsave(here("Plots", "trust_people_group.png"), width=6, height=2)


#Density plot for distribution of trust scores
ggplot(full, aes(x=trust_average,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(title="Distribution of Average Trust Scores by Sample")
ggsave(here("Plots", "trust_average_group_density.png"))

#### Ideology ####
full %>%
  select(Sample, Q51)%>%
  rename(Ideology=Q51)%>%
  group_by(Sample)%>%
  summarize(Average=mean(Ideology, na.rm=T), n=n(), sd=sd(Ideology, na.rm=T), se=sd/sqrt(n))%>%
  ggplot(., aes(x=Sample, y=Average))+geom_point()+geom_errorbar(aes(ymin=Average-(1.96*se), ymax=Average+1.96*se), width=0)+ylim(c(0,10))

ggsave(here("Plots", "cjph_ideology_group_density.png"))

#### Influence #### 
lookfor(full, "influence")
full %>% 
  select(Sample,contains("does")) %>% 
  pivot_longer(., cols=-Sample) %>% 
  mutate(name=str_replace_all(name, pattern="_|_|does", replace=" ")) %>% 
  mutate(name=str_trim(name)) %>% 
  group_by(Sample, name, value) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=n/sum(n)) %>% 
  filter(value==1) %>% 
  as_factor() %>% 
  ggplot(., aes(y=name, x=Percent, fill=Sample))+geom_col(position="dodge")+labs(y="Influence")+scale_fill_discrete(limits=rev)
ggsave(here("Plots", "influences_do_group.png"), width=6, height=2)


#### Influence #### 
lookfor(full, "influence")
library(ggsignif)
full %>% 
  select(Sample,contains("does"), contains("should")) %>% 
  pivot_longer(., cols=-Sample) %>% 
  mutate(Condition=case_when(
str_detect(name, pattern="does") ~ "Does Influence",
str_detect(name, pattern="should") ~"Should Influence"
  )) %>% 
  mutate(name=str_remove_all(name, "_does|_should")) %>% 
  mutate(name=str_replace_all(name, pattern="_", replace=" ")) %>% 
  group_by(Sample, Condition, name, value) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=(n/sum(n))*100, error=sqrt((Percent*(100-Percent))/n)) %>% 
  filter(value==1) %>% 
  as_factor() %>% 
  ungroup() %>% 
  ggplot(., aes(y=Sample, x=Percent, fill=Sample, alpha=fct_relevel(Condition, "Should Influence")))+geom_col(position="dodge")+labs(y="Influence")+facet_grid(fct_relevel(Sample, "Public Health")~fct_reorder(str_wrap(name, width=20), desc(Percent)), scales="free_y")+geom_errorbarh(aes(xmin=Percent-(1.96*error), xmax=Percent+(1.96*error)),height=0, position=position_dodge(0.9))+scale_fill_manual(values=c('black', 'lightgrey'), limits=rev, guide='none')+scale_alpha_manual(values=c(1,0.3, 0.3,1), limits=rev, name="Influence")+theme(strip.text.y=element_blank())+labs(y="Sample")

ggsave(here("Plots", "cjph_influences_do_should_policy_sample.png"), width=10, height=2)

full %>% 
  select(Sample,contains("should")) %>% 
  pivot_longer(., cols=-Sample) %>% 
mutate(name=str_replace_all(name, pattern="_|_|should", replace=" ")) %>% 
  mutate(name=str_trim(name)) %>% 
  group_by(Sample, name, value) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=n/sum(n), error=sqrt((Percent*(1-Percent))/n)) %>% 
  filter(value==1) %>% 
  as_factor() %>% 
  ggplot(., aes(y=name, x=Percent, fill=Sample, group=Sample))+geom_col(position="dodge")+labs(y="Influence")+geom_errorbar(aes(xmin=Percent-(1.96*error), xmax=Percent+(1.96*error)),height=0, position=position_dodge(0.9))

#ggsave(here("Plots", "influences_should_group.png"), width=6, height=2)
#### Science Literacy ####

#### Response to Local Conditions ####
#install.packages('corrr')
library(corrr)
# var_label(full$Q8_3)
# full %>% 
#   select(Q8_1_x:Q8_3_x, case_trend, Sample) %>% 
#   rename(`Mandatory Vaccines`=1, `Close Bars`=2, `No Mask Fines`=3) %>% 
# group_by(Sample) %>% 
#   nest() %>% 
#   mutate(
#     cor=map(data, correlate)
#   ) %>% 
#   unnest(cor)->out
# out
# out %>% 
#   pivot_longer(4:7) %>% 
#   filter(term=="case_trend") %>% 
#   filter(name!="case_trend") %>% 
#   ggplot(., aes(x=value, y=name, fill=Sample))+geom_col(position="dodge")+xlim(c(-0.2,0.2))+labs(title="Correlation Between Policy Preference\nand Local Conditions" , x="Pearson correlation Coefficient")


full %>% 
  select(Sample, Q8_1:Q8_3, case_trend) %>% 
  rename(., `Mandatory Vaccine`=2, `Close Down Bars and Restaurants`=3, `Fines For People Not Wearing Masks`=4) %>%
  pivot_longer(cols=2:4,  names_to="Policy", values_to="Support") %>% 
  ggplot(., aes(x=case_trend, y=Support, col=Sample))+facet_grid(~Policy)+geom_point(size=0.5)+geom_smooth(method="lm", se=F)+scale_color_grey()+geom_vline(xintercept=1, linetype=2)+labs(caption="< 1 Case trend falling, > 1 Case trend rising", x="Case Trend")


ggsave(here("Plots", "cjph_local_case_trend_preferences.png"), width=8, height=2)

lookfor(full, "economy")
names(full)

full %>% 
  select(decline_economy:seniors_isolation, Sample, case_trend) %>% 
  rename("Stop Economic Decline"=1, "Reduce Social Isolation"=2, "Keep Schools Open"=3,"Reduce Seniors Isolation"=4) %>% 
  pivot_longer(1:4,names_to=c("Goal"), values_to=c("Score")) %>% 
  ggplot(., aes(x=case_trend, y=Score, col=Sample))+geom_point(size=0.5)+geom_smooth(method="lm", se=F)+scale_color_grey()+facet_grid(~str_wrap(Goal, width=20))+labs(caption="< 1 Case trend falling, > 1 Case trend rising", x="Case Trend")
ggsave(here("Plots", "cjph_local_case_trend_trade_offs.png"), width=8, height=3)
#### Local Conditions Case Severity ####

full %>% 
  select(Sample, Q8_1:Q8_3, avgtotal_last7_pop_per_capita) %>% 
  rename(., `Mandatory Vaccine`=2, `Close Down Bars and Restaurants`=3, `Fines For People Not Wearing Masks`=4) %>%
  pivot_longer(cols=2:4,  names_to="Policy", values_to="Support") %>% 
  ggplot(., aes(x=avgtotal_last7_pop_per_capita, y=Support, col=Sample))+facet_grid(~str_wrap(Policy, width=20))+geom_point(size=0.5)+geom_smooth(method="lm", se=F)+scale_color_grey()+labs(x="Health Region Average 7 Day Covid19 Case Count Per Capita")
ggsave(here("Plots", "cjph_local_case_severity_per_capita_preferences.png"), width=8, height=3)
full %>% 
  select(decline_economy:seniors_isolation, Sample, avgtotal_last7_pop_per_capita) %>% 
  rename("Stop Economic Decline"=1, "Reduce Social Isolation"=2, "Keep Schools Open"=3,"Reduce Seniors Isolation"=4) %>% 
  pivot_longer(1:4,names_to=c("Goal"), values_to=c("Score")) %>% 
  ggplot(., aes(x=avgtotal_last7_pop_per_capita, y=Score, col=Sample))+geom_point(size=0.5)+geom_smooth(method="lm", se=F)+scale_color_grey()+facet_grid(~str_wrap(Goal, width=20))+labs(x="Health Region Average 7 Day Covid19 Case Count Per Capita")
ggsave(here("Plots", "cjph_local_case_severity_per_capita_trade_offs.png"), width=8, height=3)

full %>% 
  select(decline_economy:seniors_isolation, Sample, avgtotal_last7_pop_per_capita) %>% 
  rename("Stop Economic Decline"=1, "Reduce Social Isolation"=2, "Keep Schools Open"=3,"Reduce Seniors Isolation"=4) %>% 
  pivot_longer(1:4,names_to=c("Goal"), values_to=c("Score")) %>% 
  ggplot(., aes(x=avgtotal_last7_pop_per_capita, y=Score, col=Sample))+geom_point(size=0.5)+geom_smooth(method="loess", se=F)+scale_color_grey()+facet_grid(~Goal)

#### Local Conditions 
