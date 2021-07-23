source('R_Scripts/2_data_preparation.R')
theme_set(theme_bw())

#### Most Important Problem#### 
names(full)
full %>% 
  select(Sample, Obesity:Race_inequality) %>% 
 pivot_longer(-Sample) %>% 
  group_by(Sample, name, value) %>%
  as_factor() %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>% 
  filter(value!="Not Selected") %>% 
  ggplot(., aes(y=name, x=pct, fill=Sample))+geom_col(position="dodge")+scale_fill_grey()+labs(title=str_wrap("Percent Selecting Issues as Most Important Public Health Problem After COVID-19", width=40))
ggsave(here("Plots", "most_important_problem_group.png"), width=6, height=3)
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
  ggplot(., aes(x=name, y=average, col=Sample))+geom_point()+ylim(c(1,10))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip()
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
ggplot(full, aes(x=Q51, fill=Sample))+geom_density(alpha=0.5)+labs(title="Self-Reported Ideology by Sample")
ggsave(here("Plots", "ideology_group_density.png"))

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
  ggplot(., aes(y=Sample, x=Percent, fill=Sample, alpha=Condition))+geom_col(position="dodge")+labs(y="Influence")+scale_fill_discrete(limits=rev)+facet_wrap(~str_wrap(name, width=20))+scale_alpha_discrete(limits=rev)+geom_errorbarh(aes(xmin=Percent-(1.96*error), xmax=Percent+(1.96*error)),height=0, position=position_dodge(0.9))
ggsave(here("Plots", "influences_do_should_policy_sample.png"), width=6, height=2)

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
var_label(full$Q8_3)
full %>% 
  select(Q8_1_x:Q8_3_x, case_trend, Sample) %>% 
  rename(`Mandatory Vaccines`=1, `Close Bars`=2, `No Mask Fines`=3) %>% 
group_by(Sample) %>% 
  nest() %>% 
  mutate(
    cor=map(data, correlate)
  ) %>% 
  unnest(cor)->out
out
out %>% 
  pivot_longer(4:7) %>% 
  filter(term=="case_trend") %>% 
  filter(name!="case_trend") %>% 
  ggplot(., aes(x=value, y=name, fill=Sample))+geom_col(position="dodge")+xlim(c(-0.2,0.2))+labs(title="Correlation Between Policy Preference\nand Local Conditions" , x="Pearson correlation Coefficient")

#caption="This shows the correlation between the trend in COVID cases in ther respondent's local health region and the respondent's support for various policy trade-offs. Note that the public health world's support for closing bars is linked to local case trends, but the general public's views are not. Note that the general public's desires to fine non-mask wearers is slightly correlated with local conditions, the public health workforce is moderately inversely correlated with local conditions."

library(corrr)
var_label(full$Q8_3)
names(full)
full %>% 
  select(Q8_1_x:Q8_3_x, avgtotal_last7_pop_per_capita, Sample) %>% 
  rename(`Mandatory Vaccines`=1, `Close Bars`=2, `No Mask Fines`=3) %>% 
  group_by(Sample) %>% 
  nest() %>% 
  mutate(
    cor=map(data, correlate)
  ) %>% 
  unnest(cor)->out
out$term
out %>% 
  pivot_longer(4:7) %>% 
  filter(term=="avgtotal_last7_pop_per_capita") %>% 
  filter(name!="avgtotal_last7_pop_per_capita") %>% 
  ggplot(., aes(x=value, y=name, fill=Sample))+geom_col(position="dodge")+xlim(c(-0.2,0.2))+labs(title="Correlation Between Policy Preference\nand Local Conditions" , x="Pearson correlation Coefficient")
