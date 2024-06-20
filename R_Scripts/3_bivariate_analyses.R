source('R_Scripts/2_data_preparation.R')
theme_set(theme_minimal(base_size=24))
source('R_Scripts/2a_demographic_comparison.R')
#### Vote By Sample####
table(full$Vote, full$Sample)

chisq.test(table(full$Vote, full$Sample))

#### Demographic Summary ####
library(flextable)
library(gtsummary)
library(gt)

full %>% 
  select(Sample, `High Income`, Francophone1, Rural1, Degree1, Female1, Age) %>% 
tbl_summary(by=Sample, type=list(Age~"continuous"), statistic=list(Age ~ " {mean} ")) %>% 
as_gt() %>% 
  gtsave(filename=here("Tables", "cjph_demographics_comparison.html"))

ph %>% 
  count(Public_Health_Field) %>% 
  arrange(desc(n)) %>% 
rename(`Public Health Field`=1) %>% 
  gt() %>% 
  gtsave(filename=here("Tables/table_2_public_health_field.html"))

#### PUblic Health Field ####

#### Most Important Problem#### 
names(full)
library(ggsignif)
library(broom)
#Store the variables necessary for crosstabbing. 
full %>% 
  select(Sample, Obesity:Race_inequality) %>% 
 pivot_longer(-Sample) %>% 
  #group_by(Sample, name, value) %>%
  as_factor()->mip_genpop_public_health
#Conduct chi-squared test on each variable by Sample Size
mip_genpop_public_health %>% 
#nesting by variable 
    nest(-name) %>% 
  #onto each variable, we are creating a model, which is the results of a qhi-square test
  # of sample and value 
  mutate(model=map(data, ~chisq.test(.$Sample, .$value)), 
         #Tidy the results and storie in tidied
         tidied=map(model, tidy)) %>% 
  #Unnest tidied for viewing
  unnest(tidied)->mip_genpop_public_health_x2
mip_genpop_public_health_x2$model
#Adjust for multiple comparisons
mip_genpop_public_health_x2$p.value<-p.adjust(mip_genpop_public_health_x2$p.value, 
                                              method="bonferroni", n=nrow(mip_genpop_public_health_x2))
library(ggsignif)
mip_genpop_public_health %>% 
  mutate(name=fct_recode(name, 
                         "Racial Inequalities"="Race_inequality", 
                         "Vaccine Hesitancy"="Vacc_hesitancy")) %>% 
  group_by(name, Sample, value) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=(n/sum(n))*100) %>% 
  filter(value!="Not Selected") %>% 
  ggplot(., aes(y=fct_reorder(name, Percent), x=Percent, fill=Sample))+
    geom_col(position="dodge")+
    scale_fill_grey()+
    guides(fill=guide_legend(reverse=T))+  
  geom_text(aes(label=round(Percent,0)), position=position_dodge(width=0.9), hjust=-0.5)+xlim(c(0,60))+
  geom_signif(y_position=c(38,42), xmin=c(5.3,6.2),xmax=c(4.8,5.7),
              annotation=c("x2=16.0, p<0.001, df=1", "x2=13.5, p<0.001, df=1"), map_signif_level = T, angle=01, hjust=-0.05)+ labs( y="Issue")
ggsave(here("Plots", "cjph_most_important_problem_group.png"), width=12, height=6)

full %>% 
  select(Health_Promotion, Obesity:Race_inequality) %>% 
  pivot_longer(-Health_Promotion) %>% 
 # group_by(Public_Health_Field, name, value) %>%
  #filter(str_detect(Public_Health_Field, "Emergency|Epidemiology", negate=T)) %>% 
  as_factor() %>% 
filter(!is.na(Health_Promotion))->mip_public_health_field
#This code shows that there are comparisons (e.g. vaping, smoking) with low cell sizes. Consequently we use Fisher's exact test
# mip_public_health_field %>% 
#   group_by(name, Public_Health_Field, value) %>% 
#   summarize(n=n()) %>% 
#   View()
library(broom)
mip_public_health_field %>% 
  nest(-name) %>% 
  mutate(model=map(data, ~fisher.test(.$Health_Promotion, .$value)), 
         tidied=map(model, tidy)) %>% 
  unnest(tidied)->mip_public_health_field_x2
mip_public_health_field_x2$model
mip_public_health_field_x2
mip_public_health_field %>% 
  mutate(name=fct_recode(name, "Racial Inequalities"="Race_inequality", "Vaccine Hesitancy"="Vacc_hesitancy")) %>% 
  group_by(name, Health_Promotion, value) %>% 
  summarize(n=n()) %>% 
   mutate(Percent=(n/sum(n))*100) %>% 
   filter(value!="Not Selected") %>% 
  ggplot(., aes(y=fct_reorder(name, Percent), x=Percent, fill=Health_Promotion))+
  geom_col(position="dodge")+
  scale_fill_grey()+
  labs(y="Issue", fill="Field")+
  guides(fill=guide_legend(reverse=T))+
  geom_text(aes(label=round(Percent,0)), 
            position=position_dodge(width=0.9), hjust=-0.5)+
  geom_signif(
    y_position=c(25, 30, 55, 70 ), xmin=c(2.7, 3.7, 6.7, 7.8), xmax=c(3.2,4.2 , 7.2, 8.2), 
    annotations=c("x2=3.19, p=0.028, df=1", "x2=0.326, p=0.0255, df=1",
   "x2=1.95, p=0.037, df=1", "x2=1.85, p=0.0644, df=1"), angle=1, hjust=-0.025)+xlim(c(0,90))+
  theme(legend.position="bottom")
ggsave(here("Plots", "cjph_most_important_problem_field.png"), width=14, height=6)

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
zap_labels() %>% 
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
  select(starts_with('Q8_'), Sample2) %>% 
  zap_labels() %>% 
  rename(`Mandatory Vaccines`=Q8_1, `Close Bars`=Q8_2, `Fine Non-Maskers`=Q8_3) %>% 
  pivot_longer(., cols=c(1,2,3)) %>% 
  #group_by(Sample) %>% 
  ggplot(., aes(x=name, y=value, fill=Sample2))+geom_boxplot()+
  labs(title="Support For Interventions by Sample", x="Intervention")
names(full)
full %>% 
  select(Q8_1_x:Q8_3_x, Sample2) %>% 
  #Rescale Q8_1_x for the purposes of this graph only
  #mutate(across(Q8_1_x:Q8_3_x, function(x) skpersonal::revScale(x, reverse=T))) %>% 
  rename(`Mandatory Vaccines`=1, `Close Bars`=2, `Fine Non-Maskers`=3) %>% 
  pivot_longer(., cols=c(1,2,3)) %>% 
  group_by(Sample2, name) %>% 
  summarize(Average=mean(value, na.rm=T), n=n(), sd=sd(value, na.rm=T), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=Average, y=name, col=Sample2))+
  geom_pointrange(size=1,aes(xmin=Average-(se*1.96), xmax=Average+(se*1.96)), 
                  position=position_dodge(width=0.25))+
  scale_color_grey(start=0, end=0.9)+
  xlim(c(0,1))+
  theme(legend.position="bottom")+
  guides(col=guide_legend(ncol=2))+  
  labs(y="Policy", col="Sample", x="Average support for COVID19 contaiment and prevention\n0=Strongly Oppose, 1=Strongly Support")
ggsave(here('Plots', 'figure_3_Interventions_by_sample.png'), width=10, height=6)
ggsave(here('Plots', 'fig3.eps'), width=10, height=6)

full %>% 
  select(Q9_1_x:Q12_1_x, Sample2) %>% 
  mutate(across(Q9_1_x:Q12_1_x, revScale, reverse=T)) %>% 
  rename(`Stop Economic Decline`=1, `Reprieve From Isolation`=2, 
         `Keep Schools Open`=3, `Reprieve for Seniors`=4) %>% 
  pivot_longer(., cols=c(1,2,3,4)) %>% 
  group_by(Sample2, name) %>% 
  summarize(Average=mean(value, na.rm=T), n=n(), sd=sd(value, na.rm=T), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=Average, y=name, col=Sample2))+
  geom_pointrange(size=1,aes(xmin=Average-(se*1.96), xmax=Average+(se*1.96)), 
                  position=position_dodge(width=0.25))+
  scale_color_grey(start=0, end=0.9)+
  xlim(c(0,1))+
  theme(legend.position="bottom")+
  guides(col=guide_legend(ncol=2))+
  labs(y="Policy", 
       col="Sample",
       x="Average\n0 = Preference for other outcome\n 1=Preference for COVID prevention and containment")
ggsave(here("Plots/figure_4_trade_offs_sample.png"), width=12, height=6)
#### Correlation between Vaccine Severity and measures#### 

full %>% 
  select(starts_with('Q8_'), avgtotal_last7, Sample) %>% 
  zap_labels() %>% 
  pivot_longer(., cols=c(1,2,3)) %>% 
  # group_by(Sample, name) %>% 
  ggplot(., aes(x=avgtotal_last7, y=value, col=Sample))+
  geom_point()+facet_grid(~name)+geom_smooth(method="lm")

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

ggplot(full, aes(x=trust_average,fill=Sample,..scaled..))+
  geom_density(alpha=0.5)
  labs(title="Distribution of Average Trust Scores by Sample")
ggsave(here("Plots", "trust_average_group_density.png"))

#### Ideology ####
#Compare differences in self-reported ideology
full %>%
  select(Sample, Q51)%>%
  rename(Ideology=Q51)%>%
  group_by(Sample)%>%
  summarize(Average=mean(Ideology, na.rm=T), n=n(), sd=sd(Ideology, na.rm=T), se=sd/sqrt(n))%>%
  ggplot(., aes(x=Sample, y=Average))+geom_point()+geom_errorbar(aes(ymin=Average-(1.96*se), ymax=Average+1.96*se), width=0)+ylim(c(0,10))
#Save out the plot
ggsave(here("Plots", "cjph_ideology_sample_genpop.png"))

#Check variable labels for worldview questions
full %>% select(Q37_1:Q39_3) %>% 
  map(., var_label)
names(full)
#Ideological Variable Labels
full %>% select(Ideology, Q38_1_x:Q39_3_x) %>%
  select(-ends_with("_y")) %>% 
  names()->ideology_variable_labels
ideology_variable_labels<-data.frame(Item=ideology_variable_labels, label=c(
  "Self-Reported Ideology",
  "Inequalities between rich and poor should be reduced",
  "Discrimination against visible minorities still a serious problem",
  "More to do to reduce inequalities between men and women",
  "Free markets should provide goods more than government programs",
  "People who make a lot of money should not have to share with others",
  "Should not limit choices to protect people",
  "Private sector to create jobs",
  "Authorities imposing stricter punishments",
  "Respect for Authoritiy should be fundamental value",
  "First Nations have too many rights compared to regular citizens"
))
ideology_variable_labels
names(full)

full %>% 
  select(Sample, Ideology, Q38_1_x:Q39_3_x) %>% 
  #Drop the egalitarian items that end in _y
  select(-ends_with("_y"))  %>% 
  pivot_longer(., -Sample) %>% 
  nest(-name) %>% 
  mutate(model=map(data, function(x) lm(value~Sample, data=x)))->sample_ideology_models
library(modelsummary)
modelsummary(sample_ideology_models$model, stars=T)  
full %>% 
  select(Sample, Ideology, Q38_1_x:Q39_3_x) %>% 
  #Drop the egalitarian items that end in _y
  select(-ends_with("_y")) %>% 
    pivot_longer(., cols=-Sample) %>% 
  group_by(Sample, name) %>% 
  summarize(Average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(), se=sd/sqrt(n)) %>% 
  rename(Item=name) %>% 
  left_join(., ideology_variable_labels) ->public_health_ideology_worldviews_scores
#   arrange(name, Sample) %>% 
# group_by(name) %>% 
#   mutate(Difference=Average-lag(Average)) %>% 
#   ungroup() %>% 
public_health_ideology_worldviews_scores %>% 
  ggplot(., aes(x=Average, y=fct_reorder(label, Average, .fun="max"), col=Sample))+
  geom_pointrange(aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)),position=position_dodge2(width=0.4))+
  xlim(c(0,1))+
  scale_color_grey()+
  theme(legend.position = "bottom")+
 # geom_errorbar(width=0,aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)))+
  labs(x="0=Left-Wing position;1=Right-wing position", y="Survey Item")
ggsave(filename=here("Plots", "fig2.eps"), width=14, height=6)

# Check compare variances
public_health_ideology_worldviews_scores %>% 
  ggplot(., aes(x=sd, y=fct_reorder(label, sd), col=Sample))+
  geom_point(size=4)+xlim(c(0,1))+
  scale_color_grey()+
  theme(legend.position = "bottom")+
  #geom_errorbar(width=0,aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)))+
  labs(x="0=Left-Wing position;1=Right-wing position", y="Survey Item", title="SD on worldview items by sample")
#### Worldviews Within Public Health ####
look_for(full, "field")
lookfor(full, "position")

full %>% 
  select(Health_Promotion, Ideology, Q38_1_x:Q39_3_x) %>% 
  select(-ends_with("_y")) %>% 
  pivot_longer(-Health_Promotion, names_to=c("Item"), values_to=c("Score")) %>% 
 # filter(!is.na(Public_Health_Field)) %>% 
  as_factor() %>% 
  left_join(., ideology_variable_labels) %>% 
  group_by(Health_Promotion, label) %>% 
  summarize(Average=mean(Score, na.rm=T), n=n(), sd=sd(Score, na.rm=T), se=sd/sqrt(n))  %>% 
  filter(!is.na(Health_Promotion)) %>%
  ggplot(., aes(y=fct_reorder(label, Average, .desc=F), x=Average, col=Health_Promotion))+
  geom_pointrange(aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)),position=position_dodge2(width=0.4))+
  scale_color_grey(name="Field")+
  labs(y="Item")+
  theme(legend.position = "bottom")+
  xlim(c(0,1))
ggsave(here("Plots", "figure_2_cjph_public_health_field_ideology_worldviews.png"), width=14, height=6)
ggsave(here("Plots", "fig2.eps"), width=14, height=6)

#Compare Weighted and Unweighted Averages
full.wtd %>% 
  select(Ideology) %>% 
  summarize(Weighted_Ideology=survey_mean(Ideology), 
            Unweighted_Ideology=unweighted(mean(Ideology)),
            Unweighted_sd=unweighted(sd(Ideology)), Unweighted_n=unweighted(n()), Unweighted_Ideology_se=unweighted(Unweighted_sd/sqrt(Unweighted_n))) %>% 
  select(1:3,6)->out

out %>% 
  pivot_longer(., cols=everything(), names_to=c('Variable', ".value"), names_pattern = "^([^_]+)_(.*)") %>% 
  rename(Mean=Ideology, se=Ideology_se)->out
  out
full %>% 
  filter(Sample=="Public Health") %>% 
  summarize(Mean=mean(Ideology, na.rm=T), sd=sd(Ideology, na.rm=T), n=n(), se=sd/sqrt(n)) %>% 
  select(1,4) %>% 
  mutate(Variable="Public Health") %>% 
  bind_rows(., out)->ideology_mean
ideology_mean$Variable<-car::Recode(ideology_mean$Variable, "'Weighted'='Weighted General Population' ; 'Unweighted'='Unweighted General Population'")
  ggplot(ideology_mean, aes(x=Variable, y=Mean, col=Variable))+geom_point()+ylim(c(0,1))+labs(y="Average Ideology Score, 0 to 1", x="")+scale_color_manual(values=rep('black', 3), guide="none")
ggsave(filename=here("Plots", "weighted_unweighted_public_health_ideology_mean.png"))

ideology_model<-lm(Ideology~Sample+rich+degree+francophone+old+rural, data=full)
summary(ideology_model)
#### WorldViews ####

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
  ggplot(., aes(y=name, x=Percent, fill=Sample, group=Sample))+
  geom_col(position="dodge")+labs(y="Influence")+
  geom_errorbar(aes(xmin=Percent-(1.96*error), xmax=Percent+(1.96*error)), position=position_dodge(0.9))

#### Science Literacy ####
full$mean_know


  
  full %>% 
  select(Sample, mean_know) %>% 
    group_by(Sample) %>% 
    summarize(average=round(mean(mean_know),2)) %>% 
    mutate(x=c(0.5, 0.5), y=c(950, 950), average=paste("Mean score", average, sep=" "))->means
full %>% 
  select(Sample, mean_know) %>% 
  ggplot(., aes(x=mean_know))+geom_histogram(bins=5)+facet_grid(~Sample)+geom_text(aes(x=x, y=y, label=average), data=means) +labs(x="Average score, scaled 0 to 1", y="n")
  ggsave(here("Plots", "science_literacy_sample.png"), width=6, height=2)

  
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


# Examine case trend
  qplot(full$case_trend, geom="histogram")

#Examine case trend by day
  full %>% 
    group_by(date) %>% 
    summarize(avg=mean(case_trend, na.rm=T)) %>% 
    ggplot(., aes(x=date, y=avg))+geom_col()

full %>% 
  select(Sample, Q8_1:Q8_3, case_trend) %>% 
  rename(., `Mandatory Vaccine`=2, `Close Down Bars and Restaurants`=3, `Fines For People Not Wearing Masks`=4) %>%
  pivot_longer(cols=2:4,  names_to="Policy", values_to="Support") %>% 
  ggplot(., aes(x=case_trend, y=Support, col=Sample))+facet_grid(~Policy)+geom_point(size=0.5)+geom_smooth(method="loess", se=F)+scale_color_grey()+geom_vline(xintercept=1, linetype=2)+labs(caption="< 1 Case trend falling, > 1 Case trend rising", x="Case Trend")
ggsave(here("Plots", "cjph_local_case_trend_preferences.png"), width=8, height=2)

lookfor(full, "economy")
names(full)

full %>% 
  select(decline_economy:seniors_isolation, Sample, case_trend) %>% 
  zap_labels() %>% 
  rename("Stop Economic Decline"=1, "Reduce Social Isolation"=2, "Keep Schools Open"=3,"Reduce Seniors Isolation"=4) %>% 
  pivot_longer(1:4,names_to=c("Goal"), values_to=c("Score")) %>% 
  ggplot(., aes(x=case_trend, y=Score, col=Sample))+geom_point(size=0.5)+geom_smooth(method="lm", se=F)+scale_color_grey()+facet_grid(~str_wrap(Goal, width=20))+labs(caption="< 1 Case trend falling, > 1 Case trend rising", x="Case Trend")
ggsave(here("Plots", "cjph_local_case_trend_trade_offs.png"), width=8, height=3)
#### Local Conditions Case Severity ####
avg14_2sd
mean(full$avgtotal_last14_pop_per_capita, na.rm=T)-avg14_2sd
full %>% 
  select(Sample, Q8_1:Q8_3, avgtotal_last14_pop_per_capita) %>% 
  rename(., `Mandatory Vaccine`=2, `Close Down Bars and Restaurants`=3, `Fines For People Not Wearing Masks`=4) %>%
  pivot_longer(cols=2:4,  names_to="Policy", values_to="Support") %>% 
 # filter(., avgtotal_last14_pop_per_capita< (mean(avgtotal_last14_pop_per_capita, na.rm=T)+avg14_2sd) ) %>% 
  ggplot(., aes(x=avgtotal_last14_pop_per_capita, y=Support, col=Sample))+
  facet_grid(~str_wrap(Policy, width=20))+
  geom_point(size=0.5)+
  geom_smooth(method="lm", se=T)+
  scale_color_grey()+
  labs(x="Health Region Average 14 Day Covid19 Case Count Per Capita")
ggsave(here("Plots", "cjph_local_case_severity_per_capita_preferences.png"), width=8, height=3)

full %>% 
  select(decline_economy:seniors_isolation, Sample, avgtotal_last14_pop_per_capita) %>%
  zap_labels() %>% 
  rename("Stop Economic Decline"=1, "Reduce Social Isolation"=2, "Keep Schools Open"=3,"Reduce Seniors Isolation"=4) %>% 
  pivot_longer(1:4,names_to=c("Goal"), values_to=c("Score")) %>% 
  #filter(., avgtotal_last14_pop_per_capita<0.0003) %>% 
  ggplot(., aes(x=avgtotal_last14_pop_per_capita, y=Score, col=Sample))+
  geom_point(size=0.5)+geom_smooth(method="lm", se=T)+
  scale_color_grey()+facet_grid(~str_wrap(Goal, width=20))+labs(x="Health Region Average 7 Day Covid19 Case Count Per Capita")
ggsave(here("Plots", "cjph_local_case_severity_per_capita_trade_offs.png"), width=8, height=3)
#### Local Conditions 

#### Searching For Technocracy
lookfor(full, "scient")
lookfor(full, "evidence")
#Uncomment and install if necessary
#install.packages("plotrix")
library(plotrix)
library(knitr)
library(kableExtra)
t.test(Technocracy~Sample, data=full, var.equal=F)

full %>% 
  group_by(Sample) %>% 
  summarize(Average=mean(Technocracy, na.rm=T)) %>% 
  kable(., digits=2) %>% 
  save_kable(., file=here("Tables/cjph_technocracy.html"))
var_label(full$Q30_1)
val_labels(full$Q30_1)
full %>% 
  group_by(Sample2) %>% 
  summarize(Average=mean(Q30_1, na.rm=T)) %>% 
  arrange(., Average) %>% 
  rename(Sample=1) %>% 
gt() %>% 
  fmt_number(., columns=2, decimals=1) %>% 
  gtsave(., filename=here("Tables/cjph_technocracy.html"))
val_labels(full$Q30_1)
#Create 
full %>% 
  ggplot(., aes(x=trust_government, y=as.numeric(Q30_1), col=Sample))+
  geom_point()+geom_smooth(method="lm")+scale_color_grey()
full$Sample
table(full$Sample)
trust_model<-lm(Technocracy~Sample+trust_government, data=full)
trust_model2<-lm(Technocracy~Sample+trust_government:Sample, data=full)
trust_model3<-lm(Technocracy~Sample+trust_government+Ideology, data=full)
trust_model4<-lm(Technocracy~Sample+trust_government+Ideology:Sample, data=full)
trust_model

#trust_people_model<-lm(as.numeric(Q30_1)~trust_people, data=full)
# stargazer(list(trust_model,trust_model2, trust_model3, trust_model4), 
#           out=here("Tables", "cjph_trust_government_evidence.html"))

lookfor(full, "supervise")
full %>% 
  as_factor() %>% 
  summarize(mean(as.numeric(Q30_1)), std.error(as.numeric(Q30_1)))

full %>% 
  filter(Sample=="General Population") %>% 
  select(Q8_1:Q8_3, Ideology) %>% 
  cor(., use="everything") #Conservatives support stricter measures. 
# How is that possible. 
lookfor(full, "vote")
full %>% 
  filter(Sample=="General Population") %>% 
  select(Q57, Q8_1:Q8_3) %>% 
  pivot_longer(., cols=-Q57) %>% 
  group_by(name, Q57) %>% 
  summarize(mean=mean(value)) %>% 
  as_factor() %>% 
  arrange(name, desc(mean)) 


full %>% 
  filter(Sample=="General Population") %>% 
  select(Q57, Ideology) %>% 
  #pivot_longer(., cols=-Q57) %>% 
  group_by(Q57) %>% 
  summarize(mean=mean(Ideology)) %>% 
  as_factor() 

cor(genpop$Q8_1, genpop$Ideology, use="everything")
cor(genpop$Q8_2, genpop$Ideology, use="everything")
cor(genpop$Q8_3, genpop$Ideology, use="everything")
full %>% 
  ggplot(., aes(Q51, Q8_1))+geom_point()+geom_smooth()+geom_jitter()
full %>% 
  ggplot(., aes(Q51, Q8_2))+geom_point()+
  geom_smooth(method="lm")+
  geom_jitter()
full %>% 
  ggplot(., aes(Q51, Q8_3))+geom_point()+
  geom_smooth(method="lm")+
  geom_jitter()
full %>% 
  ggplot(., aes(Q51, Q9_1))+geom_point()+
  geom_smooth(method="lm")+
  geom_jitter()
full %>% 
  ggplot(., aes(Q51, Q10_1))+geom_point()+
  geom_smooth(method="lm")+
  geom_jitter()
full %>% 
  ggplot(., aes(Q51, Q11_1))+geom_point()+
  geom_smooth(method="lm")+
  geom_jitter()
full %>% 
  ggplot(., aes(Q51, Q12_1))+geom_point()+
  geom_smooth(method="lm")+
  geom_jitter()
full %>% 
  filter(Q57<5)  %>% 
  select(Q57, Q8_1:Q8_3, Q51) %>%  
  mutate(Vote=as_factor(Q57)) %>% 
  pivot_longer(Q8_1:Q8_3) %>% 
  select(-Q57) %>% 
  nest(-name) %>% 
  mutate(mod=map(data, function(x) lm(value~Vote, data=x)), 
         mod2=map(data, function(x) lm(value~Q51, data=x))) ->mods

mod3<-lm(Q51~as_factor(Q57), data=subset(full, Q57<5))

full %>% 
  mutate(Vote=as_factor(Q57)) %>% 
  select(Vote, Q8_1:Q8_3, Q51) %>% 
  pivot_longer(Q8_1:Q8_3) %>% 
  filter(., str_detect(Vote, "Conservative|Liberal|New Democratic")) %>% 
  ggplot(., aes(x=Q51, y=value, col=Vote))+geom_point()+geom_jitter()+
  geom_smooth(se=F, method="lm")+scale_color_manual(values=c("darkblue", "darkred", "orange"))+
  facet_wrap(~name)
  
