source('R_Scripts/2_data_preparation.R')
theme_set(theme_bw())

###Q1 Most Important Problem#### 
names(full)
full %>% 
  select(Sample, Obesity:Race_inequality) %>% 
 pivot_longer(-Sample) %>% 
  group_by(Sample, name, value) %>%
  as_factor() %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>% 
  filter(value!="Not Selected") %>% 
  ggplot(., aes(y=name, x=pct, fill=Sample))+ geom_col(position="dodge")+theme(legend.position = "bottom")+
  labs(y="", x="Percentage", title=str_wrap("Issue as Most Important Public Health Problem After COVID-19", width=60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "most_important_problem_group.png"), width=6, height=3)

##### Q2 Federal government managing COVID####
lookfor(full, "federal government")
full$Q2_1
ggplot(full, aes(x=Q2_1,fill=Sample,..scaled..))+geom_density(alpha=0.5)+ 
  labs(y="", x="1=Very Poorly\n 7=Very Well", title= str_wrap ("Satisfaction with Federal Government Preventing the Spread of COVID-19", width = 60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "Federal_Government_Satisfaction_COVID_Spread.png"))

full$Q2_2
ggplot(full, aes(x=Q2_2,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(y="", x="1=Very Poorly\n 7=Very Well", title=str_wrap("Satisfaction with Federal Government Ensuring Speedy Access to Vaccines", width =60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "Federal_Government_Satisfaction_Access_Vaccines.png"))

full$Q2_3
ggplot(full, aes(x=Q2_3,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(y="", x="1=Very Poorly\n 7=Very Well", title= str_wrap("Satisfaction with Federal Government Managing Economic Disruptions", width=60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "Federal_Government_Satisfaction_Economic_Disruptions.png"))

##### Q2 Significance Tests ####
wilcox.test(Q2_1 ~ Sample, data=full)
wilcox.test(Q2_2 ~ Sample, data=full)
wilcox.test(Q2_3 ~ Sample, data=full)

#### Q3-Q4 Influence Combined #### 
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
  ggplot(., aes(y=Sample, x=Percent, fill=Sample, alpha=Condition))+geom_col(position="dodge")+
  labs(y="Influence")+scale_fill_discrete()+facet_wrap(~str_wrap(name, width=20))+
  scale_alpha_discrete(range =c(0.4, 1), limits=rev)+theme(legend.position = "bottom")+
  geom_errorbarh(aes(xmin=Percent-(1.96*error), xmax=Percent+(1.96*error)),height=0, position=position_dodge(0.9))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "influences_do_should_policy_sample.png"))

####Q3 Influences that should affecting government decision-making ####

###NEED TO FIX CONFIDENCE INTERVALS###

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
  ggplot(., aes(y=name, x=Percent*100, fill=Sample, group=Sample))+
  geom_col(position="dodge")+theme(legend.position = "bottom")+labs(y="", x="Percentage", title= str_wrap("Which of the following influences do you think should affect government decision-making in Canada about COVID-19", width=60))+geom_errorbarh(aes(xmin=Percent-(1.96*error), xmax=Percent+(1.96*error)),height=0, position=position_dodge(0.9))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "influences_should_group.png"))

###sIGNIFICANCE TESTING - Work in progress


#### Q4 Influences that are affecting government decision-making #### 

###NEED TO FIX CONFIDENCE INTERVALS###

lookfor(full, "influence")
full %>% 
  select(Sample,contains("does")) %>% 
  pivot_longer(., cols=-Sample) %>% 
  mutate(name=str_replace_all(name, pattern="_|_|does", replace=" ")) %>% 
  mutate(name=str_trim(name)) %>% 
  group_by(Sample, name, value) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=n/sum(n), error=sqrt((Percent*(1-Percent/n)))) %>% 
  filter(value==1) %>% 
  as_factor() %>% 
  ggplot(., aes(y=name, x=Percent*100, fill=Sample, group=Sample))+
  geom_col(position="dodge")+theme(legend.position = "bottom")+labs(y="", x="Percentage", title= str_wrap("Which of the following influences do you think do affect government decision-making in Canada about COVID-19", width=60))+geom_errorbarh(aes(xmin=Percent-(1.96*error), xmax=Percent+(1.96*error)),height=0, position=position_dodge(0.9))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "influences_do_group.png"))

###sIGNIFICANCE TESTING - Work in progress


#### Q5 Views on CMOH ####
####NEED TO MUTATE X AXIS *100 FOR %
lookfor(full, "heard")
full$Q5
ggplot(full, aes(y=Sample, fill=as_factor(Q5)))+theme(legend.position = "bottom")+geom_bar(position="fill")+scale_fill_brewer(palette= "Set1", name="Role of Chief Medical Office of Health")+
  labs(y="", x="", title= str_wrap("In your view, which of the following best describes the role of your provincial Chief Medical Officer", width=60))+
  guides(fill=guide_legend(title="", ncol=2, byrow=TRUE))
ggsave(here("Plots", "cmoh_role_group.png"))

#### Q6 Trust in the Following ####
lookfor(full, "trust")
full %>% 
  #Pick the variables working with
  select(Q6_1:Q6_9, Sample) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-Sample) %>% 
  group_by(Sample, name) %>% 
  #Summarize each group by calculating the mean of value, which was also created 
  #in the pivotting process, and the sd, the sample size, and calculate the se for each
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=name, y=average, col=Sample))+geom_point()+ylim(c(1,5))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=Cannot be trusted at all\n 5=Can be trusted a lot", title=str_wrap("How much do you trust each of the following?", width=60), x="")+
  scale_x_discrete(labels = c("Chief Medical Officer of Health\nin your Province", "Chief Public Health Officer of Canada", "World Health Organization", "United States Centers of Disease\nControl and Prevention", "Chinese Centre for Disease Control\n and Prevention", "Provincial Government", "Federal Government"))
ggsave(here('Plots', 'trust_groups.png'))

####SIGNIFICANCE TEST
wilcox.test(Q6_1 ~ Sample, data=full)
wilcox.test(Q6_2 ~ Sample, data=full)
wilcox.test(Q6_3 ~ Sample, data=full)
wilcox.test(Q6_4 ~ Sample, data=full)
wilcox.test(Q6_5 ~ Sample, data=full)
wilcox.test(Q6_8 ~ Sample, data=full)
wilcox.test(Q6_9 ~ Sample, data=full)


#####  EXTRA Density plot for distribution of trust scores ####
ggplot(full, aes(x=trust_average,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(y="", x=" Average Trust Score", title="Distribution of Average Trust Scores by Sample")+
  guides(fill=guide_legend(title=""))+
ggsave(here("Plots", "trust_average_group_density.png"))


#### Q7 Rate Groups in terms of how well they prevent spread of COVID####
lookfor(full, "groups")
full %>% 
  #Pick the variables working with
  select(Q7_1:Q7_8, Sample) %>% 
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
  ggplot(., aes(x=name, y=average, col=Sample))+geom_point()+ylim(c(1,5))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=Very poorly \n 5=Very well", x="", title= str_wrap("How would you rate the following groups in preventing the spread of COVID-19?", width=60)) +
  scale_x_discrete(labels = c("Young People", "Your Neighbours", "Senior Citizens", "Federal Government", "Provincial Government", "Municipal Government", "School Boards", "Public Health Professionals"))
ggsave(here('Plots', 'groups_preventing_spread.png'))

####Significance Testing
wilcox.test(Q7_1 ~ Sample, data=full)
wilcox.test(Q7_2 ~ Sample, data=full)
wilcox.test(Q7_3 ~ Sample, data=full)
wilcox.test(Q7_4 ~ Sample, data=full)
wilcox.test(Q7_5 ~ Sample, data=full)
wilcox.test(Q7_6 ~ Sample, data=full)
wilcox.test(Q7_7 ~ Sample, data=full)
wilcox.test(Q7_8 ~ Sample, data=full)


#### Q8 Difference Between Samples and Support For Measures #####
full %>% 
  select(starts_with('Q8_'), Sample) %>% 
  rename(`Mandatory Vaccines`=Q8_1, `Close Bars`=Q8_2, `Fine Non-Maskers`=Q8_3) %>% 
  pivot_longer(., cols=c(1,2,3)) %>% 
  #group_by(Sample) %>% 
  ggplot(., aes(x=name, y=value, fill=as_factor(Sample)))+geom_boxplot()+
  labs(title="Support For Interventions", x="Intervention", y="")+
  guides(fill=guide_legend(title=""))
ggsave(here('Plots', 'Interventions_by_sample.png'))

lookfor(full, "support or oppose")
full$Q8_1
ggplot(full, aes(x=Q8_1,fill=Sample,..scaled..))+geom_density(alpha=0.5)+ 
  labs(y="", x="1=Strongly oppose\n 7=Strongly support", title= str_wrap ("It should be mandatory for all residents in Canada to get a vaccine that has been approved by Health Canada", width = 60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "Support_mandatory_vaccines.png"))

full$Q8_2
ggplot(full, aes(x=Q8_2,fill=Sample,..scaled..))+geom_density(alpha=0.5)+ 
  labs(y="", x="1=Strongly oppose\n 7=Strongly support", title= str_wrap ("Bars and restaurants should be closed down", width = 60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "Support_bars_closed.png"))

full$Q8_3
ggplot(full, aes(x=Q8_3,fill=Sample,..scaled..))+geom_density(alpha=0.5)+ 
  labs(y="", x="1=Strongly oppose\n 7=Strongly support", title= str_wrap ("People not wearing masks in public, indoor places should be fined", width = 60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "Support_mask_fines.png"))

##SIGNIFICANCE TEST
wilcox.test(Q8_1 ~ Sample, data=full)
wilcox.test(Q8_2 ~ Sample, data=full)
wilcox.test(Q8_3 ~ Sample, data=full)


#### Correlation between Vaccine Severity and measures - NOT WORKING #### 

full %>% 
  select(starts_with('Q8_'), avgtotal_last7, Sample) %>% 
  pivot_longer(., cols=c(1,2,3)) %>% 
  # group_by(Sample, name) %>% 
  ggplot(., aes(x=avgtotal_last7, y=value, col=as_factor(Sample)))+geom_point()+facet_grid(~name)+geom_smooth(method="lm")

#### Q9 Show differences between genpop and public health on trade-offs ####

###

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
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip()+
  labs(y="1=Stopping the spread of COVID-19 \n 10=Other considerations", x="", title= str_wrap("In public health, it is often important to decide between accomplishing multiple outcomes, which outcome is more important to you ", width=60)) +
ggsave(here('Plots', 'trade_off_group.png'))

lookfor(full, "decline")
wilcox.test(social_isolation ~ Sample, data=full)
wilcox.test(seniors_isolation ~ Sample, data=full)
wilcox.test(schools_open ~ Sample, data=full)
wilcox.test(decline_economy ~ Sample, data=full)

####Q23 Vaccine Hesitancy ####
lookfor(full, "eligible")

##Need to rescale this... Not entirely sure what to do with 98 and 99 ##

####Q24 Increasing Federal Power ####
###NEED TO RESCALE Y AXIS *100 TO BE PERCENTAGE ####

lookfor(full, "greater powers")
ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q24_1)))+geom_bar(position="fill")+labs(x="", y="")+scale_fill_manual(values = c("dark red", "tomato", "royal blue", "dark blue") ,(name=""))+theme(legend.position = "bottom")+labs (title= str_wrap("Would you disagree or agree to giving the federal government greater power to do the following even if it would mean acting in an area of provincial responsibility: Monitoring the spread of diseases", width = 60 )) +
  guides(fill = guide_legend(reverse=TRUE))
ggsave(here("Plots", "Greater_Federal_Powers_Monitoring.png"))

ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q24_2)))+geom_bar(position="fill")+labs(x="", y="")+scale_fill_manual(values = c("dark red", "tomato", "royal blue", "dark blue") ,(name=""))+theme(legend.position = "bottom")+labs (title= str_wrap("Would you disagree or agree to giving the federal government greater power to do the following even if it would mean acting in an area of provincial responsibility: Emergency response", width = 60 )) +
  guides(fill = guide_legend(reverse=TRUE))
ggsave(here("Plots", "Greater_Federal_Powers_Emergency.png"))

ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q24_3)))+geom_bar(position="fill")+labs(x="", y="")+scale_fill_manual(values = c("dark red", "tomato", "royal blue", "dark blue") ,(name=""))+theme(legend.position = "bottom")+labs (title= str_wrap("Would you disagree or agree to giving the federal government greater power to do the following even if it would mean acting in an area of provincial responsibility: Control over vaccine supply chain", width = 60 )) +
  guides(fill = guide_legend(reverse=TRUE))
ggsave(here("Plots", "Greater_Federal_Powers_Vaccines.png"))

ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q24_4)))+geom_bar(position="fill")+labs(x="", y="")+scale_fill_manual(values = c("dark red", "tomato", "royal blue", "dark blue") ,(name=""))+theme(legend.position = "bottom")+labs (title= str_wrap("Would you disagree or agree to giving the federal government greater power to do the following even if it would mean acting in an area of provincial responsibility: Population mental health", width = 60 )) +
  guides(fill = guide_legend(reverse=TRUE))
ggsave(here("Plots", "Greater_Federal_Powers_Mental_Health.png"))



####Q25 Government Priority to ensure speedy recovery####
###NEED TO FIX####

lookfor(full, "recovery")
full$Q25
ggplot(full, aes(x=as.numeric(Q25), fill=Sample))+
  geom_bar(aes(y = (..count../sum(..count..))), position = "dodge") +
  labs(x="")+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "Government_Prioritize_Speedy_Recovery"))


#### Q30 Views on science in policy ####
lookfor(full, "policy")
ggplot(full, aes(x=as.numeric(Q30_1), fill=Sample,..scaled..))+
  geom_density(alpha=0.5)+
  labs(y="", x="1=Policy Dictated By Best Scientific Evidence\n 7=Policy Determined By Many Factors Including Scientific Evidence", title= str_wrap("How important do you think scientific evidence is in making public policy?", width=60))+
  guides(fill=guide_legend(title=""))
gsave(here("Plots", "science_policy_group.png"))

####Q31 Experience providing scientific evidence to policymakers####

#### Q32-36 Trust ####
###NEED TO RESCALE Y Axis *100 to be a percentage ###
ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q32)))+geom_bar(position="fill")+
  labs(y="", x="")+scale_fill_manual(values = c("dark red", "tomato", "royal blue", "dark blue") ,(name=""))+theme(legend.position = "bottom")+labs (title= str_wrap("Politicians are ready to lie to get elected", width = 60 )) +guides(fill = guide_legend(reverse=TRUE))
ggsave(here("Plots", "trust_politicians_lie_group.png"))
ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q33)))+geom_bar(position="fill")+
  labs(y="", x="")+scale_fill_manual(values = c("dark red", "tomato", "gray", "royal blue", "dark blue") ,(name=""))+theme(legend.position = "bottom")+labs (title= str_wrap("How much of the time do you think you can trust the government in Ottawa to do what is right", width = 60 ))+guides(fill = guide_legend(reverse=TRUE))
ggsave(here("Plots", "trust_ottawa_group.png"))
ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q34)))+geom_bar(position="fill")+
  labs(y="", x="")+scale_fill_manual(values = c("tomato", "gray", "royal blue") ,(name=""))+theme(legend.position = "bottom")+labs (title= str_wrap("Do you think that people in government waste:", width = 60 ))+guides(fill = guide_legend(reverse=TRUE))
ggsave(here("Plots", "trust_taxes_group.png"))
ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q35)))+geom_bar(position="fill")+
  labs(y="", x="")+scale_fill_manual(values = c("tomato", "royal blue") ,(name=""))+theme(legend.position = "bottom")+labs (title= str_wrap("Would you say the government is...", width = 60 ))+guides(fill = guide_legend(reverse=TRUE))
ggsave(here("Plots", "trust_interests_group.png"))
ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q36)))+geom_bar(position="fill")+
  labs(y="", x="")+scale_fill_manual(values = c("royal blue", "tomato") ,(name=""))+theme(legend.position = "bottom")+labs (title= str_wrap("Generally speaking, would you say that most people can be trusted", width = 60 ))+guides(fill = guide_legend(reverse=TRUE))
ggsave(here("Plots", "trust_people_group.png"))

####Q37 Individualism #####
lookfor(full, "society")
ggplot(full, aes(x=Q37_1,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(x="1=Strongly Disagree\n 7=Strongly Agree", title= str_wrap("Government programs - not free markets - are the best way to supply people with the things they need", width=60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "Government_Programs_Needed_Things.png"))

ggplot(full, aes(x=Q37_2,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(x="1=Strongly Disagree\n 7=Strongly Agree", title= str_wrap("People who make lots of money have a moral obligation to share it with others", width=60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "Rich_obligation_share.png"))

ggplot(full, aes(x=Q37_3,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(x="1=Strongly Disagree\n 7=Strongly Agree", title= str_wrap("Sometimes government need to limit people's choices (e.g. ban smoking or require seatbelts) to keep them from hurting themselves", width=60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "Government_limiting_freedom.png"))

ggplot(full, aes(x=Q37_4,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(x="1=Strongly Disagree\n 7=Strongly Agree", title= str_wrap("The government should leave it entirely to the private sector to create jobs", width=60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "Private_sector_jobs.png"))

####Q38 Equality & Discrimination####

lookfor(full, "society")
ggplot(full, aes(x=Q38_1,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(y="", x="1=Strongly Disagree\n 7=Strongly Agree", title= str_wrap("We need to dramatically reduce inequalities between the rich and the poor", width=60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "dramatically_reduce_inequality.png"))

ggplot(full, aes(x=Q38_2,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(y="", x="1=Strongly Disagree\n 7=Strongly Agree", title= str_wrap("Discrimination against visible minorities is still a very serious problem in our society", width=60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "discrimination_against_minorities.png"))

ggplot(full, aes(x=Q38_3,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(y="", x="1=Strongly Disagree\n 7=Strongly Agree", title= str_wrap("We need to do more to reduce inequalities between men and women", width=60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "Reduce_gender_inequality.png"))


####Q39 Institutions & Law####
lookfor(full, "society")
ggplot(full, aes(x=Q39_1,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(y="", x="1=Strongly Disagree\n 7=Strongly Agree", title= str_wrap("Authorities should impose stricter punishment on those who break the law", width=60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "Impose_stricter_punishment.png"))

ggplot(full, aes(x=Q39_2,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(y="", x="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("Respect for authority should be a fundamental value in our society", width=60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "respect_authority_fundamental.png"))

ggplot(full, aes(x=Q39_3,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(y="", x="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("Compared to regular citizens, First Nations have too many special rights", width=60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "First_nations_rights.png"))


####Q40 Free Will####
ggplot(full, aes(x=Q40_1,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(y="", x="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("Most of the important things that take place in life happen by random chance", width=60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "Important_things_random.png"))
ggplot(full, aes(x=Q40_2,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(y="", x="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("The course of our lives is largely determined by forces beyond our control", width=60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "Lives_beyond_control.png"))
ggplot(full, aes(x=Q40_3,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(y="", x="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("The future is too uncertain for people to make ANY long-term plans", width=60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "Future_uncertain.png"))


####Q51 Ideology ####
ggplot(full, aes(x=Q51, fill=Sample))+geom_density(alpha=0.5)+labs(y="", x= "0=Liberal\n10=Conservative", title="Self-Reported Ideology by Sample")+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "ideology_group_density.png"))


