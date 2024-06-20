source('R_Scripts/2_data_preparation.R')
source('R_Scripts/2a_demographic_comparison.R')
theme_set(theme_minimal())
#Experimental Analysis
# Provide a variable describing experimental content
full %>% 
  mutate(Group1=case_when(
    GROUP==1~"Rash Increases",
    GROUP==2~"Rash Decreases",
    GROUP==3~"Got Worse",
    GROUP==4~"Got Better", 
    GROUP==5~ "More Outbreaks",
    GROUP==6~ "Less Outbreaks"
  ))->full
full$Group1<-factor(full$Group1, 
                    levels=c("Rash Increases", "Rash Decreases",
                             "Got Worse", "Got Better",
                        "More Outbreaks", "Less Outbreaks"))
full %>% 
  mutate(Study=case_when(
    GROUP<3 ~ "Study 1",
    GROUP==3|GROUP==4 ~ "Study 2",
    GROUP==5|GROUP==6 ~ "Study 3"
  ))->full





#Which groups were correct
#The correct response is obtained with the ratio
223/75
107/21
full$C12
full$C12
#Code 
val_labels(full$C12)
full %>% 
  mutate(correct=case_when(
    #When group = 1 and R chose patients who used the skin cream were more likely to get worse,
    # This was Incorrect
    GROUP==1 &C12==1 ~ 0,
    #When group = 1 and R chose patients who did not used the skin cream were more likely to get better, 
    # This was Correct
    GROUP==1 & C12==2 ~ 1,
    #When group = 2 and R patients who used the skin cream were more likely to get better,
    # This was Correct
    GROUP==2 &C12==1 ~ 1,
    #When group = 2 and R chose patients who used the skin cream were more likely to get worse, 
    # This was Inorrect
    GROUP==2 & C12==2 ~ 0,
    #When group =3 and R were prescribed pharmaceutical-grade heroin were more likely to GET BETTER 1
    #This was Incorrect
    GROUP==3 &C34==1 ~ 0,
    #When group =3 and R were prescribed pharmaceutical-grade heroin were more likely to GET WORSE 1
    #This was Ccorrect
    GROUP==3 &C34==2 ~ 1,
    #When group =4 and R were prescribed pharmaceutical-grade heroin were more likely to GET BETTER 1
    #This was Correct
    GROUP==4 &C34==1 ~ 1,
    #When group =4 and R were prescribed pharmaceutical-grade heroin were more likely to GET WORSE 2
    #This was Incorrect
    GROUP==4 &C34==2 ~ 0,
    #When Group = 5 and R picked 1 Long-term care centers that enforced strict social distancing measures were 
    # MORE LIKELY to experience an outbreak of COVID-19 than those who didn’t
    # This was correct
    GROUP==5 &C56==1 ~ 1,
    #When Group = 5 and R picked 1 Long-term care centers that enforced strict social distancing measures were  Less likely 
    # To experience an outbreak of COVID19 than those who didn't
    # This was correct
    GROUP==5 & C56==2 ~ 0,
    #When Group = 5 and R picked 1 Long-term care centers that enforced strict social distancing measures were  LESS likely 
    # To experience an outbreak of COVID19 than those who didn't
    # This was incorrect
    GROUP==6 & C56==1 ~ 0,
    GROUP==6 & C56==2 ~ 1
  ))->full

full$correct1<-car::Recode(full$correct, "'correct'=1; 
            'incorrect'=0; else=NA", as.factor=F)
# Hypothesis 1
# In rash condition, CRT score (numeracy) is positively related to the likelihood 
full %>% 
  filter(Sample=="General Population")  %>% 
  filter(Study=="Study 1") %>% 
glm(correct~mean_crt, data=., family="binomial") ->model1
full %>% 
  filter(Sample=="Public Health")  %>% 
  filter(Study=="Study 1") %>% 
  glm(correct~mean_crt, data=., family="binomial") ->model1a
modelsummary(model1, stars=T)

#Show results
full %>% 
  filter(Sample=="General Population") %>% 
  filter(Study=="Study 1") %>% 
  #group_by(mean_crt, correct) %>% 
  #summarize(n=n()) %>% 
  #mutate(pct=n/sum(n)) %>% 
 # filter(correct==1) %>% 
  ggplot(., aes(x=mean_crt, y=correct, col=Group1))+
  geom_point()+
  geom_jitter(width=0.1, height=0.1)+
  geom_smooth(method="loess", se=F)

# Check the slopes in the other two studies.
# The coefficients should be smaller
full %>% 
  filter(Sample=="General Population")  %>% 
  filter(Study=="Study 2") %>% 
  glm(correct~mean_crt, data=., family="binomial") ->model2
full %>% 
  filter(Sample=="Public Health")  %>% 
  filter(Study=="Study 2") %>% 
  glm(correct~mean_crt, data=., family="binomial") ->model2a
full %>% 
  filter(Sample=="General Population")  %>% 
  filter(Study=="Study 3") %>% 
  glm(correct~mean_crt, data=., family="binomial") ->model3
full %>% 
  filter(Sample=="Public Health")  %>% 
  filter(Study=="Study 3") %>% 
  glm(correct~mean_crt, data=., family="binomial") ->model3a
modelsummary(list("Rash General Population"=model1, "Rash Public Health"=model1a, 
"Prescription Heroin General Population"=model2, "Prescription Heroin Public Health"=model2a, 
"Lockdown General Population"=model3, "Lockdown Public Health"=model3a), stars=T)

#Now define ideological worldviews
full %>% 
  filter(Study=="Study 2") %>% 
  filter(Sample=="General Population") %>% 
glm(correct~mean_crt+harm1+mean_crt:harm1, family="binomial", data=.)->model4
full %>% 
  filter(Study=="Study 2") %>% 
  filter(Sample=="General Population") %>% 
  glm(correct~mean_crt+harm1+Group1+mean_crt:harm1+mean_crt:harm1:Group1, family="binomial", data=.)->model4_1

modelsummary(list(model4, model4_1), stars=T)

full %>% 
  filter(Study!="Study 3") %>% 
  filter(Sample=="General Population") %>% 
  ggplot(., aes(x=mean_crt, y=correct, linetype=Study, col=harm1))+
  geom_point()+geom_smooth(method="loess", se=F)+
  geom_jitter(width=0.1, height=0.1)+
  geom_text(data=full %>% select(mean_crt, Group1, Study, harm1) %>% distinct() %>% filter(mean_crt==1),aes(label=Group1, x=mean_crt))
full %>% 
  filter(Study=="Study 2") %>% 
  filter(Sample=="Public Health") %>% 
  glm(correct~mean_crt+harm1+Group1+mean_crt:harm1, family="binomial", data=.)->model4a
modelsummary(list("Harm Reduction General Population"=model4, "Harm Reduction Public Health"=model4a), stars=T)
summary(model4)
full$Group1
