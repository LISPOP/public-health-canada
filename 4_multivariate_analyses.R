library(here)
source(here("R_Scripts/3_bivariate_analyses.R"))
#### Ideological Differences 
# Test For Ideological Differences Controlling For Demographics
full
library(broom)

full %>% 
  select(Sample, Ideology, Q38_1_x:Q39_3_x, degree, rich, female, old2) %>% 
  pivot_longer(c(Ideology, Q38_1_x:Q39_3_x), names_to="Item", values_to="Score") %>% 
  nest(data=c(degree, rich, female, old2,  Score, Sample)) %>% 
  mutate(model1=map(data, ~lm(Score~ Sample, data=.x)), 
         model2=map(data, ~lm(Score~Sample+as_factor(degree)+as_factor(rich)+as_factor(female)+as_factor(old2), data=.x))) %>% 
  mutate(model1_tidied=map(model1, tidy), 
         model2_tidied=map(model2, tidy))  %>% 
  full_join(., ideology_variable_labels)->ideology_worldview_models

## Print Tables For Ideological Differences

library(ggeffects)
library(forcats)
summary(ideology_worldview_models$model2[[1]])
ideology_worldview_models
ideology_variable_labels


ideology_worldview_models$model2 %>% 
  set_names(., ideology_worldview_models$label) %>% 
  map_dfr(., ggemmeans, terms=c('Sample'), .id='Item') %>% 
 group_by(Item) %>% 
  mutate(Average=mean(predicted)) %>% 
ggplot(., aes(x=predicted, y=fct_reorder(Item, Average), col=x))+geom_point()+scale_color_grey(name="Sample")+xlim(c(0,1))+geom_errorbar(aes(xmin=conf.low, xmax=conf.high), width=0)+labs(y="Item", x="Estimated Average")
ggsave(filename=here("Plots", "cjph_ideology_worldview_estimated_means.png"), width=8, height=4)

#Create modelsummary
library(modelsummary)
ideology_model_table<-modelsummary(ideology_worldview_models$model2, output="gt", fmt=2, stars=c('*'=0.05, '**'=0.01, '***'=0.001), coef_omit="Intercept")

ideology_model_table %>% 
  tab_style(style=cell_fill(color='lightgrey'), locations=cells_body(rows=1:2)) %>% 
  cols_label(., `Model 1`="Ideology", 
             `Model 2`="Reduce Inequalities between rich and poor", 
             `Model 3`="Discrimination a problem", 
             `Model 4`="Reduce inequalities between men and women",
             `Model 5`="Free markets better than government programs", 
             `Model 6`="Opposed to rich sharing money", 
             `Model 7`="Opposed to limiting choices to protect people", 
             `Model 8`="Private Sector to create jobs", 
             `Model 9`="Authorities should impose stricter punishment", 
             `Model 10`="Respect for authority", 
             `Model 11`="First Nations too many rights") %>% 
  gtsave(., filename=here("Tables", "cjph_ideology_worldviews_regressions.html"))


#### Are there differences in policy preferences controlling for demograhpics
#First set of DVS is Q8
library(broom)
full %>% 
  select(Q8_1:Q8_3) %>% 
  var_label()
full %>% 
  select(Sample, Q8_1_x:Q8_3_x, degree,  rich, female, old2, Ideology) %>% 
  pivot_longer(Q8_1_x:Q8_3_x, names_to="Policy", values_to="Score") %>% 
  nest(data=c(degree, rich, female, old2, Ideology, Score, Sample)) %>% 
  mutate(model1=map(data, ~lm(Score~ Sample, data=.x)), 
         model2=map(data, ~lm(Score~Sample+degree+rich+female+old2, data=.x)), 
         model3=map(data, ~lm(Score~Sample+degree+rich+female+old2+Ideology, data=.x))) %>% 
mutate(tidied1=map(model1, tidy),
       tidied2=map(model2, tidy),
       tidied3=map(model3, tidy)) ->models1


library(stargazer)
#This function prints the model
# 
# stargazer(
#   #This weird bitty interleaves each element of each list
#   #I am trying to print the models for each DV in successive order
#   c(c(rbind(models1$model1, models1$model2, models1$model3))), 
#   type="html", 
#   #Specify file name
#   out=here("Tables/cjph_policy_preferences_genpop_public_health_demographics.html"),
#   #Define column labels
#   column.labels=rep(c("Mandatory Vaccines", "Fines For Wearing Masks", "Close Bars and Restaurants Down"),3), 
#   #Show that each column label should stretch over three columns
#   column.separate=c(3,3,3), 
#   digits=2, digits.extra=2
#   )

models1_table<-modelsummary(c(c(rbind(models1$model1, models1$model2, models1$model3))), output="gt", fmt=2, stars=c("*"=0.05, "**"=0.01, "***"=0.001), coef_omit="Intercept")
models1_tab %>% 
  tab_style(style=cell_fill(color='lightgrey'), locations=cells_body(rows=1:2)) %>% 
  tab_spanner(., label="Mandatory Vaccine", columns=2:4) %>% 
tab_spanner(., label="Fines for Wearing Masks", columns=5:7) %>% 
  tab_spanner(., label="Close Bars and Restaurants", columns=8:10) %>% 
  gtsave(., filename=here("Tables", "cjph_policy_preferences_ideology_demographics.html"))

#Second set of DVs is decline_economy to seniors isolation
library(broom)

full %>% 
  select(Sample, Q9_1_x:Q12_1_x, degree,  rich, female, old2, Ideology) %>% 
  pivot_longer(Q9_1_x:Q12_1_x, names_to="Trade_Off", values_to="Score") %>% 
  nest(data=c(degree, rich, female, old2, Score, Sample, Ideology)) %>% 
  mutate(model1=map(data, ~lm(Score~ Sample, data=.x)), 
         model2=map(data, ~lm(Score~Sample+degree+rich+female+old2, data=.x)), 
         model3=map(data, ~lm(Score~Sample+degree+rich+female+old2+Ideology, data=.x))) %>% 
  mutate(tidied1=map(model1, tidy),
         tidied2=map(model2, tidy)) ->trade_off_models

trade_off_models_tab<-modelsummary(c(c(rbind(trade_off_models$model1, trade_off_models$model2, trade_off_models$model3))), output="gt",stars=c("*"=0.05, "**"=0.01, "***"=0.001), fmt=2, coef_omit="Intercept")
                                     
trade_off_models_tab %>% 
  tab_style(style=cell_fill(color='lightgrey'), locations=cells_body(rows=1:2)) %>% 
  tab_spanner(., label="Stop Economic Decline", columns=c(2:4)) %>% 
  tab_spanner(., label="Reprieve from Social Isolation", columns=c(5:7)) %>% 
  tab_spanner(., label="Keep schools open", columns=c(8:10)) %>% 
  tab_spanner(., label="Reprieve from Social Isolation for Seniors", columns=c(11:13)) %>% 
  gtsave(., file=here("Tables", "cjph_trade_offs_ideology_demographics.html"))

####
# Model odds of selecting raical inequalities

#Create variable they selected racial inequality
look_for(full, "vacc")
#### Trust Versus Scientific Literacy


salience_mod<-glm(Q1_8~Sample, data=full, family="binomial")
salience_mod2<-glm(Q1_8~ Sample+degree+rich+female+old2, data=full, family="binomial")
salience_mod3<-glm(Q1_8~ Sample+degree+rich+female+old2+Ideology, data=full, family="binomial")
salience_mod4<-glm(Q1_2~Sample, data=full, family="binomial")
salience_mod5<-glm(Q1_2~ Sample+degree+rich+female+old2, data=full, family="binomial")
salience_mod6<-glm(Q1_2~ Sample+degree+rich+female+old2+Ideology, data=full, family="binomial")
summary(salience_mod)

salience_models_tab<-modelsummary(list(salience_mod, salience_mod2, salience_mod3, salience_mod4, salience_mod5, salience_mod6), output='gt', fmt=2, stars=c("*"=0.05, "**"=0.01, "***"=0.001), coef_omit="Intercept")
salience_models_tab %>% 
  tab_style(style=cell_fill(color='lightgrey'), locations=cells_body(rows=1:2)) %>% 
  tab_spanner(., label="Racial Inequalities", columns=c(2:4)) %>% 
  tab_spanner(., label="Vaccine Hesitancy", columns=c(5:7)) %>% 
  gtsave(., filename=here("Tables", "cjph_salience_demographics_ideology.html"))

####


qplot(full$avgtotal_last7_pop_per_capita, data=full)
summary(full$cases)
summary(full$avgtotal_last7_pop_per_capita)
cor(full$cases, full$avgtotal_last7, use="complete.obs")
full$trust_average
full %>% 
  select(Sample, avgtotal_last7_pop_per_capita, matches('Q8_[0-9]_x'), mean_know, trust_average, Ideology) %>% 
  pivot_longer(cols=3:5, names_to=c("Question"), values_to=c("Score")) %>% 
  nest(data=c(2:5, 7)) %>% 
  mutate(model1=map(data,~lm(Score~mean_know, data=.x)), 
         model2=map(data,~lm(Score~trust_average, data=.x)), 
         model3=map(data, ~lm(Score~Ideology, data=.x)),
         model4=map(data, ~lm(Score~avgtotal_last7_pop_per_capita, data=.x)))->evidence_models

evidence_models %>%  
filter(Sample=="Public Health")->public_health_evidence_models
evidence_models %>%  
  filter(Sample!="Public Health")->genpop_evidence_models

c(public_health_evidence_models$model1,public_health_evidence_models$model2, public_health_evidence_models$model3, public_health_evidence_models$model4) %>% 
  setNames(rep(c("Vaccines", "Close Bars", "Fines for Masks"), 4)) %>% 
modelsummary(., stars=T, output="gt") 

c(genpop_evidence_models$model1,genpop_evidence_models$model2, genpop_evidence_models$model3, genpop_evidence_models$model4) %>% 
  setNames(rep(c("Vaccines", "Close Bars", "Fines for Masks"), 4)) %>% 
  modelsummary(., stars=T, output="gt") 

full %>% 
  select(Sample, avgtotal_last7_pop_per_capita, Q9_1_x:Q12_1_x, mean_know, trust_average, Ideology) %>% 
  pivot_longer(cols=3:6, names_to=c("Question"), values_to=c("Score")) %>% 
  nest(data=c(2:5, 7)) %>% 
  mutate(model1=map(data,~lm(Score~mean_know, data=.x)), 
         model2=map(data,~lm(Score~trust_average, data=.x)), 
         model3=map(data, ~lm(Score~Ideology, data=.x)),
         model4=map(data, ~lm(Score~avgtotal_last7_pop_per_capita, data=.x)))->evidence_models_trades
evidence_models_trades %>%  
  filter(Sample=="Public Health")->public_health_evidence_trades_models
evidence_models_trades %>%  
  filter(Sample!="Public Health")->genpop_evidence_trades_models
genpop_evidence_trades_models
c(public_health_evidence_trades_models$model1,public_health_evidence_trades_models$model2, public_health_evidence_trades_models$model3, public_health_evidence_trades_models$model4) %>% 
  setNames(rep(c("Stop Decline Economy", "Repreive Isolation", "Schools Open", "REpreive Isolation Seniors"), 4)) %>% 
  modelsummary(., stars=T, output="gt") %>% 
  gtsave(., filename=here("Tables", "cjph_public_health_trade_offs_evidence_trust_knowledge_ideology.html"))

c(genpop_evidence_trades_models$model1,genpop_evidence_trades_models$model2, genpop_evidence_trades_models$model3, genpop_evidence_trades_models$model4) %>% 
  setNames(rep(c("Stop Decline Economy", "Repreive Isolation", "Schools Open", "REpreive Isolation Seniors"), 4)) %>% 
  modelsummary(., stars=T, output="gt") %>% 
  gtsave(., filename=here("Tables", "cjph_genpop_trade_offs_evidence_trust_knowledge_ideology.html"))

#### Looking fo