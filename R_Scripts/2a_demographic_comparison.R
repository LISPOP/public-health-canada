#This script compares our genpop sample with 

library(here)
source(here('R_Scripts', '2_data_preparation.R'))
library(gt)
#Set Theme for all plots
theme_set(theme_minimal(base_size = 20))

#### Evaluate Demographic Representativeness of Genpop Sample ####
#Read in demographic file produced from Statistics Canada Tables
census_labour_force_demographics<-read.csv(file=here('data', 'canada_demographics_comparison.csv'))
#Select first few columns
census_labour_force_demographics %>% 
  select(1:4)->census_labour_force_demographics

#Split above into census and labour force
census_demographics<-filter(census_labour_force_demographics, data_source=="Census")
labour_force_demographics<-filter(census_labour_force_demographics, data_source=="Labour Force")
# Filter the genpop
full %>% 
  filter(Sample=="General Population") ->genpop
#check
nrow(genpop)
#Filter the public health part of the sample
full %>% 
  filter(Sample=="Public Health")->ph
#Define a vector of  demographic variables for merging and selecting
demographic_variables<-c('Old1', 'High Income', 'Francophone1', 'Rural1', 'Degree1', 'Female1', 'Province')

#Select the demographic variables to be compared
genpop %>% 
  select(Old1, `High Income`, Francophone1, Rural1, Degree1, Female1, Province) %>%
  as_factor() %>% 
 #Pivot longer
  pivot_longer(cols=everything(),names_to = "variable", values_to='value') %>% 
  #Group by both values
  group_by(variable, value) %>% 
  #Count
  summarize(n=n()) %>% 
 # mutate(percent=(n/sum(n)*100)) %>% 
  #Filter out missing values
  filter(!is.na(value)) %>% 
  #ungroup
  ungroup() %>%
  #Define a new variable providing Sample as data source
  mutate(data_source=rep('Sample', nrow(.))) %>% 
  #filter(str_detect(value, "^No|^Not|^Over", negate=T)) %>% 
  #select(!(value:n)) 
  #Bind this to the dataset of sample_demographics
bind_rows(., census_demographics) ->census_sample_comparison

#For viewing
census_sample_comparison %>% 
  print(n=34) 
#Capitalize variable names
names(census_sample_comparison)<-str_to_title(names(census_sample_comparison))
#REname variable data_source
census_sample_comparison<-rename(census_sample_comparison, `Data Source`=Data_source)
names(census_sample_comparison)

#### This section gets the variable labels for each demographic variable and adds them to census_sample_comparison
#TAke the genpop data set and select just the demogtraphic variables
genpop %>% 
  select(all_of(demographic_variables)) %>% 
  #Get the variable labels
  map_df(., var_label) %>% 
  #Pivot_longer
  pivot_longer(., cols=everything()) %>% 
  #Join with  the object census_sample_comparison by name and Variable
  right_join(., census_sample_comparison, by=c("name"="Variable")) %>% 
  #Take out the 1 in the variable names
  mutate(name=str_remove_all(name, "1?")) %>% 
  #REname Variable and Label
  rename(., "Variable"=1, "Label"=2) %>% 
  #slice(1:8) %>% 
  #Form groups of the data source and each variable
  group_by(`Data Source`,Variable) %>% 
  #Calculate teh percent
  mutate(Percent=(N/sum(N))*100) %>% 
  #drop the N
  select(-N) %>% 
  #Pivot wider 
  pivot_wider(., names_from=c(`Data Source`), values_from=c(Percent)) %>% 
  #Ungroup
  ungroup() %>% 
  #Recode some values
  mutate(Variable=fct_recode(Variable, Age="Old")) %>% 
  #Turn into gt object
  gt(.) %>% 
  #reduce the number of decimals
  fmt_number(., columns = c("Sample", "Census"), decimals=0) %>% 
  #save out
  gtsave(filename=here("Tables", "table_A5_sample_genpop_census_genpop_comparison.html"))
#### Compare Public Health With Workforce ####
#Select the demographic variables to be compared
ph %>% 
  select(Old1, `High Income`, Francophone1, Rural1, Degree1, Female1, Province, age_2) %>%
  as_factor() %>% 
  #Pivot longer
  pivot_longer(cols=everything(),names_to = "variable", values_to='value') %>% 
  #Group by both values
  group_by(variable, value) %>% 
  #Count
  summarize(n=n())  %>% 
  # mutate(percent=(n/sum(n)*100)) %>% 
  #Filter out missing values
  filter(!is.na(value)) %>% 
  #ungroup
  ungroup() %>%
  #Define a new variable providing Sample as data source
  mutate(data_source=rep('Sample', nrow(.))) %>% 
  #filter(str_detect(value, "^No|^Not|^Over", negate=T)) %>% 
  #select(!(value:n)) 
  #Bind this to the dataset of sample_demographics
  bind_rows(., labour_force_demographics) ->workforce_sample_comparison

#For viewing
workforce_sample_comparison %>% 
  print(n=50) 
#Capitalize variable names
names(workforce_sample_comparison)<-str_to_title(names(workforce_sample_comparison))
#REname variable data_source
workforce_sample_comparison<-rename(workforce_sample_comparison, `Data Source`=Data_source)
names(workforce_sample_comparison)

#### This section gets the variable labels for each demographic variable and adds them to census_sample_comparison
#TAke the ph data set and select just the demogtraphic variables
demographic_variables<-c('age_2', 'High Income', 'Francophone1', 'Rural1', 'Degree1', 'Female1', 'Province')
names(workforce_sample_comparison)
ph %>% 
  select(all_of(demographic_variables)) %>% 
  #Get the variable labels
  map_df(., var_label) %>% 
  #Pivot_longer
  pivot_longer(., cols=everything()) %>% 
  #Join with  the object sample_demographics by name and Variable
  left_join(., workforce_sample_comparison, by=c("name"="Variable")) %>% 
  #Take out the 1 in the variable names
  mutate(name=str_remove_all(name, "1?")) %>% 
  #REname Variable and Label
  rename(., "Variable"=1, "Label"=2) %>% 
  #slice(1:8) %>% 
  #Form groups of the data source and each variable
  group_by(`Data Source`,Variable) %>% 
  #Calculate teh percent
  mutate(Percent=(N/sum(N))*100) %>% 
  #drop the N
  select(-N) %>% 
  #Pivot wider 
  pivot_wider(., names_from=c(`Data Source`), values_from=c(Percent)) %>% 
  #Ungroup
  ungroup() %>% 
  #Recode some values
  mutate(Variable=fct_recode(Variable, Age="age_2")) %>% 
  filter(!is.na(`Labour Force`)) %>% 
  #Turn into gt object
  gt(.) %>% 
  #reduce the number of decimals
  fmt_number(., columns = c("Sample", "Labour Force"), decimals=0) %>% 
  #save out
  gtsave(filename=here("Tables", "table_1_public_health_workforce_canada_workforce.html"))


#### Validating Against Vaccination Data ####
library(readxl)

#Read in the worksheet with Canada's vaccination data
vaccine_rates<-read.xlsx(here("data/vaccination_validation.xlsx"), sheet=1)
#Get the mean of the percent vaccinated
population_percent_vaccinated<-mean(vaccine_rates$Percent_Vaccinated)
#Store it in a vector with some missing values, for row binding
population_percent_vaccinated<-c(population_percent_vaccinated, NA, NA, NA)

full$Q23
#Filter just the genpop
full %>% 
  filter(Sample=="General Population") %>% 
  #Select the Q23
  select(Q23) %>%
  #Form groups
  group_by(Q23) %>%
  #Count
  count() %>% 
  #Ungroup
  ungroup() %>% 
  #Calculate the percentage
  mutate(percent=n/sum(n)*100) %>% 
  #Filter just the vaccinated
  filter(Q23==99) %>% 
  #Calculate p for estimating moe
  mutate(p=n/2005) %>% 
  #Now calculate moe sqrt((p*(1-p))/n)*1.96
 mutate(moe=(1.96*(sqrt((p*(1-p))/2005)))*100) %>% 
  #Select just the n, percent and moe
  select(n, percent, moe) %>% 
  #Mutate creating conf.upper and conf.lower
  mutate(conf.upper=percent+moe, conf.lower=percent-moe) %>% 
  #dump the n and store in out
  select(-n) ->sample_percent_vaccinated
sample_percent_vaccinated
#Make the names of the vaccinated vector to equal the names of out
names(population_percent_vaccinated)<-names(sample_percent_vaccinated)
#Bind the rows
sample_percent_vaccinated %>% 
  bind_rows(population_percent_vaccinated) %>% 
  #mutate and add a variable distinguishing between sample and population data
  mutate(Group=c('Sample', 'Population')) %>% 
  #Rename the variables for formatting
 select(Group=Group, Percent=percent, `Margin of Error`=moe, `95% Upper`=3, `95% Lower` =4) %>% 
  #Store as gt
  gt(.) %>% 
  #Make the format for decimanls
  fmt_number(., columns = 2:5, decimals=2) %>% 
  #Save out
  gtsave(., filename=here("Tables", "cjph_sample_population_vaccination.html"))
 
#### Validate Against Vaccine Intention

#Read in the worksheet with Canada's vaccination data
population_vaccine_intentions<-read.xlsx(here("data/vaccination_validation.xlsx"), sheet=2, detectDates=T)
population_vaccine_intentions


#Filter just the genpop
full %>% 
  filter(Sample=="General Population") %>% 
  #Select the Q23
  select(Q23) %>%
  #Form groups
  group_by(Q23) %>%
  #Count
  count() %>% 
  #Ungroup
  ungroup() %>% 
  #Calculate the percentage
  mutate(percent=n/sum(n)*100) %>% 
  #Filter just the vaccinated and store in sample_vaccine_intentions
  filter(Q23> 2 & Q23<5) ->sample_vaccine_intentions
sample_vaccine_intentions
#This code combines the data from the vaccine_intentions file with the vaccination intentions of our sample which is stored in sample_vaccine)_intentions
vaccine_intentions<-data.frame(
  #Make the date variable 
  date=c(population_vaccine_intentions$date, max(genpop$date)), 
  #Take the percentages of those intending to get vaccinated in other surveys and add up the sums of those intending to get vaccinated in our sample
  Percent=c(population_vaccine_intentions$Percent_intending_vaccination, sum(sample_vaccine_intentions$percent)),
  #Create the moe variable which repeats NA 6 timnes
  moe=rep(NA, 6),
  #Create a variable called Survey to distinguish where each data point comes from
  Survey=c(population_vaccine_intentions$Survey_company, "Study General Population"))
vaccine_intentions %>% 
  mutate(n=c(rep(NA, 5), 2005),
         #Calculate p for estimating moe
         p=Percent/100,
         #Now calculate moe sqrt((p*(1-p))/n)*1.96
         moe=(1.96*(sqrt((p*(1-p))/n)))*100)  ->vaccine_intentions
vaccine_intentions
str(vaccine_intentions)
vaccine_intentions %>% 
  ggplot(., aes(x=date, y=Percent, fill=fct_reorder(Survey, date)))+geom_col()+scale_fill_manual(name="Survey", values=c("grey60", "grey75", "grey90", "grey40", "grey95"))+geom_errorbar(width=0,aes(ymin=Percent-moe, ymax=Percent+moe))
#Save out
ggsave(filename=here("Plots", "cjph_sample_population_vaccination_intention_comparision.png"), width=6, height=2)

table(genpop$province)

#### Validating the Public Health Workforce SAmple
full %>% 
  filter(Sample!="General Population")->ph
### Distribution of Groups
ph %>%   
  select(., CPHA=Q64_1, PHPC=Q64_2, ASPQ=Q64_3, HPC=Q64_4, Provincial=Q64_5, CIPHI,`Other Association`=Other_Assoc) %>% 
  pivot_longer(., cols=CPHA:`Other Association`, names_to="Organization", values_to="Selected") %>% 
  filter(Selected==1) %>% 
  count(Organization) %>% 
  arrange(desc(n)) %>% 
  gt() %>% 
  grand_summary_rows(fns = list(id = "total", 
                                label = "Total", 
                                fn = "sum"), columns=n) %>% 
  gtsave(filename="Tables/table_a2_public_health_orgs.html")

# ph %>% 
#   filter(Q64_6==1) %>% 
#   select(Q64_1:Q64_6_SP, CIPHI,Other_Assoc) %>% 
#   View()
  

ph %>% 
  mutate(cpha=case_when(
    Q64_1==1~1,
    TRUE ~ 0
  ))->ph

# Read in Province Counts from CPHA
cpha<-read.csv(file=here("data","cpha_membership_file.csv"))

#Start with just the public health portion of the sample
ph %>% 
  filter(cpha==1) %>% 
  #Select the two variables Province and cpha members
  select(Province) %>% 
  #form groups of provinces
  group_by(Province) %>% 
  #And count; this will return the number of ph people in each province
  summarize(n=n()) %>% 
  #Add a variable called Sample; each value of this variable for this dataset will be the word "Sample"
  mutate(Sample=rep('Sample', nrow(.))) %>% 
  #Bind these rows to the cpha object that was imported just above
bind_rows(., cpha) ->cpha_sample_province
cpha_sample_province
cpha_sample_province %>% 
  #Now form groups by the variable Sample
group_by(Sample) %>% 
  #mutate and add a new variable that calculates the row percentage of each sample.
  mutate(pct=n/sum(n)) %>% 
  #Mutate Province to turn "Unknown" into a missing v alue
mutate(Province=car::Recode(Province, "'Unknown'=NA")) %>% 
  filter(str_detect(Province, "Nunavut|Northwest Territories|Yukon|International",negate=T)) %>% 
  mutate(Province=fct_reorder(Province, pct)) %>% 
  #Plot
  ggplot(., aes(y=Province, x=pct, fill=Sample))+
  geom_col(position="dodge")+
  geom_text(hjust=-0.5,aes(label=round(pct*100, 0)), position=position_dodge(width=0.9))+
  scale_fill_grey(guide=guide_legend(reverse=T))+
  labs(x="Percent")+
  theme(legend.position = "bottom")+
    scale_x_continuous(labels=scales::label_percent(accuracy=1), limits=c(0,0.45))
ggsave(here("Plots/cpjph_cpha_sample_population_comparison.png"))
#Recode the data into regions
cpha_sample_province %>% 
  filter(Province!="International"&Province!="Unknown") %>% 
  mutate(Region=car::Recode(Province, "'Alberta'='West' ; 
                            'British Columbia'='West'; 
                            'Manitoba'='West' ; 
                            'New Brunswick'='Atlantic';
                            'Newfoundland and Labrador' ='Atlantic' ;
                            'Northwest Territories' ='North' ; 
                            'Nova Scotia' ='Atlantic' ;
                            'Nunavut' ='North' ;
                            'Ontario'='Ontario' ;
                            'Prince Edward Island'='Atlantic';
                            'Yukon'='North';
                            'Saskatchewan'='West' ;
                            'Quebec'='Quebec'", 
                            levels=c("Atlantic", "Quebec", "Ontario", "West", "North"))) %>% 
#Now form groups by the variable Sample
  group_by(Sample, Region) %>% 
  summarize(n=sum(n)) %>% 
  #mutate and add a new variable that calculates the row percentage of each sample.
  mutate(pct=n/sum(n)*100) ->sample_cpha_region

sample_cpha_region %>% 
  rename(Percent=4) %>% 
  pivot_wider(., names_from=c("Sample"), names_sep=c(" "), 
              values_from = c("n", "Percent")) %>% 
#select(Region, contains("CPHA"), contains("Sample"))  %>% 
  arrange(., desc(`n CPHA`)) %>% 
gt(.) %>% 
  fmt_number(., columns=c(4,5),decimals=0) %>% 
  tab_source_note(source_note = "Actual CPHA membership and sample CPHA membership by region.") %>% 
  gtsave(., filename="table_A3.html", path=here("Tables"))

#Produce Table A1
   #Plot
sample_cpha_region
sample_cpha_region %>% 
  ggplot(., aes(y=fct_reorder(Region, pct), x=pct, fill=Sample))+
  geom_col(position="dodge")+
  geom_text(hjust=-0.5,aes(label=round(pct, 0)), position=position_dodge(width=0.9))+
  scale_fill_grey(guide=guide_legend(reverse=T))+
  labs(x="Percent", y="Region")+
  theme(legend.position = "bottom")
ggsave(here("Plots/cpjph_cpha_sample_population_comparison_region.png"), width=8, height=6)

#### Validate Against Vote ####

Party<-c("Liberal", "Conservative", "NDP", "BQ", "Green")
Vote<-c(33.1, 34.3,  16.0,7.6, 6.6)
election<-data.frame(Party, Vote, Source=rep("Election", length(Vote)))
election
full %>% 
  filter(Sample=="General Population") %>% 
count(Party=Vote) %>% 
  filter(!is.na(Party)) ->counts 
#Get Proportions
library(DescTools)
MultinomCI(counts$n) %>% 
  data.frame(Party=counts$Party) %>% 
  #Convert to Percentages
  mutate(across(1:3, function(x) x*100))->moe
moe$lwr.ci-moe$est
counts %>% 
  mutate(Vote=n/sum(n)*100) %>% 
  mutate(Source=rep("Sample", nrow(.))) %>% 
  select(-n) %>% 
 rbind(., election) %>% 
pivot_wider(., names_from=Source, values_from=Vote) %>% 
  left_join(., moe) %>% 
  select(-est) %>% 
  rename(Lower=4, Upper=5) %>% 
  select(Party, Sample, Lower, Upper, Election) %>% 
  arrange(desc(Sample)) %>% 
  gt() %>% 
  fmt_number(2:5, decimals=2) %>% 
  gtsave(., filename=here("Tables/table_a4_vote_comparison.html"))
  
#### Weighted Unweighted Comparisons ####
# Get population margins for raking 
#check the dataset that has the raw numbers for both the sample and census
census_sample_comparison
#Filter out the sample margins, just capture the population counts from the census
#Store in object population.margins
census_sample_comparison %>% 
  filter(`Data Source`!="Sample")->population.margins
#Factor VAlue
#This is necessary for splitting, below
population.margins$Value<-as.factor(population.margins$Value)
View(population.margins)

population.margins
#split into list items
population.margins<-split(population.margins[,-4],f=population.margins$Variable)

population.margins
#Change the names of each variable in each list item
population.margins<-map(population.margins, `[`, c('Value', 'N'))
population.margins
#Set the name of the income variable

names(population.margins[[1]])<-c('Degree1', "Freq")
names(population.margins[[2]])<-c('Female1', "Freq")
names(population.margins[[3]])<-c('Francophone1', "Freq")
names(population.margins[[4]])<-c('High_Income', "Freq")
names(population.margins[[5]])<-c('Old1', "Freq")
names(population.margins[[6]])<-c('Province1', "Freq")
names(population.margins[[7]])<-c('Rural1', "Freq")
population.margins
library(srvyr)
library(survey)
genpop$High_Income<-genpop$`High Income`
#Make survey design object out of genpop
genpop %>% filter(!is.na(rural)) %>% 
  as_survey_design(., ids=1)->des
#des<-as_survey_design(genpop, ids=1)


#Rake the survey design object according to the sample margins and the population margins
#Weighting according to degree status, language status, income, age

full.wtd<-rake(des, 
               sample.margins=list(~Degree1, 
                                   ~Francophone1, 
                                   ~High_Income, 
                                   ~Old1,~Rural1), 
               population.margins=list(population.margins[[1]], 
                                       population.margins[[3]], 
                                       population.margins[[4]], 
                                       population.margins[[5]],
                                       population.margins[[7]]))

# Reproduce Figure 1 with weighted data
# First make ideological variables
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


full.wtd %>% 
  select(Ideology, Q38_1_x:Q39_3_x) %>% 
  #Drop the egalitarian items that end in _y
  select(-ends_with("_y")) %>% 
  summarize(across(Ideology:Q39_3_x,
                   #apply the functions survey_mean to calculate a weighted mean
                   .fns=list(weighted=survey_mean, 
                             #This function calculates the average mean
                             unweighted=~unweighted(mean(.x)),
                             #Count and get the unweighted sd
                             n=~unweighted(n()), sd=~unweighted(sd(.x))))) %>% 
  # select(-contains("Ideology")) %>% 
  #Pivot everything down, Provide names and match
  pivot_longer(., cols=everything(), names_to=c('Question', ".value"), names_pattern="^(Ideology|Q\\d+_\\d_x)_(.*)") %>% 
  #Add unweighted standard error
  mutate(unweighted_se=sd/sqrt(n)) %>% 
  #Pivot unweighted and weighted down
  pivot_longer(., cols=c(weighted,unweighted)) %>% 
  mutate(se=case_when(
    name=="weighted"~ weighted_se,
    name=="unweighted"~ unweighted_se
  )) %>% 
  select(-weighted_se, -unweighted_se) %>% 
  #pivot_longer(., cols=c(weighted_se, unweighted_se), names_to=c("weight"), values_to=c("se")) 
  left_join(., ideology_variable_labels, by=c("Question"="Item")) ->weighted_worldview_scores_genpop
weighted_worldview_scores_genpop
weighted_worldview_scores_genpop %>% 
  ggplot(., aes(y=fct_reorder(label, value, .fun="max"), x=value, col=name))+
  geom_point()+
  xlim(0,1)+
  scale_color_grey()+
  geom_errorbar(width=0,aes(xmin=value-(1.96*se), xmax=value+(1.96*se)))+
  labs(title="Comparison of Weighted Versus Unweighted Averages", caption=str_wrap("Weights were generated using raking command in R, based using population margins of degree status, official language status, share of population earning greater than $100,000 and share of population over 65",80))


ph %>% 
  select(Ideology, Q38_1_x:Q39_3_x) %>% 
  #Drop the egalitarian items that end in _y
  select(-ends_with("_y")) %>% 
  summarize(across(Ideology:Q39_3_x,
                   #apply the functions survey_mean to calculate a weighted mean
                   .fns=list(#This function calculates the average mean
                     value=~mean(.x),
                     #Count and get the unweighted sd
                     n=~n(), sd=~sd(.x)))) %>% 
  pivot_longer(., cols=everything(), names_to=c('Question', ".value"), names_pattern="^(Ideology|Q\\d+_\\d_x)_(.*)") %>% 
  #Add unweighted standard error
  mutate(se=sd/sqrt(n))->ph_worldview_scores
names(ph_worldview_scores)  
names(weighted_worldview_scores_genpop)
ph_worldview_scores %>% 
  left_join(ideology_variable_labels, by=c("Question"="Item"))->ph_worldview_scores
ph_worldview_scores %>%   
  bind_rows(weighted_worldview_scores_genpop) %>%
  mutate(name=case_when(
    is.na(name)~"Sample",
    !is.na(name)~ str_to_title(name)
  )) %>% 
  ggplot(., aes(y=fct_reorder(label, value, .fun="max"), x=value, col=name))+
  scale_color_grey()+
  xlim(0,1)+
  theme(legend.position = "bottom")+
  geom_pointrange(aes(xmin=value-(1.96*se), xmax=value+(1.96*se)), position=position_dodge2(width=0.3))+
  labs(y="Item",
       caption=str_wrap("Weights were generated using raking command in R, using population margins from the 2016 census of share of population's degree status, official language status, rural residency, households with incomes greater than $100,000 and number of people over 65",80))
ggsave(filename=here("Plots/figure_a1_worldview_unweighted_weighted_comparison.png"), width=16, height=6)
#Captuire the variable labels from the Q8 questions
full %>% 
  select(Q8_1:Q8_3) %>% 
  rename(., `Mandatory Vaccine`=1, `Close Down Bars and Restaurants`=2, `Fines For People Not Wearing Masks`=3) %>% 
    map_dfr(., var_label) %>% 
  pivot_longer(., cols=everything()) %>% 
  rename(Label=1, Item=2) %>% 
  mutate(Question=str_extract(Item, "^Q8_[123]")) %>% 
  select(-Item)->Q8_var_labels
Q8_var_labels
#STart with the weighted data-set
full.wtd %>% 
  #Use summarize (across)
  summarize(across(Q8_1_x:Q8_3_x,
                   #apply the functions survey_mean to calculate a weighted mean
                   .fns=list(weighted=survey_mean, 
                             #This function calculates the average mean
                             unweighted=~unweighted(mean(.x)),
                             #Count and get the unweighted sd
                             n=~unweighted(n()), sd=~unweighted(sd(.x))))) %>% 
  #Pivot everything down, Provide names and match
  pivot_longer(., cols=everything(), names_to=c('Question', ".value"), names_pattern="^([^_]+_\\d+)_(.*)") %>% 
  #Add unweighted standard error
  mutate(unweighted_se=x_sd/sqrt(x_n)) %>% 
  #Pivot unweighted and weighted down
  pivot_longer(., cols=c(x_weighted,x_unweighted)) %>% 
  #Add weighted and unweighted se
  mutate(se=case_when(
    name=="x_weighted"~ x_weighted_se, 
    name=="x_unweighted"~unweighted_se)) %>% 
  left_join(., Q8_var_labels)->Q8_weighted_unweighted 
Q8_weighted_unweighted %>% 
  select(1, value, se, Label)
ph %>% 
  select(Q8_1_x:Q8_3_x, Health_Promotion) %>% 
  as_factor() %>% 
  pivot_longer(-Health_Promotion, values_to=c("score")) %>% 
  group_by(name, Health_Promotion) %>% 
  summarize(value=mean(score, na.rm=T), 
            n=n(), sd=sd(score, na.rm=T), se=sd/sqrt(n)) %>% 
  mutate(Question=str_remove_all(name, "_x")) %>% 
bind_cols(., Q8_weighted_unweighted$Label) %>% 
  rename(Label=8, Group=2)->ph_Q8_scores
ph_Q8_scores
Q8_weighted_unweighted %>% 
  mutate(Sample=rep("Genpop", nrow(.))) %>% 
  mutate(name=str_remove_all(name, "x_")) %>% 
  mutate(Group=str_c(Sample, name, sep=" ")) %>% 
  rename(sd=x_sd) ->Q8_weighted_unweighted 
Q8_weighted_unweighted %>% 
  select(-2,-3,-5, -6)->Q8_weighted_unweighted
Q8_weighted_unweighted
ph_Q8_scores %>% 
  mutate(Question=str_remove_all(name, "_x$")) %>% 
 # rename(Sample=Group) %>% 
  bind_rows(Q8_weighted_unweighted) %>% 
  #Gtraph
  ggplot(., aes(y=Label, x=value, col=Group))+
  geom_pointrange(size=1,aes(xmin=value-(se*1.96), xmax=value+(se*1.96)), 
                  position=position_dodge(width=0.25))+
  xlim(0,1)+
  scale_color_grey()+
  theme(legend.position="bottom")+
  labs(y="Policy", caption=str_wrap("Weights were generated using raking command in R, based using population margins of degree status, official language status, share of population earning greater than $100,000 and share of population over 65",80))
#Save out
ggsave(here("Plots", "figure_A2_Q8_weighted_unweighted_comparison.png"), width=14, height=6)


# Do regression of table 3 with weights

wtd_q8_1a<-svyglm(Q8_1_x~degree+rich+female+old2+rural, design=full.wtd)
wtd_q8_1b<-svyglm(Q8_1_x~egalitarianism+hierarchism+individualism, design=full.wtd)
wtd_q8_1c<-svyglm(Q8_1_x~degree+rich+female+old2+rural+
                   egalitarianism+hierarchism+individualism, design=full.wtd)
wtd_q8_2a<-svyglm(Q8_2_x~degree+rich+female+old2+rural, design=full.wtd)
wtd_q8_2b<-svyglm(Q8_2_x~egalitarianism+hierarchism+individualism, design=full.wtd)
wtd_q8_2c<-svyglm(Q8_2_x~degree+rich+female+old2+rural+
                    egalitarianism+hierarchism+individualism, design=full.wtd)
wtd_q8_3a<-svyglm(Q8_3_x~degree+rich+female+old2+rural, design=full.wtd)
wtd_q8_3b<-svyglm(Q8_3_x~egalitarianism+hierarchism+individualism, design=full.wtd)
wtd_q8_3c<-svyglm(Q8_3_x~degree+rich+female+old2+rural+
                    egalitarianism+hierarchism+individualism, design=full.wtd)
genpop_policy_models
map(list(wtd_q8_1a, wtd_q8_2a, wtd_q8_3a), tidy)
library(modelsummary)

modelsummary(wtd.q8.list, stars=T, 
             fmt=2,
             output="gt", 
             coef_omit=c(1:6), 
             gof_omit=c("BIC|AIC|RMSE|Log.Lik.| R2 Adj."),
             coef_rename=c('egalitarianism'='Egalitarianism', 'hierarchism'='Hierarchism','individualism'='Individualism'), 
             notes='Compare these coefficients with the coefficients in Table 3.')
