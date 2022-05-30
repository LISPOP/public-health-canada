#This script compares our genpop sample with 
library(gt)
library(here)
source(here('R_Scripts', '2_data_preparation.R'))

theme_set(theme_minimal())
#### Evaluate Demographic Representativeness of Genpop Sample ####
#Read in demographic file
census_demographics<-read.csv(file=here('data', 'canada_demographics_comparison.csv'))
census_demographics

# Filter the genpop
full %>% 
  filter(Sample=="General Population") ->genpop
#Select the demographic variables to be compared
genpop %>% 
  select(Old1, `High Income`, Francophone1, Rural1, Degree1, Female1, Province) %>% 
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

census_sample_comparison
#Capitalize variable names
names(census_sample_comparison)<-str_to_title(names(census_sample_comparison))
#REname variable data_source
census_sample_comparison<-rename(census_sample_comparison, `Data Source`=Data_source)
names(census_sample_comparison)
#Define a vector of  demographic variables for merging and selecting
demographic_variables<-c('Old1', 'High Income', 'Francophone1', 'Rural1', 'Degree1', 'Female1', 'Province')
#### This section gets the variable labels for each demographic variable and adds them to census_sample_comparison
#TAke the genpop data set and select just the demogtraphic variables
genpop %>% 
  select(all_of(demographic_variables)) %>% 
  #Get the variable labels
  map_df(., var_label) %>% 
  #Pivot_longer
  pivot_longer(., cols=everything()) %>% 
  #Join with  the object sample_demographics by name and Variable
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
  #Turn into gt object
  gt(.) %>% 
  #reduce the number of decimals
  fmt_number(., columns = c("Sample", "Census"), decimals=1) %>% 
  #save out
  gtsave(filename=here("Tables", "cjph_sample_census_comparison.html"))

#### Get population margins for raking 
#check the dataset that has the raw numbers for both the sample and census
census_sample_comparison
#Filter out the sample margins, just capture the population counts from the census
#Store in object population.margins
census_sample_comparison %>% 
  filter(`Data Source`!="Sample")->population.margins
#Factor VAlue
#This is necessary for splitting, below
population.margins$Value<-as.factor(population.margins$Value)
population.margins
#split into list items
population.margins<-split(population.margins[,-4],f=population.margins$Variable)

population.margins
#Change the names of each variable in each list item
population.margins<-map(population.margins, `[`, c('Value', 'N'))
#Set the name of the income variable

names(population.margins[[1]])<-c('Degree1', "Freq")
names(population.margins[[2]])<-c('Female1', "Freq")
names(population.margins[[3]])<-c('Francophone1', "Freq")
names(population.margins[[4]])<-c('High_Income', "Freq")
names(population.margins[[5]])<-c('Old1', "Freq")
names(population.margins[[6]])<-c('Rural1', "Freq")
population.margins
library(srvyr)
library(survey)
genpop$High_Income<-genpop$`High Income`
#Make survey design object out of genpop
des<-as_survey_design(genpop, ids=1)
population.margins
#Rake the survey design object according to the sample margins and the population margins
#Weighting according to degree status, language status, income, age
full.wtd<-rake(des, 
               sample.margins=list(~~Degree1, ~Francophone1, ~High_Income, ~Old1), 
               population.margins=list(population.margins[[1]], population.margins[[3]], population.margins[[4]], population.margins[[5]]))
#### Compare weighted preferences with unweighted
#Captuire the variable labels from the Q8 questions
full %>% 
  select(Q8_1:Q8_3) %>% 
  map_dfr(., var_label) %>% 
  pivot_longer(., cols=everything()) %>% 
  rename(Question=1, Label=2)->Q8_var_labels
#STart with the weighted data-set
full.wtd %>% 
  #Select the Q8 variables
  select(Q8_1:Q8_3) %>% 
  #Use summarize (across)
summarize(across(Q8_1:Q8_3,
                 #apply the functions survey_mean to calculate a weighted mean
                 .fns=list(weighted=survey_mean, 
                           #This function calculates the average mean
                           unweighted=~unweighted(mean(.x)),
                           #Count and get the unweighted sd
                           n=~unweighted(n()), sd=~unweighted(sd(.x))))) %>% 
  #Pivot everything down, Provide names and match
  pivot_longer(., cols=everything(), names_to=c('Question', ".value"), names_pattern="^([^_]+_\\d+)_(.*)") %>% 
  #Add unweighted standard error
  mutate(unweighted_se=sd/sqrt(n)) %>% 
  #Pivot unweighted and weighted down
pivot_longer(., cols=c(weighted,unweighted)) %>% 
  #Gtraph
ggplot(., aes(x=Question, y=value, col=name))+geom_point(size=3)+ylim(1,7)+labs(title="Comparison of Weighted Versus Unweighted Averages", caption=str_wrap("Weights were generated using raking command in R, based using population margins of degree status, official language status, share of population earning greater than $100,000 and share of population over 65",80))
#Save out
ggsave(here("Plots", "Q8_weighted_unweighted_comparison.png"))

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
look_for(ph, "health")
View(ph)
var_label(ph)
ph$GROUP
ph$Q64_1
ph %>% 
  mutate(cpha=case_when(
    Q64_1==1~1,
    TRUE ~ 0
  ))->ph

# Read in Province Counts from CPHA
cpha<-read.csv(file=here("data","cpha_membership_file.csv"))

#Start with just the public health portion of the sample
ph %>% 
  #Select the two variables PRovince and cpha members
  select(Province, cpha) %>% 
  #form groups of provinces
  group_by(Province) %>% 
  #And count; this will return the number of ph people in each province
  summarize(n=n()) %>% 
  #Add a variable called Sample; each value of this variable for this dataset will be the word "Sample"
  mutate(Sample=rep('Sample', nrow(.))) %>% 
  #Bind these rows to the cpha object that was imported just above
bind_rows(., cpha) %>%
  #Now form groups by the variable Sample
group_by(Sample) %>% 
  #mutate and add a new variable that calculates the row percentage of each sample.
  mutate(pct=n/sum(n)) %>% 
  #Mutate Province to turn "Unknown" into a missing v alue
mutate(Province=car::Recode(Province, "'Unknown'=NA")) %>% 
  #Plot
  ggplot(., aes(y=Province, x=pct, fill=Sample))+geom_col(position="dodge")+scale_fill_grey()
ggsave(here("Plots/cpjph_cpha_sample_population_comparison.png"))
