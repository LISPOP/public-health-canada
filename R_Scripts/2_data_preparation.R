#This script will contain code that prepares the dataset for analysis
####package management
library(skpersonal)
####Data Import####
library(here)
#This runs the data import file, so running this file from top to bottom will import the data as well. 
source(here('R_Scripts', '1_data_import.R'))
#### Some basics####
#Recode a sample variable with public health and general popuilation respondents
full$Sample<-car::Recode(as.numeric(full$phase),"2='Public Health'; 1='General Population'")

#### Science Literacy ####
#Find the questions on science literacy
look_for(full, 'tomatoes')
#Check value labels for Q14
val_labels(full$Q14_1)


full %>%
  #We are mutating existing variables so the mutate command
  mutate(
    #We are making the variable know1 using the case_when function
    know1=case_when(
      #when Q14_1 is 1 or 2, then they got the question correct
      #So when it less than 3, it gets a 1
      #separate with a comma
    Q14_1<3 ~ 1,
    #When it is greater than 2, it is equal to 0
    # separate with a comma
    Q14_1>2 ~ 0,
    #All other cases
    TRUE ~ 0
  ),
    know2=case_when(
      #when Q14_2 is 1 or 2, then they got the question correct
      #So when it less than 3, it gets a 1
    Q14_2<3 ~ 1,
    #When it is greater than 2, it is equal to 0
    Q14_2>2 ~ 0,
    TRUE ~ 0
  ),
    know3=case_when(
    Q14_3>2 ~ 1,
    #when Q14_3 is 3 or 4, then they got the question correct
    #So when it more than 2, it gets a 1
    Q14_3<3 ~ 0,
    #when it is less than 3, it is equal to 0
    TRUE ~ 0
  ),
    know4=case_when(
    #when Q14_4 is 3 or 4, then they got the question correct
    #So when it more than 2, it gets a 1
    Q14_4>2 ~ 1,
    #when it is less than 3, it is equal to 0
    Q14_4<3 ~ 0,
    TRUE ~ 0
  )
  )->full

#check full$know
full$know1
full$know2
full$know3
full$know4

##Calculate the mean of knowledge questions and save as "mean_know" variable

full %>%
mutate(across(starts_with('know'),as.numeric)) %>% 
  rowwise() %>% 
  mutate(mean_know=mean(c_across(starts_with('know')))) %>% 
  ungroup()->full

#### Cognitive Reflection Questions####
# We need to code the correct answers to these questions as correct.
# This code spits out an html table that has all the responses in it. T/here are 229 unique responses.
#

library(knitr)
library(kableExtra)
full %>%
  select(Q18_1:Q21_1) %>%
  pivot_longer(cols=1:4) %>%
  group_by(name, value) %>%
  summarize(n=n()) %>%
pivot_wider(., names_from=c('name'), values_from='n') %>%
  rename(., "response"=1) %>%
  kable(., format="html") %>% 
save_kable(., file=here("Tables", "CRT_responses.html"))
#Find the questions on crt
look_for(full, 'race')
#Check value labels for Q18
val_labels(full$Q18_1)
look_for(full, 'old')

full %>%
  #We are mutating existing variables so the mutate command
  mutate(
    #We are making the variable crt1 using the case_when function
    crt1=case_when(
     #When it is a variation of 2 or second, it is correct
      #separate with a comma
      Q18_1 == 2 ~ 1,
      Q18_1 == "2" ~ 1,
      Q18_1 == "2 nd" ~ 1,
      Q18_1 == "2e" ~ 1,
      Q18_1 == "2eme" ~ 1,
      Q18_1 == "2 iemes" ~ 1,
      Q18_1 == "2eme" ~ 1,
      Q18_1 == "2nd" ~ 1,
      Q18_1 == "Deuxieme" ~ 1,
      Q18_1 == "deuxième" ~ 1,
      Q18_1 == "sec place" ~ 1,
      Q18_1 == "second" ~ 1,
      Q18_1 == "Second." ~ 1,
      Q18_1 == "secondplac" ~ 1,
      Q18_1 == "2nd place" ~ 1,
      Q18_1 == "2nd Place" ~ 1,
      Q18_1 == "deuxieme" ~ 1,
      Q18_1 == "Deuxième" ~ 1,
      Q18_1 == "DEUXIÈME" ~ 1,
      Q18_1 == "deuxième8" ~ 1,
      Q18_1 == "SECOND" ~ 1,
      Q18_1 == "sexond" ~ 1,
      Q18_1 == "Second" ~ 1,
      Q18_1 == "2ieme" ~ 1,
      Q18_1 == " deuxième" ~ 1,
      #All other cases
      TRUE ~ 0
    ),
    crt2=case_when(
      #When it is a variation of 8 or eight, it is correct
      Q19_1 == 8 ~ 1,
      Q19_1 == "8 alive" ~ 1,
      Q19_1 == "8 left" ~ 1,
      Q19_1 == "8 moutons" ~ 1,
      Q19_1 == "8 sheep" ~ 1,
      Q19_1 == "8 vivants" ~ 1,
      Q19_1 == "Eight" ~ 1,
      Q19_1 == "eight" ~ 1,
      Q19_1 == " 8" ~ 1,
      Q19_1 == "8 Sheep" ~ 1,
      Q19_1 == "EIGHT" ~ 1,
      #All other cases
      TRUE ~ 0
    ),
    crt3=case_when(
      #When it is a variation of Emily, it is correct
      Q20_1 == "Emilie" ~ 1,
      Q20_1 == "Émilie" ~ 1,
      Q20_1 == "emille" ~ 1,
      Q20_1 == "Emily" ~ 1,
      Q20_1 == "Emily 9" ~ 1,
      Q20_1 == "Emily." ~ 1,
      Q20_1 == "Emily..." ~ 1,
      Q20_1 == "Emily27" ~ 1,
      Q20_1 == "emille" ~ 1,
      Q20_1 == "Emliy" ~ 1,
      Q20_1 == "Emiky" ~ 1,
      Q20_1 == "emilie" ~ 1,
      Q20_1 == "EMILIE" ~ 1,
      Q20_1 == "émilie" ~ 1,
      Q20_1 == "ÉMILIE" ~ 1,
      Q20_1 == "EMILTY" ~ 1,
      Q20_1 == "eMILY" ~ 1,
      Q20_1 == "EMILY" ~ 1,
      Q20_1 == "Emily’s" ~ 1,
      Q20_1 == "Emilys" ~ 1,
      Q20_1 == "emily" ~ 1,
      #All other cases
      TRUE ~ 0
    ),
    crt4=case_when(
      #When it is a variation of 0 or zero, it is correct
      Q21_1 == 0 ~ 1,
      Q21_1 == "0'" ~ 1,
      Q21_1 == "0 no dirt" ~ 1,
      Q21_1 == "C’est pad" ~ 1,
      Q21_1 == "None" ~ 1,
      Q21_1 == "Zero" ~ 1,
      Q21_1 == "zero dirt" ~ 1,
      Q21_1 == "zzero" ~ 1,
      Q21_1 == "nothing" ~ 1,
      Q21_1 == "none" ~ 1,
      Q21_1 == "NONE" ~ 1,
      Q21_1 == "zero" ~ 1,
      #All other cases
      TRUE ~ 0
    )
  )->full

#check full$crt
full$crt1
full$crt2
full$crt3
full$crt4



#### This code produces a table that compares the results of the text responses with the numeric responses.
#### It is a diagnostic tool

cat(kable(table(full$Q18_1, full$crt1), format="html"), file=here("Tables", "crt1_diagnostics.html"))
cat(kable(table(full$Q19_1, full$crt2), format="html"), file=here("Tables", "crt2_diagnostics.html"))
cat(kable(table(full$Q20_1, full$crt3), format="html"), file=here("Tables", "crt3_diagnostics.html"))
cat(kable(table(full$Q21_1, full$crt4), format="html"), file=here("Tables", "crt4_diagnostics.html"))

            
##Calculate the mean of CRT questions and save as "mean_CRT" variable
full %>% 
  rowwise() %>% 
  mutate(mean_crt=mean(c_across(starts_with('crt')))) %>% 
  ungroup()->full
#### Ideology ####
full %>%
  mutate(Ideology=revScale(Q51))->full
#### Worldviews
#Scale Egalitarian items 0 to 1

full %>% 
  mutate(across(Q38_1:Q38_3, revScale, reverse=T,.names="{.col}_x")) ->full
#Scale Individual Items 0 to 1
#Note reverse scale all but Q38_4
full %>% 
  mutate(Q37_1_x=revScale(Q37_1, reverse=T),
         Q37_2_x=revScale(Q37_2, reverse=T),
         Q37_3_x=revScale(Q37_3, reverse=T),
         Q37_4_x=revScale(Q37_4, reverse=F),
         ) ->full
#Scale Hierarchy Items 0 to 1
full %>% 
  mutate(across(Q39_1:Q39_3, skpersonal::revScale, .names="{.col}_x")) ->full
####
####TRUST SCALE####

##Preview variables and labels
full$Q32
var_label(full$Q32)
full$Q33
var_label(full$Q33)
full$Q34
var_label(full$Q34)
full$Q35
var_label(full$Q35)
full$Q36
var_label(full$Q36)

##Mutating variables to be scaled from 0-1
full %>% 
  mutate(
    trust_politicians_lie=case_when(
      Q32==1~1,
      Q32==2 ~0.66,
      Q32==3 ~ 0.33,
      Q32==4~0
    )
  )->full
full %>%
  mutate(
    trust_ottawa=case_when(
      #Q33 is scaled from 0-1
      Q33 ==  1 ~ 0,
      Q33 ==  2 ~ .25,
      Q33 ==  3 ~ .5,
      Q33 ==  4 ~ .75,
      Q33 ==  5 ~ 1,
    ),
    trust_waste=case_when(
      #Q34 is scaled from 0-1
      Q34 == 1 ~ 0,
      Q34 == 2 ~ .5,
      Q34 == 3 ~ 1,
    ),
    trust_interests=case_when(
      #Q35 is scaled from 0-1
      Q35 == 1 ~ 0,
      Q35 == 2 ~ 1,
    ),
    trust_people=case_when(
      #Q36 is scaled from 0-1
      Q36 == 1 ~ 0,
      Q36 == 2 ~ 1,
    )
  )->full

#Calculate mean
#Start with the dataframe
full %>% 
  #Work rowwise
  rowwise() %>% 
  #mutate, create  anew variable called trust_averaage
  mutate(trust_average=
           #It is the product of the average of 
           mean(
             #The columns c_across specified in the next row
             c_across(
               #starts_with trust
               starts_with('trust')
               #Close the brackets
               )
             )
         #save the foregoing
         )->full
#Check
full$trust_average
full %>% 
  select(starts_with('trust'))
#### Interest in Politics ####
full %>% 
  mutate(Interest=case_when(
    Q52==1~ 0,
    Q52==2~0.25,
    Q52==5~0.5,
    Q52==3~0.75,
    Q52==4~1
  ))->full
#### Code Public Health Most Important Problem Respones ####
#### This code below assigns meaningful variable names to the most important problem variables

#Start with the dataframe
full %>%
  #Select the most important problem question
  select(Q1_1:Q1_9) %>%
  #Get the variable labels
  var_label() %>%
  #Stack them
bind_rows() %>%
pivot_longer(cols=everything()) %>%
  #This gets the pattern Obesity, VAccine Hesitancy, etc,
mutate(out=str_extract(value, pattern="\\s(.*?)\\s-")) %>%
  #This delets the remaining -
  mutate(out=str_replace_all(out, pattern=" -", replace=""),
         #This trims whitespace
         out=str_trim(out, side='both')) %>%
  #Dump this into a temporary data frame out
  select(out)->out
#Check
out$out

#### Assign variable names to the new variables
#Start with the data frame
full %>%
  #Select the questions that are the most important problem
  select(Q1_1:Q1_9) %>%
  #Rename them with out$out from above
  rename_with(~out$out, everything()) %>%
  #Bind the columns from the original `full` dataframe and add to them the new variables we just made
  bind_cols(full, .) ->full


names(full)
full %>%
  rename(`Vacc_hesitancy`=`Vaccine hesitancy`,
         `Opioid`=`Excessive opioid use`,
         `Alcohol`=`Excessive alcohol use`,
         `Race_inequality`=`Racial inequalities`)->full
#Check
names(full)

#### This code writes out the spreadsheet with the text responses
#Encoding(full$Q1_9_SP)<-'UTF-8'
#full$Q1_9_SP<-iconv(full$Q1_9_SP, from="UTF-8", to="LATIN1")
library(readr)
library(openxlsx)
full %>%
  mutate(other_problem_text=tolower(Q1_9_SP)) %>%
  group_by(other_problem_text) %>%
  summarize(n=n()) %>%
  mutate(category=rep("", nrow(.)))->mip

###Read in the first draft of coding
# scott_v1_names<- openxlsx::getSheetNames("data/other_problem_text.xlsx")
# scott_v1_list <- lapply(scott_v1_names,openxlsx::read.xlsx,xlsxFile="data/other_problem_text.xlsx")
# names(scott_v1_list) <- scott_v1_names
# str(scott_v1_list)
#
# scott_v1_list[[2]] %>%
#   right_join(., mip, by="other_problem_text") ->mip_version_2
# str(mip_version_2)
# mip_version2_list<-list(scott_v1_list, mip_version_2)
# mip_list<-list(scott_v1_list, mip_version_2)
#
# write.xlsx(mip_list, "data/mip_list.xlsx")

#Read in second draft of mip coding

mip<-readWorkbook(xlsxFile="data/mip_list.xlsx", sheet=2)
mip %>%
  select(other_problem_text, category.x)->mip
#The responses are categorized in other_problem_text
#The codes are categorized in category.x
names(full)
full %>%
  mutate(other_problem_text=tolower(Q1_9_SP)) %>%
  full_join(., mip, by="other_problem_text") %>%
  rename(.,other_mip=`category.x`)->full
names(full)
#### Pandemic Response Preferences #### 
#This package provides a useful rescale function that rescales variables to 0 and 1
#remotes::install_github('sjkiss/skpersonal')
library(skpersonal)
summary(revScale(full$Q8_1))
head(revScale(full$Q8_1))
library(scales)
full %>% 
  ungroup() %>% 
  mutate(across(starts_with('Q8_'), revScale, .names="{.col}_x")) ->full
names(full)

#### Demographics ####
#Age
#
full$Age<-2021-full$Q55_1
look_for(full, 'year')
# Q55_1 is the age variable
# Start with dataframe
full %>%
  #mutate and create a new variable with a meaningful name
  mutate(old=case_when(
    #If Q55_1 is greater than 2021-65, then yes, they are "old" , so they get a 1
    Q55_1<2021-65 ~1,
    #otherwise they get a zero
    TRUE~ 0
  ))->full

# Create second old variable
full %>% 
  select(Sample, Age) %>% 
  group_by(Sample) %>% 
  summarize(mean=mean(Age))
2021-65
full %>%
  #mutate and create a new variable with a meaningful name
  mutate(old2=case_when(
    #If Q55_1 is greater than 2021-65, then yes, they are "old" , so they get a 1
    Q55_1<2021-44 ~1,
    #otherwise they get a zero
    TRUE~ 0
  ))->full



table(full$old2, full$Age)
full %>% 
mutate(age_4=case_when(
#18 to 34
  Age > 17 & Age < 35 ~ 1,
#35-50  
  Age > 34 & Age < 51 ~ 2,
#51-64
  Age > 50 & Age < 65 ~ 3,
#65+
  Age > 64 ~ 4
))->full

var_label(full$age_4)<-'Age Category (4), R Age'
val_labels(full$age_4)<-c('Age 65+' = 4, 'Age 51-64'= 3, 'Age 35-50' = 2, 'Age 18-34' = 1)

full%>%
mutate(age_2=case_when(
#Under 50
  Age < 50 ~ 1,
#50 and up
  Age > 49 ~ 2
))->full

var_label(full$age_2)<-'Age Category (2), R Age'
val_labels(full$age_2)<-c('Age 50+' = 2, 'Age 18-49' = 1)

names(full)


full %>%
  mutate(quebec=case_when(
    S1==13 ~1,
    TRUE~ 0
  ))->full


#Please repeat this for:"

lookfor(full, "education")
full %>%
  #mutate and create a new variable with a meaningful name
  #e.g. Is the person "Old" or not? Is the person "Male" or not
  # e.g. Is the person a "Quebecker or not"
  mutate(degree=case_when(
    Q54>6 ~1,
    #otherwise they get a zero
    TRUE~ 0
  ))->full
# quebec
full %>%
  #mutate and create a new variable with a meaningful name
  #e.g. Is the person "Old" or not? Is the person "Male" or not
  # e.g. Is the person a "Quebecker or not"
  mutate(female=case_when(
    Q53==2~1,
    #otherwise they get a zero
    TRUE~ 0
  ))->full
full$S1
lookfor(full, "gender")
lookfor(full, "language")
full %>%
  mutate(francophone=case_when(
    L1==2~1,
    TRUE~0
  ))->full

# Rich

full %>%
  mutate(rich=case_when(
    Q56>5 ~ 1,
    TRUE~0
  ))->full

#### Vaccine Hesitancy #### 

full$Vaccines<-as_factor(full$Q23)
levels(full$Vaccines)
full$Vaccines<-fct_relevel(full$Vaccines, "Not Sure", after=2)
full %>% 
  mutate(Vaccines=recode(Vaccines, 'I have already been vaccinated'=NA_character_)) ->full

full %>% 
  filter(!is.na(Vaccines)) %>% 
  ggplot(., aes(y=Sample, fill=Vaccines))+geom_bar(position="fill")+labs(title="Plans to Vaccinate by Sample")
#ggsave(filename=here("Plots", "vaccine_plans_group.png"), width=8, height=2)


#### Assign Variable Labels To Influence Questions #### 
#These questions asked about what does and what should influence public policy
#This code section takes the variable names from these questions and assigns them to 
full %>% 
  select(num_range('Q3_', 1:7)) %>% 
  var_label() %>% 
  bind_rows() %>%
  pivot_longer(cols=everything()) %>%
  #This gets the pattern Scientific Evidence - 
  mutate(out=str_extract(value, pattern="\\s(.*?)\\s-")) %>%
  #This delets the remaining -
  mutate(out=str_replace_all(out, pattern=" -", replace=""),
         #This trims whitespace
         out=str_trim(out, side='both'),
         out=paste(out, " does", sep=""), 
         out=str_replace_all(out,pattern=" ", replace="_")) %>%
  #Dump this into a temporary data frame out
  select(out)->out

out
#This code renames the original variables with the value labels stored in out, above. 
#Start with the data frame
full %>%
  #Select the questions that are the most important problem
  select(num_range('Q3_', 1:7)) %>%
  #Rename them with out$out from above
  rename_with(~out$out, everything()) %>%
  #Bind the columns from the original `full` dataframe and add to them the new variables we just made
  bind_cols(full, .) ->full

full %>% 
  select(num_range('Q4_', 1:7)) %>% 
  var_label() %>% 
  bind_rows() %>%
  pivot_longer(cols=everything()) %>%
  #This gets the pattern scientific evidenc - ,
  mutate(out=str_extract(value, pattern="\\s(.*?)\\s-")) %>%
  #This delets the remaining -
  mutate(out=str_replace_all(out, pattern=" -", replace=""),
         #This trims whitespace
         out=str_trim(out, side='both'),
out=paste(out, " should", sep=""), 
out=str_replace_all(out,pattern=" ", replace="_")) %>%
  #Dump this into a temporary data frame out
  select(out)->out

out

#Start with the data frame
full %>%
  #Select the questions that are the most important problem
  select(num_range('Q4_', 1:7)) %>%
  #Rename them with out$out from above
  rename_with(~out$out, everything()) %>%
  #Bind the columns from the original `full` dataframe and add to them the new variables we just made
  bind_cols(full, .) ->full


#### Provide names for trade-off variables
#Check this
full %>% 
mutate(across(Q9_1:Q12_1, revScale, .names="{.col}_x")) %>% 
  select(Q9_1_x:Q12_1_x) %>% 
  summary()

#Repeat and ave
full %>% 
  mutate(across(Q9_1:Q12_1, revScale, .names="{.col}_x"))->full

#Add the named versions of these variables for backwards compatibility
full %>% 
  mutate(decline_economy=Q9_1,
         social_isolation=Q10_1, schools_open=Q11_1, seniors_isolation=Q12_1 )->full



#This line executes the file that merges Tim's FSA file with the COVID case count data
# When troubleshooting the merge script, it is advised to *not* run this line
# INstead, it is advised to run this full script and then step through the code in 
# 2b_fsa_merge_covid_incidence.R step by step to see how it works.
source('R_scripts/2b_fsa_merge_covid_incidence.R')

#### Assigning Rural Values ####
#Draw a histogram of the population density

qplot(full$FSA.pop.km2)
summary(full$FSA.pop.km2) #median is at 903

# mean is at 4593, so there are some huge outliers, very dense populations. 
full$Sample
full %>% 
  filter(Sample=="Public Health") %>% 
  ggplot(., aes(x=FSA.pop.km2))+geom_histogram()

full %>% 
  group_by(Sample) %>% 
  summarize(mean=mean(FSA.pop.km2, na.rm=T), median=median(FSA.pop.km2, na.rm=T))

full %>% 
  filter(FSA.pop.km2<25000) %>% 
  ggplot(., aes(x=FSA.pop.km2))+geom_histogram()+facet_grid(~Sample)

# First Cut Rural < 1430 people per square km urban > 1429 people per square km

full %>% 
  mutate(rural=case_when(
    FSA.pop.km2 < 400 ~ 1,
    FSA.pop.km2 > 401 ~ 0,
    TRUE ~ NA_real_
  ))->full


names(full)
#### Assign Value labels and variable labels ####
#This code section assigns some value labels and variable labels to new variables 
# to improve communication with colleagues
#Ensure the library(labelled) is loaded
library(labelled)
names(full)
#Set the variable label for each variable
var_label(full$old)<-'Dichotomous variable, R is 65+'
var_label(full$old2)<-'Dichotomous variable, R is 45+'
#Set the value labels for each variable
val_labels(full$old)<-c(`Over 65`=1, `Under 65`=0)
val_labels(full$old2)<-c(`Over 45`=1, `Under 45`=0)
var_label(full$degree)<-c('Dichotomous variable, R has university degree')
val_labels(full$degree)<-c(`Degree`=1, `No Degree`=0)

var_label(full$rich)<-'Dichotomous variable, R household > $100,000'
val_labels(full$rich)<-c(`Over $100,000`=1, `Under $100,000`=0)

var_label(full$quebec)<-'Dichotomous variable, R is resident of Quebec'
val_labels(full$quebec)<-c(`Quebecker`=1, `Outside Quebec`=0)

var_label(full$francophone)<- 'Dichotomous variable, R is francophone'
val_labels(full$francophone)<-c(`Francophone`=1, `Not Francophone`=0)

var_label(full$female)<-'Dichotomous variable, R is female'
val_labels(full$female)<-c(`Female`=1,`Not Female`=0)

add_value_labels(full, 
                 Q30_1=c('Public policy should be based on the best available scientific evidence'=1, 
                         'Public policy should be determined by many factors including scientific evidence'=7))
val_labels(full$rural)<-c(`Rural`=1, `Not Rural`=0)

# Add Variable Labels for Q8_1_x specifying that they are reversed versions of Q8_1, Q8_2, etc. 
var_label(full$Q8_1_x)<- 'Reversed version of Q8_1'
var_label(full$Q8_2_x)<- 'Reversed version of Q8_2'
var_label(full$Q8_3_x)<- 'Reversed version of Q8_3'

# Add Variable label for Vaccines specifying it is vaccine hesitancy from Q23
var_label(full$Vaccines)<- 'Vaccine hesitancy question, Q23'

#add Variable Label for rural specifying that 1 = rural and 0 = not rural 
var_label(full$rural)<- 'Dichotomous variable, R lives in a rural neighborhood'

#Add variable label 
names(full)
#Add variable label to HR specifying that it is Health Region code
var_label(full$HR)<- 'Health region code'
var_label(full$HR_UID)<- 'Health region code'
#Add var_label to HR_Name specifying it is the name of the health region
var_label(full$HR_NAME) <- 'Name of health region'
names(full)
#Add variable label to pop.2016 specifying that it is population for FSA
var_label(full$FSA.pop.2016)<- 'Population for FSA'
#Add variable label to pop.2016 specifying that it is population for FSA
var_label(full$FSA.pop.km2)<- 'Population per km2 for FSA'
#Add Var label to area.km2 specifying it is FSA Square Kilometer
var_label(full$FSA.area.km2)<- 'FSA Square Kilometer'
# Add var label to date_report specifying it is the date reported of Covid cases for Respondent Health Region
var_label(full$date_report)<- 'Date reported of COVID cases for the respondent health region'
# Add var label to cases specifying it is the # of COVID cases for that health region
var_label(full$cases)<- '# of COVID cases for that health region'
# Add var label to cumulative_cases specifying it is the # of covid cases for that health 
var_label(full$cumulative_cases)<- 'Cumulative COVID cases for that health region'
#Add var label to cumulative_deaths specifying that it is the cumulative covid deaths
var_label(full$cumulative_deaths)<- 'Cumulative deaths from COVID'
#add var label to deaths specifying that it is the deaths.
var_label(full$deaths)<- 'Deaths'

#Add var label to case_trend specifying that it is the covid trending number, the average covid cases in the last 7 days divided by the last 14 days
var_label(full$case_trend)<- 'Covid trending numbers (average covid cases in the last 7 days divided by average cases in the last 14 days'
#Add var_label to pop to health region population
var_label(full$health_region_population) <- 'Population of health region'
#Add var_label to avgtotal_last7_pop_per_capita 
var_label(full$avgtotal_last7_pop_per_capita) <- 'Average COVID cases in last 7 days per capita'
var_label(full$Age)<-"R Age"

full %>% 
  select(Q9_1:Q12_1) %>% 
  val_labels()
var_label(full$Q9_1_x)<-"Stop Economic Decline At Expense of Increased COVID19"
var_label(full$Q10_1_x)<-"Reprieve From Social Isolation At Expense of Increased COVID19"
var_label(full$Q11_1_x)<-"Keep Schools Open At Expense of Increased COVID19"
var_label(full$Q12_1_x)<-"Reprieve For Seniors fro Isolation At Expense of Increased COVID19"

# Value and variable labels for interest
var_label(full$Interest)<-'interest in politics'
val_labels(full$Interest)<-c(`Very disinterested`=0, `Moderately disinterested`=0.25, 
                             `Neutral`=0.5, `Moderately interested`=0.75, `Very interested`=1)

#Rename some variables

#Saving factors with capitalized names
full$`High Income`<-as_factor(full$rich)
full$`High_Income`<-as_factor(full$rich)
full$Rural1<-as_factor(full$rural)
full$Francophone1<-as_factor(full$francophone)
full$Degree1<-as_factor(full$degree)
full$Female1<-as_factor(full$female)
full$Old1<-as_factor(full$old)
full$Old2<-as_factor(full$old2)
full$Province<-full$province
names(full)

#Missing FSA,population or hrealth region data

full %>% 
  filter(is.na(FSA.pop.km2)) %>% 
  select(CID, FSA, province, health_region)

#### Bad Data####
qplot(full$Age)
full %>% 
  filter(Age> 17)->full

full %>% 
  filter(province_fsa_bad==1) %>% 
  select(FSA, province, province_full, Province)

full %>% 
  filter(province_fsa_bad!=1)->full
#### Write out the data save file ####
# names(full)
# table(full$Sample)
# 
# write_sav(full, path=paste0(here("data", "/recoded_data"), "_",Sys.Date(), ".sav"))
names(full)
table(full$province_fsa_bad) 


