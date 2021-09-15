#### Merge Region Health name #### 
#install.packages('tidylog')
#This is a little package that provides diagnostics after merging. 
#library(tidylog) 
#First we read in Tim's file that used the PCCF+ to assign the FSAs to health regions. 
## It is based on code from Tim Gravelle and Laurier's PCCF file, which has to be run elsewhere
health_regions<-read.csv(file=here("data", "CID health region spatial join-2021 09 10.csv"))

#### PRovince names in Tim's file ####
#Tim's file did not have province names, which might be necessary
health_regions$PR
names(health_regions)
health_regions %>% 
  mutate(province=case_when(
    PR==10 ~ "Newfoundland and Labrador",
    PR==11 ~ "PEI",
    PR==12 ~ "Nova Scotia",
    PR==13  ~ "New Brunswick",
    PR==24 ~ "Quebec",
    PR==35~ "Ontario",
    PR==46 ~ "Manitoba",
    PR==47 ~ "Saskatchewan",
    PR==48 ~ "Alberta",
    PR==59 ~ "British Columbia",
    PR==60 ~ "Yukon",
    PR==61~ "NWT",
    PR==62 ~ "Nunavut"
  ))->health_regions
class(health_regions$province)
class(full$province)
table(full$province)
as.character(full$province)
health_regions$province<-as.factor(health_regions$province)
health_regions %>% 
  select(-PR)->health_regions
names(health_regions)
names(full)

#Merge With full

full %>% 
  left_join(., health_regions, by=c('CID', 'FSA'))->full
names(full)
full %>% 
  rename(province=province.x) %>% 
  select(-province.y)->full

nrow(full)
full %>% 
  filter(is.na(HR_UID)) %>% 
  select(CID, FSA, HR_UID, province) %>% 
  View()
#### Compare with covid19 dataset####
#Note, that this is only a moderately well documented package
#Uncomment and run this to install the COVID19DATA

#devtools::install_github("ccodwg/Covid19CanadaData")
library(Covid19CanadaData)

#This file has covid case counts for health regions, without HRUIDs
# But as of May 21, 2021, this was not working at all. 
#covid<-dl_dataset(uuid='746c01f3-e597-4413-89e0-afa561bf81d8')

#This line downloads the ccodwg timeseries by health region, but it only includes case counts
#The dataset below has more data.
#covid<-read_csv(file="https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/timeseries_hr/cases_timeseries_hr.csv")

#This appears to download health canada's comprehensive time series by health region
#However, this file does not contain the HRUIDs 
covid<-read_csv(file="https://health-infobase.canada.ca/src/data/covidLive/file_out_v5.csv")
head(covid)
#Note that there are no HR_UID numbers in the covid file.

# THE FUNDAMENTAL PROBLEM IS THAT TIM'S FILE HAS THE HRUID BUT THE COVID CASE DATA DOES NOT
# WE NEED TO ADD THE HRUID TO THE COVID CASE DATA FILE SO THAT WE CAN THEN LINK THE COVID CASE DATA FILE
# TO THE FSA VIA THE HEALTH REGIONS (HRUID) 

#### Get HHR_UID####
#This file does have the health region names and the hruids
hruid<-read_csv(file="https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/other/hr_map.csv")
names(hruid)
hruid$health_region
hruid$health_region_esri
#Some inspections
names(covid)
names(hruid)
names(health_regions)

# THE NEED TO MERGE BY HR_UID IS  ALSO NECESSARY BECAUSE THE NAMES IN THE FILES ARE DIFFERENT
# FOR EXAMPLE IN THE covid case set there is this
covid %>% 
  filter(str_detect(health_region, 'Calgary')) #Calgary
# IN THE HRUID FILE THERE IS THIS
hruid%>% 
  filter(str_detect(health_region, 'Calgary')) # Calgary
#AND IN TIM'S FILE THERE IS THIS. 
health_regions %>% 
  filter(str_detect(HR_NAME, "Calgary")) #Calgary Zone
#You see the problem.
# Tim's data has "Caglary Zone"
#But the CCODWG has "Calgary". But, by adding the hruid to the covid case count data, we should 
# then be able to merge with Tim's file that contains FSAs and health region data

#THIS MERGES THE COVID FILE WITH THE HRUID FILE BY THE VARIABLE HEALTH_REGION
covid
names(hruid)
table(hruid$province)
table(covid$province)
covid %>% 
  left_join(., hruid, by=c("province"="province", "health_region"))->covid

names(covid)

names(health_regions)
health_regions$HR
health_regions$HR_UID


#This is some checks to see where we are at. 
#Now merge full with 
names(full)
#### Merge Covid Case Count ####

#We need to be sure that both covid and full have proper date variables
#Because we want to get the case counts for the day the repsondent too the interview
qplot(full$START_DATE, geom='histogram')
library(lubridate)
covid$date<-dmy(covid$date_report)
class(covid$date)
class(full$date)

names(full)
names(covid)
#This is a test merge that dumps the merge into out.
full %>% 
  left_join(., covid, by=c("province", "health_region", "HR_UID", "date" )) ->out

#Check in the new maerged dataset
# out %>% 
#   filter(HR_NAME=="Calgary Zone") %>% 
#   #filter(date=="2020-02-16") %>% 
#   select(date, HR_UID,cases, Comm_Name, health_region, HR_NAME) %>% 
#   View()
#Compare CAlgary in the original covid data-set

covid %>% 
  filter(health_region=="Calgary") %>% 
  filter(date=="2020-02-16") %>% 
  select(health_region, date_report, date, cases)

#A person can search for different communities here and see how they shake out. 
# out %>% 
#   filter(str_detect(Comm_Name, "TORONTO")) %>% 
#   select(date, HR_UID,cases, Comm_Name, health_region, HR_NAME) %>% 
#   View()
  
#### Complete merge #### 
full %>% 
  left_join(., covid, by=c('province',"HR_UID", "date" )) ->full

#Which of our respondents do not have health region names
#It looks like there are 
names(full)
# full %>% 
#   filter(is.na(health_region.x)) %>%
# select(HR_UID, Comm_Name, FSA, HR_NAME, cases, date, Q65_1) %>% 
#   distinct(Q65_1) %>% 
#   View()

# full %>% 
#   filter(is.na(HR_UID)) %>% 
#   select(HR_UID, Comm_Name, FSA, HR_NAME, cases, date) %>% 
#   write_csv(., file=here("data", "missing_health_regions.csv"))

#### Make trend variable#### 


#This command creates the variable case_trend
# It divides the 7 day average of cases by the 14 day cases.
# If the two are equal, (e.g. the most recent seven days average are the same 
# As the most 14 days average) , then the ratio will equal 1, and the covide
# Case rate is stabilized. If it the most recent seven day case average is larger
# than 1, then it is rsiing, if it lower than 1 it is falling. 
full$case_trend<-full$avgtotal_last7/full$avgtotal_last14

full$avgtotal_last7_pop_per_capita<-full$avgtotal_last7/full$pop
#Divide by population

qplot(full$case_trend, geom="histogram", title="Distribution of trend variable")

      names(full)
      