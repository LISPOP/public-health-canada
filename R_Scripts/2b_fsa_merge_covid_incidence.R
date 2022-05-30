#### Merge Region Health name #### 
#install.packages('tidylog')
#This is a little package that provides diagnostics after merging. 
#library(tidylog) 
#First we read in Tim's file that used the PCCF+ to assign the FSAs to health regions. 
## It is based on code from Tim Gravelle and Laurier's PCCF file, which has to be run elsewhere
health_regions<-read.csv(file=here("data", "CID health region spatial join-2021 09 24.csv"))

#### PRovince names in Tim's file ####
#Tim's file did not have province names, which might be necessary
health_regions$PR
names(health_regions)
health_regions %>% 
  filter(CID==492388703| CID==492712971) %>% 
  select(CID, PR, FSA)

health_regions %>% 
  mutate(province=case_when(
    PR==10 ~ "Newfoundland and Labrador",
    PR==11 ~ "Prince Edward Island",
    PR==12 ~ "Nova Scotia",
    PR==13  ~ "New Brunswick",
    PR==24 ~ "Quebec",
    PR==35~ "Ontario",
    PR==46 ~ "Manitoba",
    PR==47 ~ "Saskatchewan",
    PR==48 ~ "Alberta",
    PR==59 ~ "British Columbia",
    PR==60 ~ "Yukon",
    PR==61~ "Northwest Territories",
    PR==62 ~ "Nunavut"
  ))->health_regions

#Convert to factor
health_regions$province<-as.factor(health_regions$province)
#Delete -PR
health_regions %>% 
  select(-PR)->health_regions
names(health_regions)
names(full)

#Merge With full
full %>% 
  left_join(., health_regions, by=c("CID", "FSA"))->full
names(full)
full %>% 
  rename(`FSA.pop.2016`=pop.2016, FSA.area.km2=area.km2, FSA.pop.km2=pop.km2)->full
#Check which cases have deviant provinces
full %>% 
  mutate(province_fsa_bad=case_when(
    as.character(province.x)!=as.character(province.y)~ 1,
    TRUE ~ 0
  ))->full

full %>% 
  filter(province_fsa_bad==1) %>% 
  select(CID, FSA, province.x, province.y) %>% 
  write.csv(here("data", "fsa_province_mismatch.csv"))

full %>% 
 # select(-province.x) %>% 
  rename(province_original=province.x, province=province.y)->full

#Now full has the full FSA and health region name added

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

#Note that there are no HR_UID numbers in the covid file.

# THE FUNDAMENTAL PROBLEM IS THAT TIM'S FILE HAS THE HRUID BUT THE COVID CASE DATA DOES NOT
# WE NEED TO ADD THE HRUID TO THE COVID CASE DATA FILE SO THAT WE CAN THEN LINK THE COVID CASE DATA FILE
# TO THE FSA VIA THE HEALTH REGIONS (HRUID) 

# #### Get HHR_UID AND HEALTH REGION POPULATION####
# #This file does have the health region names and the hruids
hruid<-read_csv(file="https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/other/hr_map.csv")


#Some inspections
names(covid)
names(hruid)
names(health_regions)
# 
hruid$province
# Clean up the province names
table(full$province)
table(health_regions$province)
table(hruid$province)
hruid$province<-car::Recode(hruid$province, "'BC'='British Columbia'; 'NL'='Newfoundland and Labrador'")
table(covid$province)
covid$province<-car::Recode(covid$province, "'BC'='British Columbia'; 'NL'='Newfoundland and Labrador'")
#THIS MERGES THE COVID FILE WITH THE HRUID FILE BY THE VARIABLE HEALTH_REGION
covid %>%
left_join(., hruid, by=c("province"="province", "health_region"))->covid


names(health_regions)
health_regions$HR
health_regions$HR_UID


#### Merge Covid Case Count ####

#We need to be sure that both covid and full have proper date variables
#Because we want to get the case counts for the day the repsondent too the interview
qplot(full$START_DATE, geom='histogram')


library(lubridate)
covid$date<-dmy(covid$date_report)
class(covid$date)
class(full$date)
qplot(covid$date, geom='histogram')
names(full)
names(covid)
#This is a test merge that dumps the merge into out.
names(covid)
nrow(covid)
covid$province

names(full)
full %>% 
  left_join(., covid, by=c("province", "health_region",  "HR_UID", "date" )) %>% nrow()

nrow(covid)
full %>% 
  left_join(., covid, by=c("province", "health_region",  "date" )) ->out
nrow(out)

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
  left_join(., covid, by=c( "province", "health_region","HR_UID",  "date" )) ->full

#rename population variables for the health region and the fsa
full %>% 
  rename("health_region_population"="pop")->full
names(full)
#Checkout missing FSA data

full %>% 
  filter(is.na(FSA.pop.2016)) %>% 
  select(FSA, health_region, FSA.pop.2016,FSA.pop.km2, CID,Comm_Name) ->missing_CID
missing_CID

#### Fill IN Missing Data
missing_CID %>% 
  filter(!is.na(health_region))->missing_CID
missing_CID
# For these 6 cases, can you look up an adjacent FSA, find the pop.km2 somewhere of that FSA and change the values here,in the line below IN ORDER!!!!
#missing_CID$pop.km2<-c()

nrow(health_regions)
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
#Model Cases getting better
seven_day<-c(rep(10,7))
fourteen_day<-c(rep(100, 7), seven_day)
mean(seven_day)/mean(fourteen_day)

#Model cases steady
seven_day<-c(rep(10,7))
fourteen_day<-c(rep(10,14))
mean(seven_day)/mean(fourteen_day)

#Model cases increasing
#Make seven days of 10 cases
seven_day<-c(rep(10,7))
#Make fourteen days where the first seven days have 1 case a day and the second seven cases have 10 a day, as above
fourteen_day<-c(rep(1,7), seven_day)
#Take the average of the seven day and divide by the average o the fourteen day
mean(seven_day)/mean(fourteen_day)


#Divide by population
full$avgtotal_last7_pop_per_capita<-full$avgtotal_last7/full$health_region_population
qplot(full$case_trend, geom="histogram", main="Distribution of case trend variable")
names(full)

names(full)

