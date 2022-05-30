#This script loads the data file that has the survey respondents
#### Load libraries ####
library(tidyverse)
library(haven)
library(here)
library(labelled)
#### Import Data####
#Check working directory
getwd()
#What files are there
list.files()
#list.files in /data
list.files(path="./data")
#read in data
#here returns the root directory
here()
#combining here() with 'data' and the file name returns the exact file path whether stored on a PC or Mac
#Store as full
full<-read_sav('https://github.com/LISPOP/public-health-canada/raw/main/data/ORD-571352-N3W7_Final_SPSS_051821.SAV', encoding='utf8')
#Check structure
str(full)
#View the data
#View(full)
#load the labelled library
library(labelled)
#check value labels

val_labels(full)
#These are some minor transformations
nrow(full)
#Provide uppercase FSA variable to match Tim's FSA's file
full$FSA<-toupper(full$Q65_1)
#provide province variable as factor
full$province<-as_factor(full$S1)
#provide date variable
full$date<-as.Date(full$START_DATE)
nrow(full)
#### export postal codes####
#This section exports the unique FSAs and were used by Tim to merge FSAs to health regions.
#It's no longer necessary to run.
# names(full)
# full %>%
#  # distinct(Q65_1) %>%
#   mutate(fsa=tolower(Q65_1), fsa2=toupper(Q65_1)) %>%
#   select(CID,fsa:fsa2) %>%
#   #distinct(CID) %>% 
#   write_csv(., file=here("data", "fsa2.csv"))


