#read_in
fsa<-read.csv(file=here("data/fsa_canada.csv"))
#check how many rows there are
nrow(fsa)
#How manuy unique FSAs there are
#note this includes the Canada levl
length(unique(fsa$GEO_CODE..POR.))
fsa$GEO_CODE..POR.
fsa %>% 
  filter(`GEO_CODE..POR.`=='A0D')

fsa %>% 
  #Filter out the canada level
  filter(GEO_LEVEL==2) %>% 
  #Keep just the population count for each FSA
  filter(DIM..Profile.of.Forward.Sortation.Areas..2247.=="Population, 2016")->out

out %>% 
  #Define the rural variable
  mutate(rural=case_when(
    #when the first digit i.e. \\d ==0 then define the FSA as rural
    str_match(GEO_CODE..POR., "\\d")==0 ~ 'rural',
    #Else as other
    TRUE~'other'
    #Store
  ))->out
#Check the population variable
out$Dim..Sex..3...Member.ID...1...Total...Sex
#Rename
out %>% 
  rename(`Total`="Dim..Sex..3...Member.ID...1...Total...Sex")->out

#Convert to number
out$Total<-as.numeric(out$Total)
View(out)

out %>% 
  group_by(rural) %>% 
  summarize(Total=sum(Total)) %>% 
  mutate(Percent=Total/sum(Total))->out

