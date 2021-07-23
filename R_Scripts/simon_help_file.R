source('R_Scripts/4_cpha_grey_literature_summary.R')
#### Do multiple plots at once ####
# Let's work with Q40
full %>% 
  select(starts_with('Q40'))

### The key is to turn the wide data frame long.
full %>% 
  #We are just working with a subset of variables so we use select
  select(Sample, starts_with('Q40')) ->check
check

check %>% 
  #We are folding everything except Sample longer, watch what happens.
  #The - is an operator in R that says "not". 
  pivot_longer(cols=-Sample)->check
check

#Now we have one variable "name" that contains the name of each variable
#And a variable contained value that contains the response each respondent gave to "name".

check %>% 
  #We use the same code to make the density plot
  #We just add a "facet" or panel that uses the variable "name"
  ggplot(., aes(x=value, fill=Sample))+geom_density()+facet_grid(~name)

check %>% 
  #We use the same code to make the density plot
  #We just add a "facet" or panel that uses the variable "name"
  #facet_wrap is an alternative in which you can specify the number of rows or the number of columns
  ggplot(., aes(x=value, fill=Sample))+geom_density()+facet_wrap(~name, nrow=2, ncol=2)

check %>% 
  #We use the same code to make the density plot
  #We just add a "facet" or panel that uses the variable "name"
  #facet_wrap is an alternative in which you can specify the number of rows or the number of columns
  ggplot(., aes(x=value, fill=Sample))+geom_density()+facet_wrap(~name, nrow=3)

#You can change the labelling of the strip text in this way. 
#Basically it's easiest to actually rename the variables before folding down. 

#Check what the variable labels are in the documentation or by using var_labels()

full %>% 
  select(starts_with('Q40')) %>% 
  var_label()

#So now we have the actual question text
#We can just rename in the pipe flow
#Start with the data frame
full %>% 
  #select variables
  select(Sample, starts_with('Q40')) %>% 
  #Rename
  #Here the period just means, do the rename to the data that you just piped
  #Provide the series of renames by position, 
  #So the second variable gets renamed Forces Beyond control, the 3rd , the 4th, etc. 
  rename(., `Random Chance`=2, `Forces Beyond Control`=3, `Future Uncertain`=4 ) %>% 
  #Pivot longer except for the Sample
  pivot_longer(cols=-Sample) %>% 
  #Plot
  ggplot(., aes(x=value, fill=Sample))+geom_density()+facet_wrap(~name, nrow=3)


# The key philosophy behind ggplot and the tidyverse is that every graphical element of a graph that is meant to communicate some information must come from a "column". So, in this case, the panel communicates the informatino about which particular variable has been responded to. 