#####################
### Packages      ###
#####################

library(dplyr)


#####################
### Data Cleaning ###
#####################

## Recommendations - convert data file into a csv on your computer. Then update the below code with your file path string
kisumu_data = read.csv("/Users/ijeamakaanyene/kisumu_241project/Kisumu Street Youth Seroprevalence Data 9_28_2015.csv", header = TRUE)

## Outcome: Glue Sniffing. Use variable glue.ever: 0 - no, 1 - yes

## Exposure: Length of Time on Street. Use variable onstrt_new: 0 - <1yr, 1 - >=1 yr


## Confounders: 
#Education. Use variable edatt_cat_new: 0 is <= grade 5, 1 is > grade 5

#Age. Use variable age (keeping continuous)

#Electricity at Home. Use variable elec: 0 = no electricity, 1 = electricity 
kisumu_data = kisumu_data %>%
  mutate(elec = if_else(electricity == "yes", 1, 
                       if_else(electricity == "no", 0, 
                               if_else(is.na(electricity) == TRUE, 2, 2))))


kisumu_filtered = kisumu_data %>%
  filter(ever.homeless == "yes") %>%
  select(subject.identification.number, glue.ever, onstrt_new, edatt_cat_new, age, elec) %>%
  na.omit(kisumu_data)

##
