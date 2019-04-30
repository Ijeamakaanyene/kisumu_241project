#####################
### Packages      ###
#####################

library(dplyr)
library(lmtest)
library(ResourceSelection)


#####################
### Data Cleaning ###
#####################

# Change outcome to drug use in the past months 


## Recommendations - convert data file into a csv on your computer. Then update the below code with your file path string
kisumu_data = read.csv("~/Berkeley/Spring 2019/PH 241/Kisumu Street Youth Seroprevalence Data 9_28_2015.csv", header = TRUE)

## Exposure: Education. Use variable edatt_cat_new: 0 is <= grade 5, 1 is > grade 5

## Outcome: Length of Time on Street. Use variable onstrt_new: 0 - <1yr, 1 - >=1 yr

## Confounders: 

#Age. Use variable age (keeping continuous)

# Create variable with orphan (either parent) -> no = 0, single = 1, double = 2 
kisumu_data = kisumu_data %>%
  mutate(orphan = if_else(dborphan == "double orphan", 2, 
                          if_else(dborphan == "single orphan", 1,
                                  if_else(dborphan == "parents living", 0, 3)))) %>%
  filter(orphan != 3)

#Electricity at Home. Use variable elec: 0 = no electricity, 1 = electricity 
kisumu_data = kisumu_data %>%
  mutate(elec = if_else(electricity == "yes", 1, 
                       if_else(electricity == "no", 0, 
                               if_else(is.na(electricity) == TRUE, 2, 2)))) %>%
  filter(elec != 2)


kisumu_filtered = kisumu_data %>%
  filter(ever.homeless == "yes") %>%
  select(subject.identification.number, orphan, onstrt_new, edatt_cat_new, age, elec) %>%
  na.omit(kisumu_data)

#kisumu_filtered = kisumu_filtered %>% mutate(age_centered = age - 17)

###############################
### 1st Logistic Regression ###
###############################

# PAIRWISE COMPARISONS


# Education and Length of Time on Street (unadjusted)
# p-value: 0.522 
edu.v.timestreet = glm(onstrt_new ~ edatt_cat_new, family = binomial, data = kisumu_filtered)
summary(edu.v.timestreet)

# Orphan Status & Time on street (unadjusted )
# p-value: 0.03823
orphan.v.timestreet<- glm(onstrt_new ~ orphan,
                         family=binomial(link='logit'), data=kisumu_filtered)
summary(orphan.v.timestreet)

# Age and Length on Time on Street (unadjusted)
# p-value: 8.98e-07
age.v.timestreet = glm(onstrt_new ~ age, family = binomial, data = kisumu_filtered)
summary(age.v.timestreet)

# Electricity at home and length of time on street (unadjusted)
# p-vlue: 0.434
elec.v.timestreet = glm(onstrt_new ~ elec,
                   family=binomial(link='logit'), data=kisumu_filtered)
summary(elec.v.timestreet)

# CREATING MODELS
#For each covariate with a p-value < .2 add to model

# Comparing Models w/ and without Orphan
model1 <- glm(onstrt_new ~ edatt_cat_new + orphan + age, 
              family=binomial(link='logit'), data=kisumu_filtered)
summary(model1)


model2 <- glm(onstrt_new ~ edatt_cat_new + age,
                     family=binomial(link='logit'), data=kisumu_filtered)
summary(model2)



# LIKELIHOOD RATIO TEST
lrtest(model1, model2)
# P-value: 0.1106
# We fail to reject the null that the simpler is better than the complex
# Therefore, we can opt to use the model with electricity (model 2)



# ASSESSING COLLINEARITY
vars <- kisumu_filtered %>%
  dplyr::select(onstrt_new, edatt_cat_new, age, orphan, elec)

cor(vars)

###############################
### 2nd Logistic Regression ###
###############################

# Interaction between age and electricity 
# No interaction - p-value of 0.68
age_elec = glm(onstrt_new ~ edatt_cat_new + age + elec + age*elec, 
               family = binomial, data = kisumu_filtered)

summary(age_elec)

# Interaction between education and age 
# No interaction - p-value of 0.68
age_edu = glm(onstrt_new ~ edatt_cat_new + age + elec + age*edatt_cat_new, 
               family = binomial, data = kisumu_filtered)

summary(age_elec)


## QUESTION FOR SUZANNE: WE HAVE NO INTERACTION TERMS. YAY? NAY? SHOULD WE LOOK AT MORE COVARIATES.
# ANSWER: ITS OKAY! 

# ASSESS COLLINEARITY
# Electricity and the interaction term are highly correlated (0.98887291)
#vars_int = kisumu_filtered %>%
  #mutate(int = elec*age_centered) %>%
  #select(age_centered, elec, int)

#cor(vars_int)

# LIKELIHOOD RATIO TEST 
# Comparing interaction model with non-interaction model
# We obtain a p-value of 0.0407, can reject the null hypothesis that the simpler model is better. 
# We go with the model with the interaction. 

#lrtest(elec_age, model1)

############################
### Goodness of Fit Test ###
############################

# GOODNESS OF FIT TEST

hoslem.test(kisumu_filtered$glue, fitted(model1), g = 5)
#  Null hypothesis is that the model sufficiently fits my data
# Stick with around five quantiles




