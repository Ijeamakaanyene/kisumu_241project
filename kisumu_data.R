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
kisumu_data = read.csv("/Users/ijeamakaanyene/Desktop/Berkeley Coursework/Spring_2019/Statistical Analysis of Categorical Data/Assignments/Final Projects/Kisumu Street Youth Seroprevalence Data 9_28_2015.csv", header = TRUE)

## Exposure: Education. Use variable edatt_cat_new: 0 is <= grade 5, 1 is > grade 5

## Outcome: Length of Time on Street. Use variable onstrt_new: 0 - <1yr, 1 - >=1 yr

## Confounders: 
# Glue Sniffing. Use variable glue: 0 - no, 1 - yes

#Age. Use variable age (keeping continuous)

#Electricity at Home. Use variable elec: 0 = no electricity, 1 = electricity 
kisumu_data = kisumu_data %>%
  mutate(elec = if_else(electricity == "yes", 1, 
                       if_else(electricity == "no", 0, 
                               if_else(is.na(electricity) == TRUE, 2, 2))))


kisumu_filtered = kisumu_data %>%
  filter(ever.homeless == "yes") %>%
  select(subject.identification.number, glue.ever, onstrt_new, edatt_cat_new, age, elec) %>%
  mutate(glue = if_else(glue.ever == "yes", 1, if_else(glue.ever == "no", 0, 2))) %>% 
  na.omit(kisumu_data) %>% 
  filter(glue != 2, elec != 2)

#kisumu_filtered = kisumu_filtered %>% mutate(age_centered = age - 17)

###############################
### 1st Logistic Regression ###
###############################

# PAIRWISE COMPARISONS

# Test out ethnicity as a confounder. 

# Education and Length of Time on Street (unadjusted)
# p-value: 0.309 
edu.v.timestreet = glm(onstrt_new ~ edatt_cat_new, family = binomial, data = kisumu_filtered)
summary(edu.v.timestreet)

# Length of Time (<1 yr vs. > 1 yr) and Glue Sniffing (unadjusted)
# p-value: 0.0663
timestreet.v.glue <- glm(onstrt_new ~ glue,
                         family=binomial(link='logit'), data=kisumu_filtered)
summary(timestreet.v.glue)

# Age and Length on Time on Street (unadjusted)
# p-value: 2.39e-07
age.v.timestreet = glm(onstrt_new ~ age, family = binomial, data = kisumu_filtered)
summary(age.v.timestreet)

# Electricity at home and length of time on street (unadjusted)
# p-vlue: 0.422
elec.v.timestreet = glm(onstrt_new ~ elec,
                   family=binomial(link='logit'), data=kisumu_filtered)
summary(elec.v.timestreet)

# CREATING MODELS
#For each covariate with a p-value < .2 add to model

# Comparing Models w/ and without Glue
model1 <- glm(onstrt_new ~ edatt_cat_new + glue + age, 
              family=binomial(link='logit'), data=kisumu_filtered)
summary(model1)


model2 <- glm(onstrt_new ~ edatt_cat_new + age,
                     family=binomial(link='logit'), data=kisumu_filtered)
summary(model2)


#Comparing Models w/ and without Electricity 
model3 = glm(onstrt_new ~ edatt_cat_new + age + elec, 
             family = binomial, data = kisumu_filtered)
summary(model3)

model4 = glm(onstrt_new ~ edatt_cat_new + age, 
             family = binomial, data = kisumu_filtered)
summary(model4)

# LIKELIHOOD RATIO TEST
lrtest(model1, model2)
# We fail to reject the null that the simpler is better than the complex
# Therefore, we can opt to use the model with glue and age (model 2)

lrtest(model3, model4)
# We fail to reject the null that the simpler is better than complex. 
# Therefore, we can opt to use the model with electrity and age (model 4)

# ASSESSING COLLINEARITY
vars <- kisumu_filtered %>%
  dplyr::select(onstrt_new, edatt_cat_new, age, glue)

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




