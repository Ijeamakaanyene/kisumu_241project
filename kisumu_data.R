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
kisumu_data = read.csv("/Users/emilyliu/Desktop/Spring_2019/241 Stats/kisumu_241project/Kisumu Street Youth Seroprevalence Data 9_28_2015.csv", header = TRUE)

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

kisumu_data$orphan = as.factor(kisumu_data$orphan)

#Electricity at Home. Use variable elec: 0 = no electricity, 1 = electricity 
#kisumu_data = kisumu_data %>%
#  mutate(elec = if_else(electricity == "yes", 1, 
#                       if_else(electricity == "no", 0, 
#                               if_else(is.na(electricity) == TRUE, 2, 2)))) %>%
#  filter(elec != 2)

#Family wealth. 
kisumu_data = kisumu_data %>%
  mutate(fam.wealth = if_else(family.wealth == "low", 2, 
                          if_else(family.wealth == "middle", 1,
                                  if_else(family.wealth == "high", 0, 3)))) %>% 
  filter(fam.wealth != 3)

kisumu_data$fam.wealth = as.factor(kisumu_data$fam.wealth)

kisumu_data$survival.activities = as.factor(kisumu_data$survival.activities)

kisumu_filtered = kisumu_data %>%
  filter(ever.homeless == "yes") %>%
  select(subject.identification.number, orphan, onstrt_new, survival.activities, fam.wealth, edatt_cat_new, age) %>%
  na.omit(kisumu_data)

#kisumu_filtered = kisumu_filtered %>% mutate(age_centered = age - 17)

###############################
### 1st Logistic Regression ###
###############################

# PAIRWISE COMPARISONS

# Survival Activities and Length of Time on Street (unadjusted)
# p-value: 0.5541 
sur.v.timestreet = glm(onstrt_new ~ survival.activities, family = binomial, data = kisumu_filtered)
summary(sur.v.timestreet)

# Family Wealth and Length of Time on Street (unadjusted)
# p-value: 0.1382 
wealth.v.timestreet = glm(onstrt_new ~ fam.wealth, family = binomial, data = kisumu_filtered)
summary(wealth.v.timestreet)

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
model1 <- glm(onstrt_new ~ edatt_cat_new + orphan + age + fam.wealth, 
              family=binomial(link='logit'), data=kisumu_filtered)
summary(model1)


model2 <- glm(onstrt_new ~ edatt_cat_new + orphan + age,
                     family=binomial(link='logit'), data=kisumu_filtered)
summary(model2)



# LIKELIHOOD RATIO TEST
lrtest(model1, model2)
# P-value: 0.01426
# We reject the null that the simpler is better than the complex
# We will use MODEL 1


# ASSESSING COLLINEARITY
vars <- kisumu_filtered %>%
  dplyr::select(onstrt_new, edatt_cat_new, age)

cor(vars)

###############################
### 2nd Logistic Regression ###
###############################

# Interaction between education and age
# No interaction - p-value of 0.33679
age_edu = glm(onstrt_new ~ edatt_cat_new + age + age*edatt_cat_new, 
               family = binomial, data = kisumu_filtered)

summary(age_edu)

# Interaction between education and orphan 
# No interaction - p-value of 0.2460
orphan_edu = glm(onstrt_new ~ edatt_cat_new + age + orphan + edatt_cat_new*orphan, 
               family = binomial, data = kisumu_filtered)

summary(orphan_edu)

# Interaction between education and family wealth 
# No interaction - p-value of 0.46858
fam_edu = glm(onstrt_new ~ edatt_cat_new + age + fam.wealth + fam.wealth*edatt_cat_new, 
               family = binomial, data = kisumu_filtered)

summary(fam_edu)

# Interaction between orphan and family wealth 
# No interaction - p-value of 0.46858
inc_orphan = glm(onstrt_new ~ edatt_cat_new + age + fam.wealth + orphan + fam.wealth*orphan, 
              family = binomial, data = kisumu_filtered)

summary(inc_orphan)

# ASSESS COLLINEARITY
vars_int = kisumu_filtered %>%
  select(edatt_cat_new, age)
cor(vars_int)


# LIKELIHOOD RATIO TEST 
# Comparing interaction model with non-interaction model
# We obtain a p-value of 0.0407, can reject the null hypothesis that the simpler model is better. 
# We go with the model with the interaction. 

#lrtest(age_edu, model2)


############################
### Goodness of Fit Test ###
############################

# GOODNESS OF FIT TEST

hoslem.test(kisumu_filtered$edatt_cat_new, fitted(model1), g = 5)



#  Null hypothesis is that the model sufficiently fits my data
# Stick with around five quantiles

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




test <- kisumu_filtered %>%
  dplyr::select(elec, edatt_cat_new, age, elec)

cor(test)



