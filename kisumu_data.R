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


# Education and Length of Time on Street (unadjusted)
edu.v.timestreet = glm()








# Length of Time (<1 yr vs. > 1 yr) and Glue Sniffing (unadjusted)
# p-value: 0.0663

timestreet.v.glue <- glm(formula=glue ~ onstrt_new,
                         family=binomial(link='logit'), data=kisumu_filtered)
summary(timestreet.v.glue)


# Education and Glue Sniffing
# p-value: 0.00415

education.v.glue <- glm(formula=glue ~ edatt_cat_new,
                        family=binomial(link='logit'), data=kisumu_filtered)

summary(education.v.glue)

#Age and Glue Sniffing
# p-value: 0.667

age.v.glue <- glm(formula=glue ~ age,
                  family=binomial(link='logit'), data=kisumu_filtered)
summary(age.v.glue)

# Electricity at home and glue sniffing 
# p-vlue: 0.133

elec.v.glue <- glm(formula=glue ~ elec,
                   family=binomial(link='logit'), data=kisumu_filtered)
summary(elec.v.glue)

# CREATING MODELS
#For each covariate with a p-value < .2 add to model

model1 <- glm(formula=glue ~ onstrt_new + elec+ edatt_cat_new, 
              family=binomial(link='logit'), data=kisumu_filtered)
summary(model1)

# Both Time on Street and Electricity have p-values > 0.05

model2 <- glm(formula=glue ~ onstrt_new + edatt_cat_new,
                     family=binomial(link='logit'), data=kisumu_filtered)
summary(model2)

# Other Models
#model3 <- glm(formula=glue ~ elec + edatt_cat_new,
                  #family=binomial(link='logit'), data=kisumu_filtered)
#summary(model3)
#model4 <- glm(formula=glue ~ edatt_cat_new,
              #family=binomial(link='logit'), data=kisumu_filtered)
#summary(model4)


# LIKELIHOOD RATIO TEST
lrtest(model1, model2)


# We fail to reject the null that the simpler is better than complex. 
# Therefore, we can opt to use the model with electricity and time on the street (model 1)

# ASSESSING COLLINEARITY
vars <- kisumu_filtered %>%
  dplyr::select(onstrt_new, edatt_cat_new)

cor(vars)

###############################
### 2nd Logistic Regression ###
###############################

# Interaction between time on street and education 
# No Interaction - p-value of 0.919
length_edu = glm(glue ~ onstrt_new + edatt_cat_new + onstrt_new * edatt_cat_new, 
                 family = binomial(link = "logit"), data = kisumu_filtered)
summary(length_edu)


# Interaction between length on street and age
# No significant interaction
#age_length_time = glm(glue.ever ~ onstrt_new + age + onstrt_new * age, 
                      #family = binomial(link = "logit"), data = kisumu_filtered)

#summary(age_length_time)

# Interaction between length on street and electricity
# No significant interaction
elec_length_time = glm(glue ~ onstrt_new + elec + elec * onstrt_new, 
                       family = binomial(link = "logit"), data = kisumu_filtered)

summary(elec_length_time)

# Interaction between age and electricity
# Interesting interaction / p-value of 0.0536
#elec_age = glm(glue.ever ~ onstrt_new + elec + age_centered + elec * age_centered, 
               #family = binomial(link = "logit"), data = kisumu_filtered)

#summary(elec_age)

# Interaction between education and electricity
# No significant interaction
edu_elect = glm(glue ~ onstrt_new + elec + edatt_cat_new + elec*edatt_cat_new, 
                family = binomial(link = "logit"), data = kisumu_filtered)
summary(edu_elect)

## QUESTION FOR SUZANNE: WE HAVE NO INTERACTION TERMS. YAY? NAY? SHOULD WE LOOK AT MORE COVARIATES.
# CAN WE KEEP AGE IS A CONFOUNDER BASED OFF OF OUR DAG- EVEN IF THE P-VALUE IS GREATER THAN .05

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
#  Null hypothesis is that the model sufficients fits my data
# Stick with five 




