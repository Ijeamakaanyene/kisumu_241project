#####################
### Packages      ###
#####################

library(dplyr)
library(lmtest)
library(ResourceSelection)


#####################
### Data Cleaning ###
#####################

## Recommendations - convert data file into a csv on your computer. Then update the below code with your file path string
kisumu_data = read.csv("/Users/emilyliu/Desktop/Spring_2019/241 Stats/kisumu_241project/Kisumu Street Youth Seroprevalence Data 9_28_2015.csv", header = TRUE)

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
  mutate(glue = if_else(glue.ever == "yes", 1, if_else(glue.ever == "no", 0, 2))) %>% na.omit(kisumu_data) %>% 
  filter(glue != 2)

#kisumu_filtered = kisumu_filtered %>% mutate(age_centered = age - 17)

###############################
### 1st Logistic Regression ###
###############################

# PAIRWISE COMPARISONS

# Length of Time (<1 yr vs. > 1 yr) and Glue Sniffing (unadjusted)
# p-value: 0.0718

timestreet.v.glue <- glm(formula=glue.ever ~ onstrt_new,
                         family=binomial(link='logit'), data=kisumu_filtered)
summary(timestreet.v.glue)


# Education and Glue Sniffing
# p-value: 0.00292

education.v.glue <- glm(formula=glue.ever ~ edatt_cat_new,
                        family=binomial(link='logit'), data=kisumu_filtered)

summary(education.v.glue)

#Age and Glue Sniffing
# p-value: 0.69

age.v.glue <- glm(formula=glue.ever ~ age,
                  family=binomial(link='logit'), data=kisumu_filtered)
summary(age.v.glue)

# Electricity at home and glue sniffing 
# p-vlue: 0.0864

elec.v.glue <- glm(formula=glue.ever ~ elec,
                   family=binomial(link='logit'), data=kisumu_filtered)
summary(elec.v.glue)

# CREATING MODELS
#For each covariate with a p-value < .2 add to model

model1 <- glm(formula=glue.ever ~ onstrt_new + edatt_cat_new, 
              family=binomial(link='logit'), data=kisumu_filtered)
summary(model1)

model2 <- glm(formula=glue.ever ~ onstrt_new + elec + edatt_cat_new,
                     family=binomial(link='logit'), data=kisumu_filtered)
summary(model2)

# LIKELIHOOD RATIO TEST
lrtest(model1, model2)

# We fail to reject the null that more complex model is better than the simpler model 
# Therefore, we can opt to use the model with age (model 1)

# ASSESSING COLLINEARITY
vars <- kisumu_filtered %>%
  dplyr::select(onstrt_new, edatt_cat_new)

cor(vars)

###############################
### 2nd Logistic Regression ###
###############################

# Interaction between length on street and age
# No significant interaction
age_length_time = glm(glue.ever ~ onstrt_new + age_centered + onstrt_new * age_centered, 
                      family = binomial(link = "logit"), data = kisumu_filtered)

summary(age_length_time)

# Interaction between length on street and electricity
# No significant interaction
elec_length_time = glm(glue.ever ~ onstrt_new + elec + elec * onstrt_new, 
                       family = binomial(link = "logit"), data = kisumu_filtered)

summary(elec_length_time)

# Interaction between age and electricity
# Interesting interaction / p-value of 0.0536
elec_age = glm(glue.ever ~ onstrt_new + elec + age_centered + elec * age_centered, 
               family = binomial(link = "logit"), data = kisumu_filtered)

summary(elec_age)

# ASSESS COLLINEARITY
# Electricity and the interaction term are highly correlated (0.98887291)
vars_int = kisumu_filtered %>%
  mutate(int = elec*age_centered) %>%
  select(age_centered, elec, int)

cor(vars_int)

# LIKELIHOOD RATIO TEST 
# Comparing interaction model with non-interaction model
# We obtain a p-value of 0.0407, can reject the null hypothesis that the simpler model is better. 
# We go with the model with the interaction. 

lrtest(elec_age, model1)

# GOODNESS OF FIT TEST

hoslem.test(kisumu_filtered$glue.ever, fitted(elec_age))




