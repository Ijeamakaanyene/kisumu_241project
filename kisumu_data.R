#####################
### Packages      ###
#####################

library(dplyr)


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
  na.omit(kisumu_data)

kisumu_filtered = kisumu_filtered %>% mutate(age_centered = age - 17)

###############################
### 1st Logistic Regression ###
###############################
#Length of Time (<1 yr vs. > 1 yr) and Glue Sniffing (unadjusted)

timestreet.v.glue <- glm(formula=glue.ever ~ onstrt_new,
                         family=binomial(link='logit'), data=kisumu_filtered)
summary(timestreet.v.glue)

#Our coefficient estimate is 0.02469. 
#At an alpha of 0.05, we fail to reject the null with a p-value of 0.971

#Education and Glue Sniffing

education.v.glue <- glm(formula=glue.ever ~ edatt_cat_new,
                        family=binomial(link='logit'), data=kisumu_filtered)

summary(education.v.glue)
#Our coefficient estimate for education is 0.2742. At an alpha of 0.05, we fail to reject the 
#null with a p-value of .615

#Age and Glue Sniffing

age.v.glue <- glm(formula=glue.ever ~ age,
                  family=binomial(link='logit'), data=kisumu_filtered)
summary(age.v.glue)

#Our coefficient estimate for age is 0.2885. At an alpha of 0.05, we
#reject the null with a p-value of 0.0442.

#Electricity at home and glue sniffing 

elec.v.glue <- glm(formula=glue.ever ~ elec,
                   family=binomial(link='logit'), data=kisumu_filtered)
summary(elec.v.glue)

#Our coefficient estimate for electricity is -.9869. At an alpha of 0.05, we fail to 
#reject the null with a p-value of 0.106.

#For each covariate with a p-value < .2 add to model#

beautifulmodelone <- glm(formula=glue.ever ~ age + elec,
                         family=binomial(link='logit'), data=kisumu_filtered)
summary(beautifulmodelone)


###############################
### 2nd Logistic Regression ###
###############################

# Interaction between length on street and education
# No significant interaction
edu_length_time_lm = glm(glue.ever ~ onstrt_new + edatt_cat_new + edatt_cat_new * onstrt_new, family = binomial(link = "logit"), data = kisumu_filtered)
summary(edu_length_time_lm)

# Interaction between length on street and age
# No significant interaction
age_length_time = glm(glue.ever ~ onstrt_new + age + onstrt_new * age, family = binomial(link = "logit"), data = kisumu_filtered)
summary(age_length_time)

# Interaction between length on street and electricity
# No significant interaction
elec_length_time = glm(glue.ever ~ onstrt_new + elec + elec * onstrt_new, family = binomial(link = "logit"), data = kisumu_filtered)
summary(elec_length_time)

# Interaction between age and electricity
# Interesting interaction / p-value of 0.0536
elec_age = glm(glue.ever ~ onstrt_new + elec + age + elec * age, family = binomial(link = "logit"), data = kisumu_filtered)
summary(elec_age)

# Interaction between age and education
# No significant interaction
age_edu = glm(glue.ever ~ onstrt_new + age + edatt_cat_new + age * edatt_cat_new, family = binomial(link = "logit"), data = kisumu_filtered)
summary(age_edu)

# Assess collinearity
# Electricity and the interaction term are highly correlated (0.98887291)
vars_int = kisumu_filtered %>%
  mutate(int = elec*age_centered) %>%
  select(age_centered, elec, int)

cor(vars_int)

# Centering age (see above)
# Median age = 17
summary(kisumu_filtered$age)



