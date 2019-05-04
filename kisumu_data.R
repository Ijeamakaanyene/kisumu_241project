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
kisumu_data = read.csv("/Users/ijeamakaanyene/kisumu_241project/Kisumu Street Youth Seroprevalence Data 9_28_2015.csv", header = TRUE)

## Exposure: Education. Use variable edatt_cat_new: 0 is <= grade 5, 1 is > grade 5
## Outcome: Length of Time on Street. Use variable onstrt_new: 0 - <1yr, 1 - >=1 yr

## Confounders: AGE, ORPHAN STATUS, FAMILY WEALTH

# AGE: Use variable age (keeping continuous)

# ORPHAN
# Create variable with orphan (either parent) -> no = 0, single = 1, double = 2 
kisumu_data = kisumu_data %>%
  mutate(orphan = if_else(dborphan == "double orphan", 2, 
                          if_else(dborphan == "single orphan", 1,
                                  if_else(dborphan == "parents living", 0, 3)))) %>% 
  filter(orphan != 3)

# Convert orphan into factor
kisumu_data$orphan = as.factor(kisumu_data$orphan)

# FAMILY WEALTH
# Variable called fam.wealth -> low = 2, middle = 1, high = 0
kisumu_data = kisumu_data %>%
  mutate(fam.wealth = if_else(family.wealth == "low", 2, 
                          if_else(family.wealth == "middle", 1,
                                  if_else(family.wealth == "high", 0, 3)))) %>% 
  filter(fam.wealth != 3 & edatt_cat_new != 2)

# Convert family wealth into a factor 
kisumu_data$fam.wealth = as.factor(kisumu_data$fam.wealth)

# Subset dataset to variables of interest

kisumu_filtered = kisumu_data %>%
  filter(ever.homeless == "yes") %>%
  select(subject.identification.number, orphan, onstrt_new, survival.activities, fam.wealth, edatt_cat_new, age) %>%
  na.omit(kisumu_data)

###############################
### 1st Logistic Regression ###
###############################

# Intercept
# Odds of Disease given no exposure
# Unexposed group = <= grade 5
# Diseased group = >=1 year on street
n_distinct(filter(kisumu_filtered, edatt_cat_new ==0 & onstrt_new ==1))
n_distinct(filter(kisumu_filtered, edatt_cat_new == 0))
prob <- 97/125
den <- 1-prob
odds <- log(prob/den)
odds


# PAIRWISE COMPARISONS

# Survival Activities and Length of Time on Street (unadjusted)
# p-value: 0.3144 
sur.v.timestreet = glm(onstrt_new ~ survival.activities, family = binomial, data = kisumu_filtered)
summary(sur.v.timestreet)

# Family Wealth and Length of Time on Street (unadjusted)
# p-value: 0.1382 
wealth.v.timestreet = glm(onstrt_new ~ fam.wealth, family = binomial, data = kisumu_filtered)
summary(wealth.v.timestreet)

# Education and Length of Time on Street (unadjusted)
# p-value: 0.0849
edu.v.timestreet = glm(onstrt_new ~ edatt_cat_new, family = binomial, data = kisumu_filtered)
summary(edu.v.timestreet)

# Orphan Status & Time on street (unadjusted )
# p-value: 0.03823
orphan.v.timestreet<- glm(onstrt_new ~ orphan,
                         family=binomial(link='logit'), data=kisumu_filtered)
summary(orphan.v.timestreet)

# Age and Length on Time on Street (unadjusted)
# p-value: 9.05e-07
age.v.timestreet = glm(onstrt_new ~ age, family = binomial, data = kisumu_filtered)
summary(age.v.timestreet)

# Electricity at home and length of time on street (unadjusted)
# p-vlue: 0.434
#elec.v.timestreet = glm(onstrt_new ~ elec,
                   #family=binomial(link='logit'), data=kisumu_filtered)
#summary(elec.v.timestreet)

# CREATING MODELS
# For each covariate with a p-value < .2 add to model
# AGE, ORPHAN, FAMILY WEALTH


# COMPARING MODELS 

# Model 1: Full model - education, orphan, age, family wealth
model1 <- glm(onstrt_new ~ edatt_cat_new + orphan + age + fam.wealth, 
              family=binomial(link='logit'), data=kisumu_filtered)
summary(model1)

# Model 2: Simple model - education, orphan, age
model2 <- glm(onstrt_new ~ edatt_cat_new + orphan + age,
                     family=binomial(link='logit'), data=kisumu_filtered)
summary(model2)
exp(model2$coefficients)
exp(confint(model2, level = 0.95))


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
# Interaction - p-value of 0.15738 
age_edu = glm(onstrt_new ~ edatt_cat_new + age + fam.wealth + orphan + age*edatt_cat_new, 
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

lrtest(model1, age_edu)

# LIKELIHOOD RATIO TEST 
# Because we have no significant interactions, we do not have to conduct a LRT. 
# We default to MODEL 1. 

############################
### Goodness of Fit Test ###
############################

# GOODNESS OF FIT TEST

hoslem.test(kisumu_filtered$edatt_cat_new, fitted(age_edu), g = 5)

# p-value < 2.2e-16
# We reject the null hypothesis that our model is a good fit. 
# Our model is a terrible fit...



############################
### NOTES                ###
############################

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



