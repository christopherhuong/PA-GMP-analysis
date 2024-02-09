# 
# Analysis code for 
# "Associations between physical activity and subcategories of mental health: A propensity score analysis among a global sample of 341,956 adults"
# Christopher Huong & Denver Brown 2023
# 
# Simplified and annotated for resubmission 12/10/2023

library(tidyverse)
library(naniar)
library(WeightIt)
library(survey)
library(cobalt)
library(MatchThem) 
library(mice)
library(miceadds)
library(knitr)

# Load in raw data, check variable names, and check range of response dates

load('rawdata.RData')

names(rawdata)
rawdata$Submit.Date..UTC. <- as.Date(rawdata$Submit.Date..UTC.)
range(rawdata$Submit.Date..UTC.)

# Select DVs, predictor, and covariates, rename variables
cleandata <- rawdata %>% 
  select(Overall.MHQ, Cognition, Adaptability...Resilence, Drive...Motivation, Mood...Outlook, Social...Self, Mind.Body.Connection, Frequency.of.doing.exercise, Age, Biological.Sex, Gender.Identity, Country, Ethnicity, Education, Employment, Current.Family.Situation, Frequency.of.Socializing, Frequency.of.getting.a.good.nights.sleep, Presence.Absence.of.Diagnosed.Medical.Disorder, Mental.Health.Treatment.Status, Childhood.traumas, Adult.traumas)

names(cleandata) <- c('mhq', 'cog', 'adaptresil', 'drivemotiv', 'moodoutlook', 'socialself', 'mindbody', 'PA', 'age', 'sex', 'genderidentity', 'country', 'ethnicity', 'education', 'employment', 'relationship', 'socialize', 'sleep', 'meddiagnosis', 'mhseeking', 'childtrauma', 'adulttrauma')

# Preprocessing variables
summary(cleandata$PA)
# remove 2 non-english responses
cleandata <- cleandata %>% subset(PA == "Every day" | PA == "Few days a week" | PA == "Less than once a week" | PA == "Once a week" | PA == "Rarely/Never")
# recode to binary treatment
cleandata <- cleandata %>% mutate(PA = if_else(PA == "Rarely/Never", 0L, 1L))
table(cleandata$PA)
# Age
summary(cleandata$age)
cleandata$age <- as.numeric(cleandata$age)
# Sex
summary(cleandata$sex)
cleandata$sex <- factor(cleandata$sex, order=F)
# Gender
summary(cleandata$genderidentity)
cleandata$genderidentity <- factor(cleandata$genderidentity, order=F)
# Country
cleandata$country <- as.integer(cleandata$country)
summary(cleandata$country)
# Education
cleandata <- cleandata %>%              
  mutate(education = case_when(education == "Primary Education" ~ "less.hs",
                               education == "Some High School" ~ "less.hs",
                               education == "MÃ©dio completo" ~ "less.hs",
                               education == "High School" ~ "hs",
                               education == "Ensino tÃ©cnico" ~ "vocational",
                               education == "Ensino profissionalizante" ~ "vocational",
                               education == "Vocational certification" ~ "vocational",
                               education == "Associateâ€™s Degree" ~ "assoc.deg",
                               education == "Bachelor's Degree" ~ "bach.deg",
                               education == "Master's Degree" ~ "grad.deg",
                               education == "PhD" ~ "grad.deg",
                               education == "J.D" ~ "grad.deg",
                               education == "J.D. (Direito)" ~ "grad.deg",
                               education == "M.D." ~ "grad.deg",
                               education == "M.D. (Medicina)" ~ "grad.deg",
                               education == "Other" ~ "other",
                               education == "Prefer not to say" ~ "Prefer not to say"))
cleandata$education <- factor(cleandata$education, order=F)
summary(cleandata$education)
# Employment
cleandata$employment <- factor(cleandata$employment, order = F)
summary(cleandata$employment)
# Relationship status
cleandata$relationship <- factor(cleandata$relationship, order = F)
summary(cleandata$relationship)
# Socialize
cleandata$socialize <- factor(cleandata$socialize, order = F, levels = c("Rarely/Never", "1-3 times a month", "Once a week", "Several days a week"))
summary(cleandata$socialize)
# Sleep
cleandata$sleep <- factor(cleandata$sleep, order = F, levels = c("Hardly ever", "Some of the time", "Most of the time", "All of the time"))
summary(cleandata$sleep)
# Medical Diagnosis
cleandata$meddiagnosis <- factor(cleandata$meddiagnosis, order = F)
summary(cleandata$meddiagnosis)
# Seeking mental health treatment
cleandata$mhseeking <- factor(cleandata$mhseeking, order = F)
summary(cleandata$mhseeking)
# Childhood and adult trauma
cleandata <- cleandata %>%
  mutate(childtrauma = if_else(childtrauma =="|I did not experience any of the above during my childhood" | childtrauma == "|None of the above", 0L,1L),
         adulttrauma = if_else(adulttrauma == "|I did not experience any of the above" | adulttrauma == "|None of the above", 0L,1L))

table(cleandata$childtrauma)
table(cleandata$adulttrauma)

# Code and check missingness
colSums(is.na(cleandata)) #check NAs per var
apply(cleandata == "", 2, sum)
apply(cleandata == "Prefer not to say", 2, sum)
cleandata[cleandata == ""] <- NA
cleandata[cleandata == "Prefer not to say"] <- NA 

colSums(is.na(cleandata)) #check NAs per var

#percent missingness by var
percentmiss <- function(x){
  sum(is.na(x)) / length(x) * 100} 

apply(cleandata, 2, percentmiss)

# "Data inspection revealed considerable missingness for ethnicity (84.2%) and gender identity (98.5%) due to only having been included on surveys for individuals who reported residing in certain countries and therefore these variables were excluded"

cleandata <- cleandata %>% select(-c(ethnicity, genderidentity))
cleandata <- droplevels(cleandata)  #get rid of "Prefer not to say" factor levels
summary(cleandata)

# save(cleandata, file="cleandata.RData")


# Propensity score weighting using GBM
# Check pre-weighting covariate balance across "treatment" groups
load("cleandata.RData")

bal.tab(PA ~ age + sex + education + employment + relationship + socialize + sleep + meddiagnosis + mhseeking + childtrauma + adulttrauma,
        data = cleandata,
        s.d.denom = "pooled",
        stats = c("m", "ks"),
        thresholds = c(m = .01))



# (1a) Propensity score weighting usingnd binary treatment based on the ATC estimand. 
gbm_weights <- weightit(PA ~ age + sex + education + employment + relationship + socialize 
                        + sleep + meddiagnosis + mhseeking + childtrauma + adulttrauma,
                        data = cleandata,
                        method = "gbm",
                        estimand = "ATC",
                        trim.at = 0.99,
                        distribution = "bernoulli")

save(gbm_weights, file = "gbm_weights.RData")
load("gbm_weights.RData")

# Covariate balancing diagnostics
gbm_weights
summary(gbm_weights)


bal.tab(gbm_weights$treat ~ gbm_weights$covs,
        data = gbm_weights,
        weights = gbm_weights$weights,
        stats = c("m"),
        s.d.denom = "control",
        thresholds = c(m = .01))

love.plot(gbm_weights, binary = "std", var.order = "un", stats = "m",
          thresholds = .01) + theme(legend.position = "top")


# (1b) Estimating main treatment effect using weighted GLM, clustered by country
des <- svydesign(ids = ~country, weights = gbm_weights$weights, data = cleandata)
# 
# 
# 
mhq <- svyglm(mhq ~ PA, design = des, family = gaussian())

summary(mhq)$coefficients[2,1:2] %>% round(2) %>% kable()
# |           |     x|
#   |:----------|-----:|
#   |Estimate   | 17.41|
#   |Std. Error |  1.41|
confint(mhq)[2,] %>% round(2)
# 2.5 % 97.5 % 
#   14.62  20.20 

# (1c) Cohen's d calculation

# Retrieve standard deviations for active and inactive
n1 = 135525 
n2 = 206431 
n = n1+n2

sd1 = sd(cleandata %>% filter(PA == 0) %>% pull(mhq))
sd2 = sd(cleandata %>% filter(PA == 1) %>% pull(mhq))
# Pool SDs
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n-2))
cohensd = as.numeric(mhq$coefficients)[2] / sd_pooled
cohensd %>% round(2)

# Repeat for all MHQ subcategories

# Core Cognition ----------------------------------------------------------
cog <- svyglm(cog ~ PA, design = des, family = gaussian())
summary(cog)$coefficients[2,1:2] %>% round(2) %>% kable()
# |           |     x|
#   |:----------|-----:|
#   |Estimate   | 16.00|
#   |Std. Error |  1.24|
confint(cog)[2,] %>% round(2)
# 2.5 % 97.5 % 
#   13.56  18.44 

# Cohens D
sd1 = sd(cleandata %>% filter(PA == 0) %>% pull(cog))
sd2 = sd(cleandata %>% filter(PA == 1) %>% pull(cog))
# Pool SDs
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n-2))
cohensd = as.numeric(cog$coefficients)[2] / sd_pooled
cohensd %>% round(2)


# Adaptability and Resilience ---------------------------------------------
adaptresil <- svyglm(adaptresil ~ PA, design = des, family = gaussian())
summary(adaptresil)$coefficients[2,1:2] %>% round(2) %>% kable()
# |           |     x|
#   |:----------|-----:|
#   |Estimate   | 17.09|
#   |Std. Error |  1.41|
confint(adaptresil)[2,] %>% round(2)
#   2.5 % 97.5 % 
#   14.31  19.86 

# Cohens D
sd1 = sd(cleandata %>% filter(PA == 0) %>% pull(adaptresil))
sd2 = sd(cleandata %>% filter(PA == 1) %>% pull(adaptresil))
# Pool SDs
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n-2))
cohensd = as.numeric(adaptresil$coefficients)[2] / sd_pooled
cohensd %>% round(2)


# Drive and Motivation ----------------------------------------------------

drivemotiv <- svyglm(drivemotiv ~ PA, design = des, family = gaussian())
summary(drivemotiv)$coefficients[2,1:2] %>% round(2) %>% kable()
# |           |     x|
#   |:----------|-----:|
#   |Estimate   | 15.43|
#   |Std. Error |  1.54|
confint(drivemotiv)[2,] %>% round(2)
#   2.5 % 97.5 % 
#   12.39  18.47 

# Cohens D
sd1 = sd(cleandata %>% filter(PA == 0) %>% pull(drivemotiv))
sd2 = sd(cleandata %>% filter(PA == 1) %>% pull(drivemotiv))
# Pool SDs
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n-2))
cohensd = as.numeric(drivemotiv$coefficients)[2] / sd_pooled
cohensd %>% round(2)


# Mood and Outlook --------------------------------------------------------

moodoutlook <- svyglm(moodoutlook ~ PA, design = des, family = gaussian())
summary(moodoutlook)$coefficients[2,1:2] %>% round(2) %>% kable()
# |           |     x|
#   |:----------|-----:|
#   |Estimate   | 14.81|
#   |Std. Error |  1.40|
confint(moodoutlook)[2,] %>% round(2)
#   2.5 % 97.5 % 
#   12.06  17.56 

# Cohens D
sd1 = sd(cleandata %>% filter(PA == 0) %>% pull(moodoutlook))
sd2 = sd(cleandata %>% filter(PA == 1) %>% pull(moodoutlook))
# Pool SDs
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n-2))
cohensd = as.numeric(moodoutlook$coefficients)[2] / sd_pooled
cohensd %>% round(2)


# Social Self -------------------------------------------------------------

socialself <- svyglm(socialself ~ PA, design = des, family = gaussian())
summary(socialself)$coefficients[2,1:2] %>% round(2) %>% kable()
# |           |     x|
#   |:----------|-----:|
#   |Estimate   | 12.53|
#   |Std. Error |  1.47|
confint(socialself)[2,] %>% round(2)
#   2.5 % 97.5 % 
#   9.63  15.44 

# Cohens D
sd1 = sd(cleandata %>% filter(PA == 0) %>% pull(socialself))
sd2 = sd(cleandata %>% filter(PA == 1) %>% pull(socialself))
# Pool SDs
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n-2))
cohensd = as.numeric(socialself$coefficients)[2] / sd_pooled
cohensd %>% round(2)


# Mind-Body Connection ----------------------------------------------------

mindbody <- svyglm(mindbody ~ PA, design = des, family = gaussian())
summary(mindbody)$coefficients[2,1:2] %>% round(2) %>% kable()
# |           |     x|
#   |:----------|-----:|
#   |Estimate   | 18.90|
#   |Std. Error |  1.30|
confint(mindbody)[2,] %>% round(2)
#   2.5 % 97.5 % 
#   16.34  21.46 

# Cohens D
sd1 = sd(cleandata %>% filter(PA == 0) %>% pull(mindbody))
sd2 = sd(cleandata %>% filter(PA == 1) %>% pull(mindbody))
# Pool SDs
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n-2))
cohensd = as.numeric(mindbody$coefficients)[2] / sd_pooled
cohensd %>% round(2)





# (2a) Analysis (1a-1c) on each age group

# cleandata_18to24 <- cleandata %>% filter(age == 1)
# cleandata_25to34 <- cleandata %>% filter(age == 2)
# cleandata_35to44 <- cleandata %>% filter(age == 3)
# cleandata_45to54 <- cleandata %>% filter(age == 4)
# cleandata_55to64 <- cleandata %>% filter(age == 5)
# cleandata_65to74 <- cleandata %>% filter(age == 6)
# cleandata_75to84 <- cleandata %>% filter(age == 7)
# cleandata_85plus <- cleandata %>% filter(age == 8)
# 
# 
# gbm_weights_18to24 <- weightit(PA ~ sex+education+employment+relationship+socialize+sleep+meddiagnosis+mhseeking+childtrauma+adulttrauma,
#                         data = cleandata_18to24,method = "gbm",estimand = "ATC",trim.at = 0.99,distribution = "bernoulli")
# save(gbm_weights_18to24, file = "gbm_weights_18to24.Rdata")
# ###
# gbm_weights_25to34 <- weightit(PA ~ sex+education+employment+relationship+socialize+sleep+meddiagnosis+mhseeking+childtrauma+adulttrauma,
#                                data = cleandata_25to34,method = "gbm",estimand = "ATC",trim.at = 0.99,distribution = "bernoulli")
# save(gbm_weights_25to34, file = "gbm_weights_25to34.Rdata")
# ###
# gbm_weights_35to44 <- weightit(PA ~ sex+education+employment+relationship+socialize+sleep+meddiagnosis+mhseeking+childtrauma+adulttrauma,
#                                data = cleandata_35to44,method = "gbm",estimand = "ATC",trim.at = 0.99,distribution = "bernoulli")
# save(gbm_weights_35to44, file = "gbm_weights_35to44.Rdata")
# ###
# gbm_weights_45to54 <- weightit(PA ~ sex+education+employment+relationship+socialize+sleep+meddiagnosis+mhseeking+childtrauma+adulttrauma,
#                                data = cleandata_45to54,method = "gbm",estimand = "ATC",trim.at = 0.99,distribution = "bernoulli")
# save(gbm_weights_45to54, file = "gbm_weights_45to54.Rdata")
# ###
# gbm_weights_55to64 <- weightit(PA ~ sex+education+employment+relationship+socialize+sleep+meddiagnosis+mhseeking+childtrauma+adulttrauma,
#                                data = cleandata_55to64,method = "gbm",estimand = "ATC",trim.at = 0.99,distribution = "bernoulli")
# save(gbm_weights_55to64, file = "gbm_weights_55to64.Rdata")
# ###
# gbm_weights_65to74 <- weightit(PA ~ sex+education+employment+relationship+socialize+sleep+meddiagnosis+mhseeking+childtrauma+adulttrauma,
#                                data = cleandata_65to74,method = "gbm",estimand = "ATC",trim.at = 0.99,distribution = "bernoulli")
# save(gbm_weights_65to74, file = "gbm_weights_65to74.Rdata")
# ###
# gbm_weights_75to84 <- weightit(PA ~ sex+education+employment+relationship+socialize+sleep+meddiagnosis+mhseeking+childtrauma+adulttrauma,
#                                data = cleandata_75to84,method = "gbm",estimand = "ATC",trim.at = 0.99,distribution = "bernoulli")
# save(gbm_weights_75to84, file = "gbm_weights_75to84.Rdata")
# ###
# gbm_weights_85plus <- weightit(PA ~ sex+education+employment+relationship+socialize+sleep+meddiagnosis+mhseeking+childtrauma+adulttrauma,
#                                data = cleandata_85plus,method = "gbm",estimand = "ATC",trim.at = 0.99,distribution = "bernoulli")
# save(gbm_weights_85plus, file = "gbm_weights_85plus.Rdata")
# ###


# (2b) Estimating MHQ main treatment effect using weighted GLM for each age group
load("cleandata.RData")
load("gbm_weights_18to24.Rdata")
load("gbm_weights_25to34.Rdata")
load("gbm_weights_35to44.Rdata")
load("gbm_weights_45to54.Rdata")
load("gbm_weights_55to64.Rdata")
load("gbm_weights_65to74.Rdata")
load("gbm_weights_75to84.Rdata")
load("gbm_weights_85plus.Rdata")

# Helper function
interact <- function(a,b) {
  des <- svydesign(ids = ~country, weights = a$weights,
                       data = subset(cleandata, age==b))
  
  mhq_interact <- svyglm(mhq ~ PA,
                         design = des, family = gaussian())
}


# MHQ 18 to 24 ------------------------------------------------------------
age18 <- interact(gbm_weights_18to24, 1)
age18_est <- summary(age18)$coefficients[2,1] %>% round(2)
confint(age18) %>% round(2)
age18_se <- summary(age18)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 1) %>% pull(mhq))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 1)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age ==1 ) %>% pull(mhq))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 1)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age18_d = (as.numeric(age18$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_18to24, age18)

#17.01, 1.14, 0.25

# MHQ 25 to 34 ------------------------------------------------------------
age25 <- interact(gbm_weights_25to34, 2)
age25_est <- summary(age25)$coefficients[2,1] %>% round(2)
confint(age25) %>% round(2)
age25_se <- summary(age25)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 2) %>% pull(mhq))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 2)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 2) %>% pull(mhq))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 2)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age25_d = (as.numeric(age25$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_25to34, age25)

#19.19, 1.35, 0.28



# MHQ 35 to 44 ------------------------------------------------------------
age35 <- interact(gbm_weights_35to44, 3)
age35_est <- summary(age35)$coefficients[2,1] %>% round(2)
confint(age35) %>% round(2)
age35_se <- summary(age35)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 3) %>% pull(mhq))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 3)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 3) %>% pull(mhq))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 3)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age35_d = (as.numeric(age35$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_35to44, age35)

#20.09, 1.47, 0.30



# MHQ 45 to 54 ------------------------------------------------------------
age45 <- interact(gbm_weights_45to54, 4)
age45_est <- summary(age45)$coefficients[2,1] %>% round(2)
confint(age45) %>% round(2)
age45_se <- summary(age45)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 4) %>% pull(mhq))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 4)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 4) %>% pull(mhq))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 4)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age45_d = (as.numeric(age45$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_45to54, age45)

#18.47, 1.84, 0.28

# MHQ 55 to 64 ------------------------------------------------------------
age55 <- interact(gbm_weights_55to64, 5)
age55_est <- summary(age55)$coefficients[2,1] %>% round(2)
confint(age55) %>% round(2)
age55_se <- summary(age55)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 5) %>% pull(mhq))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 5)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 5) %>% pull(mhq))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 5)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age55_d = (as.numeric(age55$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_55to64, age55)

#15.46, 1.89, 0.24

# MHQ 65 to 74 ------------------------------------------------------------
age65 <- interact(gbm_weights_65to74, 6)
age65_est <- summary(age65)$coefficients[2,1] %>% round(2)
confint(age65) %>% round(2)
age65_se <- summary(age65)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 6) %>% pull(mhq))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 6)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 6) %>% pull(mhq))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 6)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age65_d = (as.numeric(age65$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_65to74, age65)

#12.25, 1.49, 0.21

# MHQ 75 to 84 ------------------------------------------------------------
age75 <- interact(gbm_weights_75to84, 7)
age75_est <- summary(age75)$coefficients[2,1] %>% round(2)
confint(age75) %>% round(2)
age75_se <- summary(age75)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 7) %>% pull(mhq))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 7)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 7) %>% pull(mhq))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 7)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age75_d = (as.numeric(age75$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_75to84, age75)

#11.27, 1.74, 0.21

# MHQ 85+ ------------------------------------------------------------
age85 <- interact(gbm_weights_85plus, 8)
age85_est <- summary(age85)$coefficients[2,1] %>% round(2)
confint(age85) %>% round(2)
age85_se <- summary(age85)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 8) %>% pull(mhq))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 8)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 8) %>% pull(mhq))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 8)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age85_d = (as.numeric(age85$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_85plus, age85)

#22.83, 5.48, 0.37

# Put the MHQ X Age values in a data frame
age_values <- data.frame(
  outcome = character(),
  estimate = character(),
  "age18.24" = numeric(),
  "age25.34" = numeric(),
  "age35.44" = numeric(),
  "age45.54" = numeric(),
  "age55.64" = numeric(),
  "age65.74" = numeric(),
  "age75.84" = numeric(),
  "age85plus" = numeric()
)

age_values[1,] <- c("MHQ", "ATC", age18_est, age25_est, age35_est, age45_est, age55_est, age65_est, age75_est, age85_est)
age_values[2,] <- c("MHQ", "SE", age18_se, age25_se, age35_se, age45_se, age55_se, age65_se, age75_se, age85_se)
age_values[3,] <- c("MHQ", "D", age18_d, age25_d, age35_d, age45_d, age55_d, age65_d, age75_d, age85_d)


# (2b) Estimating Core Cognition main treatment effect using weighted GLM for each age group
load("gbm_weights_18to24.Rdata")
load("gbm_weights_25to34.Rdata")
load("gbm_weights_35to44.Rdata")
load("gbm_weights_45to54.Rdata")
load("gbm_weights_55to64.Rdata")
load("gbm_weights_65to74.Rdata")
load("gbm_weights_75to84.Rdata")
load("gbm_weights_85plus.Rdata")

# Helper function
interact <- function(a,b) {
  des <- svydesign(ids = ~country, weights = a$weights,
                   data = subset(cleandata, age==b))
  
  mhq_interact <- svyglm(cog ~ PA,
                         design = des, family = gaussian())
}


# COG 18 to 24 ------------------------------------------------------------
age18 <- interact(gbm_weights_18to24, 1)
age18_est <- summary(age18)$coefficients[2,1] %>% round(2)
age18_se <- summary(age18)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 1) %>% pull(cog))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 1)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age ==1 ) %>% pull(cog))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 1)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age18_d = (as.numeric(age18$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_18to24, age18)

#16.76, 0.93, 0.27

# COG 25 to 34 ------------------------------------------------------------
age25 <- interact(gbm_weights_25to34, 2)
age25_est <- summary(age25)$coefficients[2,1] %>% round(2)
age25_se <- summary(age25)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 2) %>% pull(cog))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 2)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 2) %>% pull(cog))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 2)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age25_d = (as.numeric(age25$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_25to34, age25)

#18.10, 1.1, 0.28



# COG 35 to 44 ------------------------------------------------------------
age35 <- interact(gbm_weights_35to44, 3)
age35_est <- summary(age35)$coefficients[2,1] %>% round(2)
age35_se <- summary(age35)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 3) %>% pull(cog))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 3)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 3) %>% pull(cog))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 3)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age35_d = (as.numeric(age35$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_35to44, age35)

#18.60, 1.31, 0.29



# COG 45 to 54 ------------------------------------------------------------
age45 <- interact(gbm_weights_45to54, 4)
age45_est <- summary(age45)$coefficients[2,1] %>% round(2)
age45_se <- summary(age45)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 4) %>% pull(cog))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 4)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 4) %>% pull(cog))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 4)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age45_d = (as.numeric(age45$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_45to54, age45)

#16.51, 1.48, 0.27

# COG 55 to 64 ------------------------------------------------------------
age55 <- interact(gbm_weights_55to64, 5)
age55_est <- summary(age55)$coefficients[2,1] %>% round(2)
age55_se <- summary(age55)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 5) %>% pull(cog))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 5)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 5) %>% pull(cog))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 5)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age55_d = (as.numeric(age55$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_55to64, age55)

#13.54, 1.42, 0.23

# COG 65 to 74 ------------------------------------------------------------
age65 <- interact(gbm_weights_65to74, 6)
age65_est <- summary(age65)$coefficients[2,1] %>% round(2)
age65_se <- summary(age65)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 6) %>% pull(cog))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 6)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 6) %>% pull(cog))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 6)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age65_d = (as.numeric(age65$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_65to74, age65)

#9.73, 1.3, 0.18

# COG 75 to 84 ------------------------------------------------------------
age75 <- interact(gbm_weights_75to84, 7)
age75_est <- summary(age75)$coefficients[2,1] %>% round(2)
age75_se <- summary(age75)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 7) %>% pull(cog))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 7)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 7) %>% pull(cog))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 7)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age75_d = (as.numeric(age75$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_75to84, age75)

#7.89, 1.7, 0.16

# COG 85+ ------------------------------------------------------------
age85 <- interact(gbm_weights_85plus, 8)
age85_est <- summary(age85)$coefficients[2,1] %>% round(2)
age85_se <- summary(age85)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 8) %>% pull(cog))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 8)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 8) %>% pull(cog))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 8)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age85_d = (as.numeric(age85$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_85plus, age85)

#18.96, 4.37, 0.33

age_values[4,] <- c("Cog", "ATC", age18_est, age25_est, age35_est, age45_est, age55_est, age65_est, age75_est, age85_est)
age_values[5,] <- c("Cog", "SE", age18_se, age25_se, age35_se, age45_se, age55_se, age65_se, age75_se, age85_se)
age_values[6,] <- c("Cog", "D", age18_d, age25_d, age35_d, age45_d, age55_d, age65_d, age75_d, age85_d)



# (2b) Estimating Adaptability and Resilience main treatment effect using weighted GLM for each age group
load("gbm_weights_18to24.Rdata")
load("gbm_weights_25to34.Rdata")
load("gbm_weights_35to44.Rdata")
load("gbm_weights_45to54.Rdata")
load("gbm_weights_55to64.Rdata")
load("gbm_weights_65to74.Rdata")
load("gbm_weights_75to84.Rdata")
load("gbm_weights_85plus.Rdata")

# Helper function
interact <- function(a,b) {
  des <- svydesign(ids = ~country, weights = a$weights,
                   data = subset(cleandata, age==b))
  
  mhq_interact <- svyglm(adaptresil ~ PA,
                         design = des, family = gaussian())
}


# ADAPTRESIL 18 to 24 ------------------------------------------------------------
age18 <- interact(gbm_weights_18to24, 1)
age18_est <- summary(age18)$coefficients[2,1] %>% round(2)
age18_se <- summary(age18)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 1) %>% pull(adaptresil))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 1)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age ==1 ) %>% pull(adaptresil))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 1)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age18_d = (as.numeric(age18$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_18to24, age18)

#19.4, 1.39, 0.29

# ADAPTRESIL 25 to 34 ------------------------------------------------------------
age25 <- interact(gbm_weights_25to34, 2)
age25_est <- summary(age25)$coefficients[2,1] %>% round(2)
age25_se <- summary(age25)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 2) %>% pull(adaptresil))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 2)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 2) %>% pull(adaptresil))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 2)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age25_d = (as.numeric(age25$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_25to34, age25)

#18.16, 1.6, 0.28



# ADAPTRESIL 35 to 44 ------------------------------------------------------------
age35 <- interact(gbm_weights_35to44, 3)
age35_est <- summary(age35)$coefficients[2,1] %>% round(2)
age35_se <- summary(age35)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 3) %>% pull(adaptresil))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 3)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 3) %>% pull(adaptresil))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 3)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age35_d = (as.numeric(age35$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_35to44, age35)

#18.62, 1.6, 0.29


# ADAPTRESIL 45 to 54 ------------------------------------------------------------
age45 <- interact(gbm_weights_45to54, 4)
age45_est <- summary(age45)$coefficients[2,1] %>% round(2)
age45_se <- summary(age45)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 4) %>% pull(adaptresil))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 4)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 4) %>% pull(adaptresil))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 4)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age45_d = (as.numeric(age45$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_45to54, age45)

#16.54, 1.87, 0.27

# ADAPTRESIL 55 to 64 ------------------------------------------------------------
age55 <- interact(gbm_weights_55to64, 5)
age55_est <- summary(age55)$coefficients[2,1] %>% round(2)
age55_se <- summary(age55)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 5) %>% pull(adaptresil))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 5)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 5) %>% pull(adaptresil))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 5)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age55_d = (as.numeric(age55$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_55to64, age55)

#14.45, 1.88, 0.24

# ADAPTRESIL 65 to 74 ------------------------------------------------------------
age65 <- interact(gbm_weights_65to74, 6)
age65_est <- summary(age65)$coefficients[2,1] %>% round(2)
age65_se <- summary(age65)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 6) %>% pull(adaptresil))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 6)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 6) %>% pull(adaptresil))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 6)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age65_d = (as.numeric(age65$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_65to74, age65)

#14.22, 1.67, 0.26

# ADAPTRESIL 75 to 84 ------------------------------------------------------------
age75 <- interact(gbm_weights_75to84, 7)
age75_est <- summary(age75)$coefficients[2,1] %>% round(2)
age75_se <- summary(age75)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 7) %>% pull(adaptresil))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 7)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 7) %>% pull(adaptresil))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 7)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age75_d = (as.numeric(age75$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_75to84, age75)

#12.12, 1.81, 0.25

# ADAPTRESIL 85+ ------------------------------------------------------------
age85 <- interact(gbm_weights_85plus, 8)
age85_est <- summary(age85)$coefficients[2,1] %>% round(2)
age85_se <- summary(age85)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 8) %>% pull(adaptresil))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 8)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 8) %>% pull(adaptresil))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 8)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age85_d = (as.numeric(age85$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_85plus, age85)

#21.35, 4.01, 0.40

age_values[7,] <- c("Adaptresil", "ATC", age18_est, age25_est, age35_est, age45_est, age55_est, age65_est, age75_est, age85_est)
age_values[8,] <- c("Adaptresil", "SE", age18_se, age25_se, age35_se, age45_se, age55_se, age65_se, age75_se, age85_se)
age_values[9,] <- c("Adaptresil", "D", age18_d, age25_d, age35_d, age45_d, age55_d, age65_d, age75_d, age85_d)


# (2b) Estimating Drive and Motivation main treatment effect using weighted GLM for each age group
load("gbm_weights_18to24.Rdata")
load("gbm_weights_25to34.Rdata")
load("gbm_weights_35to44.Rdata")
load("gbm_weights_45to54.Rdata")
load("gbm_weights_55to64.Rdata")
load("gbm_weights_65to74.Rdata")
load("gbm_weights_75to84.Rdata")
load("gbm_weights_85plus.Rdata")

# Helper function
interact <- function(a,b) {
  des <- svydesign(ids = ~country, weights = a$weights,
                   data = subset(cleandata, age==b))
  
  mhq_interact <- svyglm(drivemotiv ~ PA,
                         design = des, family = gaussian())
}


# drivemotiv 18 to 24 ------------------------------------------------------------
age18 <- interact(gbm_weights_18to24, 1)
age18_est <- summary(age18)$coefficients[2,1] %>% round(2)
age18_se <- summary(age18)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 1) %>% pull(drivemotiv))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 1)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age ==1 ) %>% pull(drivemotiv))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 1)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age18_d = (as.numeric(age18$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_18to24, age18)


# drivemotiv 25 to 34 ------------------------------------------------------------
age25 <- interact(gbm_weights_25to34, 2)
age25_est <- summary(age25)$coefficients[2,1] %>% round(2)
age25_se <- summary(age25)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 2) %>% pull(drivemotiv))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 2)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 2) %>% pull(drivemotiv))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 2)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age25_d = (as.numeric(age25$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_25to34, age25)


# drivemotiv 35 to 44 ------------------------------------------------------------
age35 <- interact(gbm_weights_35to44, 3)
age35_est <- summary(age35)$coefficients[2,1] %>% round(2)
age35_se <- summary(age35)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 3) %>% pull(drivemotiv))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 3)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 3) %>% pull(drivemotiv))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 3)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age35_d = (as.numeric(age35$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_35to44, age35)


# drivemotiv 45 to 54 ------------------------------------------------------------
age45 <- interact(gbm_weights_45to54, 4)
age45_est <- summary(age45)$coefficients[2,1] %>% round(2)
age45_se <- summary(age45)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 4) %>% pull(drivemotiv))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 4)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 4) %>% pull(drivemotiv))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 4)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age45_d = (as.numeric(age45$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_45to54, age45)


# drivemotiv 55 to 64 ------------------------------------------------------------
age55 <- interact(gbm_weights_55to64, 5)
age55_est <- summary(age55)$coefficients[2,1] %>% round(2)
age55_se <- summary(age55)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 5) %>% pull(drivemotiv))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 5)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 5) %>% pull(drivemotiv))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 5)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age55_d = (as.numeric(age55$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_55to64, age55)


# drivemotiv 65 to 74 ------------------------------------------------------------
age65 <- interact(gbm_weights_65to74, 6)
age65_est <- summary(age65)$coefficients[2,1] %>% round(2)
age65_se <- summary(age65)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 6) %>% pull(drivemotiv))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 6)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 6) %>% pull(drivemotiv))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 6)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age65_d = (as.numeric(age65$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_65to74, age65)


# drivemotiv 75 to 84 ------------------------------------------------------------
age75 <- interact(gbm_weights_75to84, 7)
age75_est <- summary(age75)$coefficients[2,1] %>% round(2)
age75_se <- summary(age75)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 7) %>% pull(drivemotiv))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 7)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 7) %>% pull(drivemotiv))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 7)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age75_d = (as.numeric(age75$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_75to84, age75)


# drivemotiv 85+ ------------------------------------------------------------
age85 <- interact(gbm_weights_85plus, 8)
age85_est <- summary(age85)$coefficients[2,1] %>% round(2)
age85_se <- summary(age85)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 8) %>% pull(drivemotiv))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 8)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 8) %>% pull(drivemotiv))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 8)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age85_d = (as.numeric(age85$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_85plus, age85)



age_values[10,] <- c("Drivemotiv", "ATC", age18_est, age25_est, age35_est, age45_est, age55_est, age65_est, age75_est, age85_est)
age_values[11,] <- c("Drivemotiv", "SE", age18_se, age25_se, age35_se, age45_se, age55_se, age65_se, age75_se, age85_se)
age_values[12,] <- c("Drivemotiv", "D", age18_d, age25_d, age35_d, age45_d, age55_d, age65_d, age75_d, age85_d)




# (2b) Estimating Mood and Outlook main treatment effect using weighted GLM for each age group
load("gbm_weights_18to24.Rdata")
load("gbm_weights_25to34.Rdata")
load("gbm_weights_35to44.Rdata")
load("gbm_weights_45to54.Rdata")
load("gbm_weights_55to64.Rdata")
load("gbm_weights_65to74.Rdata")
load("gbm_weights_75to84.Rdata")
load("gbm_weights_85plus.Rdata")

# Helper function
interact <- function(a,b) {
  des <- svydesign(ids = ~country, weights = a$weights,
                   data = subset(cleandata, age==b))
  
  mhq_interact <- svyglm(moodoutlook ~ PA,
                         design = des, family = gaussian())
}


# moodoutlook 18 to 24 ------------------------------------------------------------
age18 <- interact(gbm_weights_18to24, 1)
age18_est <- summary(age18)$coefficients[2,1] %>% round(2)
age18_se <- summary(age18)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 1) %>% pull(moodoutlook))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 1)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age ==1 ) %>% pull(moodoutlook))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 1)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age18_d = (as.numeric(age18$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_18to24, age18)


# moodoutlook 25 to 34 ------------------------------------------------------------
age25 <- interact(gbm_weights_25to34, 2)
age25_est <- summary(age25)$coefficients[2,1] %>% round(2)
age25_se <- summary(age25)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 2) %>% pull(moodoutlook))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 2)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 2) %>% pull(moodoutlook))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 2)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age25_d = (as.numeric(age25$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_25to34, age25)


# moodoutlook 35 to 44 ------------------------------------------------------------
age35 <- interact(gbm_weights_35to44, 3)
age35_est <- summary(age35)$coefficients[2,1] %>% round(2)
age35_se <- summary(age35)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 3) %>% pull(moodoutlook))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 3)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 3) %>% pull(moodoutlook))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 3)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age35_d = (as.numeric(age35$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_35to44, age35)


# moodoutlook 45 to 54 ------------------------------------------------------------
age45 <- interact(gbm_weights_45to54, 4)
age45_est <- summary(age45)$coefficients[2,1] %>% round(2)
age45_se <- summary(age45)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 4) %>% pull(moodoutlook))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 4)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 4) %>% pull(moodoutlook))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 4)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age45_d = (as.numeric(age45$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_45to54, age45)


# moodoutlook 55 to 64 ------------------------------------------------------------
age55 <- interact(gbm_weights_55to64, 5)
age55_est <- summary(age55)$coefficients[2,1] %>% round(2)
age55_se <- summary(age55)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 5) %>% pull(moodoutlook))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 5)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 5) %>% pull(moodoutlook))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 5)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age55_d = (as.numeric(age55$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_55to64, age55)


# moodoutlook 65 to 74 ------------------------------------------------------------
age65 <- interact(gbm_weights_65to74, 6)
age65_est <- summary(age65)$coefficients[2,1] %>% round(2)
age65_se <- summary(age65)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 6) %>% pull(moodoutlook))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 6)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 6) %>% pull(moodoutlook))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 6)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age65_d = (as.numeric(age65$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_65to74, age65)


# moodoutlook 75 to 84 ------------------------------------------------------------
age75 <- interact(gbm_weights_75to84, 7)
age75_est <- summary(age75)$coefficients[2,1] %>% round(2)
age75_se <- summary(age75)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 7) %>% pull(moodoutlook))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 7)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 7) %>% pull(moodoutlook))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 7)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age75_d = (as.numeric(age75$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_75to84, age75)


# moodoutlook 85+ ------------------------------------------------------------
age85 <- interact(gbm_weights_85plus, 8)
age85_est <- summary(age85)$coefficients[2,1] %>% round(2)
age85_se <- summary(age85)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 8) %>% pull(moodoutlook))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 8)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 8) %>% pull(moodoutlook))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 8)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age85_d = (as.numeric(age85$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_85plus, age85)



age_values[13,] <- c("Moodoutlook", "ATC", age18_est, age25_est, age35_est, age45_est, age55_est, age65_est, age75_est, age85_est)
age_values[14,] <- c("Moodoutlook", "SE", age18_se, age25_se, age35_se, age45_se, age55_se, age65_se, age75_se, age85_se)
age_values[15,] <- c("Moodoutlook", "D", age18_d, age25_d, age35_d, age45_d, age55_d, age65_d, age75_d, age85_d)






# (2b) Estimating Social Self main treatment effect using weighted GLM for each age group
load("gbm_weights_18to24.Rdata")
load("gbm_weights_25to34.Rdata")
load("gbm_weights_35to44.Rdata")
load("gbm_weights_45to54.Rdata")
load("gbm_weights_55to64.Rdata")
load("gbm_weights_65to74.Rdata")
load("gbm_weights_75to84.Rdata")
load("gbm_weights_85plus.Rdata")

# Helper function
interact <- function(a,b) {
  des <- svydesign(ids = ~country, weights = a$weights,
                   data = subset(cleandata, age==b))
  
  mhq_interact <- svyglm(socialself ~ PA,
                         design = des, family = gaussian())
}


# socialself 18 to 24 ------------------------------------------------------------
age18 <- interact(gbm_weights_18to24, 1)
age18_est <- summary(age18)$coefficients[2,1] %>% round(2)
age18_se <- summary(age18)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 1) %>% pull(socialself))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 1)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age ==1 ) %>% pull(socialself))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 1)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age18_d = (as.numeric(age18$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_18to24, age18)


# socialself 25 to 34 ------------------------------------------------------------
age25 <- interact(gbm_weights_25to34, 2)
age25_est <- summary(age25)$coefficients[2,1] %>% round(2)
age25_se <- summary(age25)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 2) %>% pull(socialself))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 2)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 2) %>% pull(socialself))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 2)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age25_d = (as.numeric(age25$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_25to34, age25)


# socialself 35 to 44 ------------------------------------------------------------
age35 <- interact(gbm_weights_35to44, 3)
age35_est <- summary(age35)$coefficients[2,1] %>% round(2)
age35_se <- summary(age35)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 3) %>% pull(socialself))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 3)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 3) %>% pull(socialself))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 3)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age35_d = (as.numeric(age35$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_35to44, age35)


# socialself 45 to 54 ------------------------------------------------------------
age45 <- interact(gbm_weights_45to54, 4)
age45_est <- summary(age45)$coefficients[2,1] %>% round(2)
age45_se <- summary(age45)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 4) %>% pull(socialself))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 4)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 4) %>% pull(socialself))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 4)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age45_d = (as.numeric(age45$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_45to54, age45)


# socialself 55 to 64 ------------------------------------------------------------
age55 <- interact(gbm_weights_55to64, 5)
age55_est <- summary(age55)$coefficients[2,1] %>% round(2)
age55_se <- summary(age55)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 5) %>% pull(socialself))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 5)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 5) %>% pull(socialself))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 5)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age55_d = (as.numeric(age55$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_55to64, age55)


# socialself 65 to 74 ------------------------------------------------------------
age65 <- interact(gbm_weights_65to74, 6)
age65_est <- summary(age65)$coefficients[2,1] %>% round(2)
age65_se <- summary(age65)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 6) %>% pull(socialself))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 6)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 6) %>% pull(socialself))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 6)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age65_d = (as.numeric(age65$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_65to74, age65)


# socialself 75 to 84 ------------------------------------------------------------
age75 <- interact(gbm_weights_75to84, 7)
age75_est <- summary(age75)$coefficients[2,1] %>% round(2)
age75_se <- summary(age75)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 7) %>% pull(socialself))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 7)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 7) %>% pull(socialself))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 7)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age75_d = (as.numeric(age75$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_75to84, age75)


# socialself 85+ ------------------------------------------------------------
age85 <- interact(gbm_weights_85plus, 8)
age85_est <- summary(age85)$coefficients[2,1] %>% round(2)
age85_se <- summary(age85)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 8) %>% pull(socialself))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 8)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 8) %>% pull(socialself))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 8)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age85_d = (as.numeric(age85$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_85plus, age85)



age_values[16,] <- c("Socialself", "ATC", age18_est, age25_est, age35_est, age45_est, age55_est, age65_est, age75_est, age85_est)
age_values[17,] <- c("Socialself", "SE", age18_se, age25_se, age35_se, age45_se, age55_se, age65_se, age75_se, age85_se)
age_values[18,] <- c("Socialself", "D", age18_d, age25_d, age35_d, age45_d, age55_d, age65_d, age75_d, age85_d)



# (2b) Estimating Mind-Body Connection main treatment effect using weighted GLM for each age group
load("gbm_weights_18to24.Rdata")
load("gbm_weights_25to34.Rdata")
load("gbm_weights_35to44.Rdata")
load("gbm_weights_45to54.Rdata")
load("gbm_weights_55to64.Rdata")
load("gbm_weights_65to74.Rdata")
load("gbm_weights_75to84.Rdata")
load("gbm_weights_85plus.Rdata")

# Helper function
interact <- function(a,b) {
  des <- svydesign(ids = ~country, weights = a$weights,
                   data = subset(cleandata, age==b))
  
  mhq_interact <- svyglm(mindbody ~ PA,
                         design = des, family = gaussian())
}


# mindbody 18 to 24 ------------------------------------------------------------
age18 <- interact(gbm_weights_18to24, 1)
age18_est <- summary(age18)$coefficients[2,1] %>% round(2)
age18_se <- summary(age18)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 1) %>% pull(mindbody))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 1)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age ==1 ) %>% pull(mindbody))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 1)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age18_d = (as.numeric(age18$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_18to24, age18)


# mindbody 25 to 34 ------------------------------------------------------------
age25 <- interact(gbm_weights_25to34, 2)
age25_est <- summary(age25)$coefficients[2,1] %>% round(2)
age25_se <- summary(age25)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 2) %>% pull(mindbody))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 2)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 2) %>% pull(mindbody))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 2)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age25_d = (as.numeric(age25$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_25to34, age25)


# mindbody 35 to 44 ------------------------------------------------------------
age35 <- interact(gbm_weights_35to44, 3)
age35_est <- summary(age35)$coefficients[2,1] %>% round(2)
age35_se <- summary(age35)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 3) %>% pull(mindbody))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 3)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 3) %>% pull(mindbody))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 3)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age35_d = (as.numeric(age35$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_35to44, age35)


# mindbody 45 to 54 ------------------------------------------------------------
age45 <- interact(gbm_weights_45to54, 4)
age45_est <- summary(age45)$coefficients[2,1] %>% round(2)
age45_se <- summary(age45)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 4) %>% pull(mindbody))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 4)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 4) %>% pull(mindbody))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 4)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age45_d = (as.numeric(age45$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_45to54, age45)


# mindbody 55 to 64 ------------------------------------------------------------
age55 <- interact(gbm_weights_55to64, 5)
age55_est <- summary(age55)$coefficients[2,1] %>% round(2)
age55_se <- summary(age55)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 5) %>% pull(mindbody))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 5)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 5) %>% pull(mindbody))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 5)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age55_d = (as.numeric(age55$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_55to64, age55)


# mindbody 65 to 74 ------------------------------------------------------------
age65 <- interact(gbm_weights_65to74, 6)
age65_est <- summary(age65)$coefficients[2,1] %>% round(2)
age65_se <- summary(age65)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 6) %>% pull(mindbody))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 6)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 6) %>% pull(mindbody))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 6)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age65_d = (as.numeric(age65$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_65to74, age65)


# mindbody 75 to 84 ------------------------------------------------------------
age75 <- interact(gbm_weights_75to84, 7)
age75_est <- summary(age75)$coefficients[2,1] %>% round(2)
age75_se <- summary(age75)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 7) %>% pull(mindbody))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 7)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 7) %>% pull(mindbody))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 7)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age75_d = (as.numeric(age75$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_75to84, age75)


# mindbody 85+ ------------------------------------------------------------
age85 <- interact(gbm_weights_85plus, 8)
age85_est <- summary(age85)$coefficients[2,1] %>% round(2)
age85_se <- summary(age85)$coefficients[2,2] %>% round(2)

sd1 = sd(cleandata %>% filter(PA == 0) %>% filter(age == 8) %>% pull(mindbody))
n1 = nrow((cleandata %>% filter(PA == 0) %>% filter(age == 8)))
sd2 = sd(cleandata %>% filter(PA == 1) %>% filter(age == 8) %>% pull(mindbody))
n2 = nrow((cleandata %>% filter(PA == 1) %>% filter(age == 8)))
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))

age85_d = (as.numeric(age85$coefficients)[2] / sd_pooled) %>% round(2)
rm(gbm_weights_85plus, age85)



age_values[19,] <- c("Mindbody", "ATC", age18_est, age25_est, age35_est, age45_est, age55_est, age65_est, age75_est, age85_est)
age_values[20,] <- c("Mindbody", "SE", age18_se, age25_se, age35_se, age45_se, age55_se, age65_se, age75_se, age85_se)
age_values[21,] <- c("Mindbody", "D", age18_d, age25_d, age35_d, age45_d, age55_d, age65_d, age75_d, age85_d)


# write.csv(age_values, file="age_values.csv", row.names=F)


# Subcategory X Age estimates; plots and table ----------------------------
age_values <- read.csv("age_values.csv")

#transform for easier ggplotting
age_values_melt <- reshape2::melt(age_values, id.vars = c("outcome", "estimate"), variable.name = "age", value.name = "value")

age_values_melt <- age_values_melt %>%
  mutate(age = case_when(age == "age18.24" ~ 18,
                         age == "age25.34" ~ 25,
                         age == "age35.44" ~ 35,
                         age == "age45.54" ~ 45,
                         age == "age55.64" ~ 55,
                         age == "age65.74" ~ 65,
                         age == "age75.84" ~ 75,
                         age == "age85plus" ~ 85))

age_values_melt$age <- as.factor(age_values_melt$age)

estimates <- age_values_melt %>% filter(estimate == "ATC" | estimate == "SE")
estimates <- estimates %>% pivot_wider(names_from = estimate, values_from = value)
estimates <- estimates %>%
  mutate(lower = round(ATC-1.96*SE, 2),
         upper = round(ATC+1.96*SE, 2))


# MHQ X Age Plot ----------------------------------------------------------
pdf(file="mhq_age_plot.pdf", width=6, height=3)
estimates %>% filter(outcome == "MHQ") %>%
  ggplot(aes(x=age, y=ATC, group = 1)) + 
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.5, position=position_dodge(0.1)) +
    geom_line() + geom_point() + theme_minimal() +
    labs(title = "MHQ", y = "ATC", x = "Age Group") +
    ylim(0 , 35)
dev.off()

# Subcategories X Age Plots -----------------------------------------------
pdf(file="subcategory_age_plots.pdf", width=9, height=6)

create_ggplot <- function(subcategory, title) {
  estimates %>%
    filter(outcome == subcategory) %>%
    ggplot(aes(x = age, y = ATC, group = 1)) + 
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5, position = position_dodge(0.1)) +
    geom_line() + geom_point() + theme_minimal() +
    labs(title = title, y = "ATC", x = "Age Group") +
    ylim(0, 35)
}

plot1 <- create_ggplot("Cog", "Core Cognition")
plot2 <- create_ggplot("Adaptresil", "Adaptability and Resilience")
plot3 <- create_ggplot("Drivemotiv", "Drive and Motivation")
plot4 <- create_ggplot("Moodoutlook", "Mood and Outlook")
plot5 <- create_ggplot("Socialself", "Social Self")
plot6 <- create_ggplot("Mindbody", "Mind-Body Connection")

grid_layout <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2, byrow = TRUE)
gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 2)

dev.off()





# Sensitivity analyses ----------------------------------------------------
library(tidyverse)
library(survey)
# (3a) Doubly robust estimation using GBM weights
load("cleandata.RData")
load("gbm_weights.Rdata")

des <- svydesign(ids = ~country, weights = gbm_weights$weights, data = cleandata)


mhq_gbm_doublerobust <- 
  svyglm(mhq ~ PA+age+sex+education+employment+relationship+socialize
              + sleep+meddiagnosis+mhseeking+childtrauma+adulttrauma,
                           design = des,
                           family = gaussian())

summary(mhq_gbm_doublerobust)$coefficients[2,1:4] %>% round(2)
# Estimate Std. Error    t value   Pr(>|t|) 
# 17.28       1.07      16.15       0.00 

# (3b) Cohen's d calculation 
##
n1 = 135525 
n2 = 206431 
n = n1+n2

sd1 = sd(cleandata %>% filter(PA == 0) %>% pull(mhq))
sd2 = sd(cleandata %>% filter(PA == 1) %>% pull(mhq))
# Pool SDs
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n-2))
cohensd = as.numeric(mhq_gbm_doublerobust$coefficients)[2] / sd_pooled
cohensd %>% round(2)




# (4a) Multiple imputation + GBM
cleandata$id <- 1:nrow(cleandata)

predMatrix <- quickpred(cleandata, mincor=0.10)
predMatrix[, c("id")] <- 0                        # id = 0
predMatrix[, c("country")] <- -2                  # country = -2
predMatrix[c("country","id"), "country"] <- 0     # id x country = 0
#In the predictor matrix, -2 denotes the class variable,
#a value 1 indicates a fixed effect and a value 2 indicates a random effect.
impMethod <- make.method(data = cleandata, defaultMethod = "pmm")
impMethod[c("sex")] <- "polyreg"      
impMethod[c("education")] <- "polyreg"
impMethod[c("relationship")] <- "polyreg"
impMethod[c("meddiagnosis")] <- "logreg"
impMethod[c("mhseeking")] <- "logreg"
impMethod[c("childtrauma")] <- "logreg"
impMethod[c("adulttrauma")] <- "logreg"


cleandata_mi <- mice(cleandata, method = impMethod,
                    predictorMatrix = predMatrix,
                    maxit = 5, m = 5, seed = 111)

# save(cleandata_mi, file = "cleandata_mi.RData")

# load("cleandata_mi.RData")

gbm_weights_mi <- weightthem(PA ~ age   + sex + education + employment + relationship + socialize
                             + sleep + meddiagnosis + mhseeking + childtrauma + adulttrauma,
                             cleandata_mi, 
                             approach = 'within', method = "gbm", estimand = "ATC", trim = 0.99, distribution = "bernoulli") 
# save(gbm_weights_mi, file = "gbm_weights_mi.RData")
load("gbm_weights_mi.RData")

des <- svydesign(ids = ~country, weights = gbm_weights_mi$weights, data = cleandata)


mhq_gbm_mi <- svyglm(mhq ~ PA, design = des, family = gaussian())

summary(mhq_gbm_mi)$coefficients[2,1:4] %>% round(2)
# Estimate Std. Error    t value   Pr(>|t|) 
# 34.56       1.49      23.19       0.00 
# (4b) Cohen's d calculation
n1 = 135525 
n2 = 206431 
n = n1+n2
sd1 = sd(cleandata %>% filter(PA == 0) %>% pull(mhq))
sd2 = sd(cleandata %>% filter(PA == 1) %>% pull(mhq))
# Pool SDs
sd_pooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n-2))
cohensd = as.numeric(mhq_gbm_mi$coefficients)[2] / sd_pooled
cohensd %>% round(2)
# 0.49
# (4c) MI + GBM Doubly robust estimation ------------
mhq_gbm_mi_doublerobust <- 
  svyglm(mhq ~ PA+age+sex+education+employment+relationship+socialize
         + sleep+meddiagnosis+mhseeking+childtrauma+adulttrauma,
         design = des,
         family = gaussian())

summary(mhq_gbm_mi_doublerobust)$coefficients[2,1:4] %>% round(2)
# Estimate Std. Error    t value   Pr(>|t|) 
# 17.61       1.15      15.36       0.00 
# (4d) Cohen's d calculation
cohensd = as.numeric(mhq_gbm_mi_doublerobust$coefficients)[2] / sd_pooled
cohensd %>% round(2)
#0.25




# (4a) Multiple imputation + Covariate Balancing Propensity Score weighting
cbps_weights_mi <- weightthem(PA ~ age   + sex + education + employment + relationship + socialize
                             + sleep + meddiagnosis + mhseeking + childtrauma + adulttrauma,
                             cleandata_mi, 
                             approach = 'within', method = "cbps", estimand = "ATC", trim = 0.99, distribution = "bernoulli") 
save(cbps_weights_mi, file = "cbps_weights_mi.RData")
load("cbps_weights_mi.RData")

des <- svydesign(ids = ~country, weights = cbps_weights_mi$weights, data = cleandata)

mhq_cbps_mi <- svyglm(mhq ~ PA, design = des, family = gaussian())

summary(mhq_cbps_mi)$coefficients[2,1:4] %>% round(2)
# Estimate Std. Error    t value   Pr(>|t|) 
# 34.56       1.49      23.19       0.00 

# (4b) Cohen's d calculation
cohensd = as.numeric(mhq_cbps_mi$coefficients)[2] / sd_pooled
cohensd %>% round(2)
# 0.49

# (4c) MI + CBPS Doubly robust estimation
mhq_cbps_mi_doublerobust <- 
  svyglm(mhq ~ PA+age+sex+education+employment+relationship+socialize
         + sleep+meddiagnosis+mhseeking+childtrauma+adulttrauma,
         design = des,
         family = gaussian())

summary(mhq_cbps_mi_doublerobust)$coefficients[2,1:4] %>% round(2)


# (4d) Cohen's d calculation
cohensd = as.numeric(mhq_cbps_mi_doublerobust$coefficients)[2] / sd_pooled
cohensd %>% round(2)
# 0.25















