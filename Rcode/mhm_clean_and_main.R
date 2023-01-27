
library(tidyverse)
library(naniar)
library(knitr)
library(WeightIt)
library(survey)
library(cobalt)


load("dat.RData")




# WRANGLING ---------------------------------------------------------------







mhm <-data.frame()[1:nrow(dat), ]  # create new DF with n rows

mhm <- mhm %>%                    #add and rename relevant variables to new df
  add_column(
              mhq = dat$Overall.MHQ,
              cog = dat$Cognition,
              adaptresil = dat$Adaptability...Resilence,
              drivemotiv = dat$Drive...Motivation,  #differs from the response item
              moodoutlook = dat$Mood...Outlook,
              socialself = dat$Social...Self,
              mindbody = dat$Mind.Body.Connection,
              PA = dat$Frequency.of.doing.exercise,
              age = dat$Age,
              sex = dat$Biological.Sex,
              country = dat$Country,
              ethnicity = dat$Ethnicity,
              education = dat$Education,
              employment = dat$Employment,
              relationship = dat$Current.Family.Situation,
              socialize = dat$Frequency.of.Socializing,
              sleep = dat$Frequency.of.getting.a.good.nights.sleep,
              meddiagnosis = dat$Presence.Absence.of.Diagnosed.Medical.Disorder,
              mhseeking = dat$Mental.Health.Treatment.Status,
              childtrauma = dat$Childhood.traumas,
              adulttrauma = dat$Adult.traumas)
  






mhm <- mhm %>%
  subset(select = -c(ethnicity))

###### drop due to missingness
###### effects of ethnicity may be somewhat attenuated by country nesting
###### mhdiagnosis is redundant with mhseeking, drop



# only keep english responses, removes 2 rows which had arabic or something
summary(mhm$PA)

mhm <- mhm %>%
  subset(PA == "Every day" |
           PA == "Few days a week" |
           PA == "Less than once a week" |
           PA == "Once a week" |
           PA == "Rarely/Never")

#dropped 2 rows

mhm <- mhm %>%
  mutate(PA = if_else(PA == "Rarely/Never", 0L, 1L))

summary(mhm$PA)
###

summary(mhm$age)
mhm$age <- as.numeric(mhm$age)


###
summary(mhm$sex)


mhm$sex <- factor(mhm$sex, order = F)
summary(mhm$sex)        


###

mhm$country <- as.integer(mhm$country)
summary(mhm$country)

###

summary(mhm$education)   ####### WHAT IS "Médio completo" ?????
mhm <- mhm %>%              
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

table(mhm$education)
str(mhm$education)



mhm$education <- factor(mhm$education, order = F)

###
table(mhm$employment)
mhm$employment <- factor(mhm$employment, order = F)
summary(mhm$employment)

###
table(mhm$relationship)
mhm$relationship <- factor(mhm$relationship, order = F)
summary(mhm$relationship)
###
table(mhm$socialize)
mhm$socialize <- factor(mhm$socialize, order = F,
                        levels = c("Rarely/Never",
                                   "1-3 times a month",
                                   "Once a week",
                                   "Several days a week"))
summary(mhm$socialize)
###
table(mhm$sleep)
mhm$sleep <- factor(mhm$sleep, order = F,
                    levels = c("Hardly ever",
                               "Some of the time",
                               "Most of the time",
                               "All of the time"))
summary(mhm$sleep)
###
table(mhm$meddiagnosis)
mhm$meddiagnosis <- factor(mhm$meddiagnosis, order = F)
summary(mhm$meddiagnosis)

###

table(mhm$mhseeking)
mhm$mhseeking <- factor(mhm$mhseeking, order = F)
summary(mhm$mhseeking)



###
mhm$childtrauma <- 
  if_else((mhm$childtrauma == "|I did not experience any of the above during my childhood")|(mhm$childtrauma == "|None of the above"), 
          "No", "Yes")
mhm$childtrauma <- as.factor(mhm$childtrauma)
summary(mhm$childtrauma)
####
mhm$adulttrauma <- 
  if_else((mhm$adulttrauma == "|I did not experience any of the above during my childhood")|(mhm$adulttrauma == "|None of the above"), 
          "No", "Yes")
mhm$adulttrauma <- as.factor(mhm$adulttrauma)
summary(mhm$adulttrauma)

###


mhm[mhm == "Prefer not to say"] <- NA 
mhm[mhm == ""] <- NA
sum(is.na(mhm)) # 38543

mhm <- droplevels(mhm)  #get rid of "Prefer not to say" factors
summary(mhm)


save(mhm, file = "mhm.RData")


############# plot missingness
gg_miss_var(mhm %>%
  dplyr::select(PA, age, sex, country, education,
                  employment, relationship, socialize,
                  sleep, meddiagnosis, mhseeking,
                  childtrauma,adulttrauma), show_pct = T)


############# return percent of missingness
percentmiss <- function(x){
  sum(is.na(x)) / length(x) * 100} 

kable(apply(mhm, 2, percentmiss),
      digits = 3)   ####percent missingness per col
kable(table(apply(mhm, 1, percentmiss)),
      digits = 3)###number of subjects with missing values by percent 

#   |             |     x|
#   |:------------|-----:|
#   |mhq          | 0.000|
#   |cog          | 0.000|
#   |adaptresil   | 0.000|
#   |drivemotiv   | 0.000|
#   |moodoutlook  | 0.000|
#   |socialself   | 0.000|
#   |mindbody     | 0.000|
#   |PA           | 0.000|
#   |age          | 0.000|
#   |sex          | 0.961|
#   |country      | 0.000|
#   |education    | 2.534|
#   |employment   | 0.000|
#   |relationship | 4.292|
#   |socialize    | 0.000|
#   |sleep        | 0.000|
#   |meddiagnosis | 2.363|
#   |mhseeking    | 1.121|
#   |childtrauma  | 0.000|
#   |adulttrauma  | 0.000|
# # 
#   
#   |Var1 |   Freq|
#   |:----|------:|
#   |0    | 309471|
#   |5    |  27393|
#   |10   |   4285|
#   |15   |    675|
#   |20   |    105|
#   |25   |     27|




######## PRE-WEIGHTING BALANCE #############


bal.tab(PA ~ age + sex + education + employment + relationship
        + socialize + sleep + meddiagnosis + mhseeking
        + childtrauma + adulttrauma,
        data = mhm,
        s.d.denom = "treated",
        stats = c("m", "ks"),
        thresholds = c(m = .05))

# 30 balanced, 11 not balanced







########### GBM WEIGHTING #########



# gbm_weights <- weightit(PA ~   
#                         age
#                       + sex
#                       + education
#                       + employment
#                       + relationship
#                       + socialize
#                       + sleep
#                       + meddiagnosis
#                       + mhseeking
#                       + childtrauma
#                       + adulttrauma,
#                       mhm, 
#                       method = "gbm",    
#                       estimand = "ATT",
#                       trim.at = 0.99,
#                       distribution = "bernoulli")
# 
# save(gbm_weights, file = "gbm_weights.RData")

load("gbm_weights.RData")


########### COVARIATE BALANCING DIAGNOSTICS ###########
gbm_weights
summary(gbm_weights)


bal.tab(gbm_weights$treat ~ gbm_weights$covs,
        data = gbm_weights,
        weights = gbm_weights$weights,
        stats = c("m", "ks"),
        s.d.denom = "treated",
        thresholds = c(m = .05))

love.plot(gbm_weights, binary = "std", var.order = "un", stats = "m",
           thresholds = .05) + theme(legend.position = "top")

############### ESTIMATING MAIN TREATMENT EFFECT ############


des <- svydesign(ids = ~country, weights = gbm_weights$weights,
                                  data = mhm)



mhq <- svyglm(mhq ~ PA,
              design = des,
              family = gaussian())

summary(mhq)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     63.180      2.665   23.71   <2e-16 ***
#   PA            18.445      1.484   12.43   <2e-16 ***
confint(mhq)
#                2.5 %  97.5 %
# (Intercept) 57.92777 68.4314
# PA          15.52110 21.3689



mhq_doublerobust <- svyglm(mhq ~ PA
                           + age  
                           + sex
                           + education
                           + employment
                           + relationship
                           + socialize
                           + sleep
                           + meddiagnosis
                           + mhseeking
                           + childtrauma
                           + adulttrauma,
                          design = des,
                          data = gbm_weights,
                          family = gaussian())

summary(mhq_doublerobust)
# Coefficients:
#                  Estimate     Std. Error   t value   Pr(>|t|)
# (Intercept)        -3.2311     3.0760      -1.050   0.29483
# PA                  18.0694     1.0739      16.825  < 2e-16
confint(mhq_doublerobust)
#                           2.5 %       97.5 %
#   (Intercept)          -9.2975699   2.83540597
# PA                     15.9513466  20.18738800

# core cognition ----------------------------------------------------------




cog <- svyglm(cog ~ PA,
              design = des,
              family = gaussian())

summary(cog)
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     76.996      2.508   30.70   <2e-16 ***
#   PA            16.439      1.352   12.16   <2e-16 ***
confint(cog)
#                2.5 %   97.5 %
#   (Intercept)  72.05418 81.93794
# PA             13.77475 19.10279



# adapt / resilience ------------------------------------------------------



adaptresil <- svyglm(adaptresil ~ PA,
              design = des,
              family = gaussian())

summary(adaptresil)
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     83.583      2.304   36.27   <2e-16 ***
#   PA            18.138      1.389   13.06   <2e-16 ***
confint(adaptresil)
#                  2.5 %   97.5 %
#   (Intercept)    79.04222 88.12355
# PA               15.40061 20.87592



# drive / motivation ------------------------------------------------------




drivemotiv <- svyglm(drivemotiv ~ PA,
                     design = des,
                     family = gaussian())

summary(drivemotiv)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     79.719      2.534  31.456   <2e-16 ***
#   PA            15.746      1.585   9.934   <2e-16 ***
confint(drivemotiv)
#                 2.5 %   97.5 %
#   (Intercept)   74.72539 84.71355
# PA              12.62235 18.86920


# mood / outlook ----------------------------------------------------------



moodoutlook <- svyglm(moodoutlook ~ PA,
                     design = des,
                     family = gaussian())

summary(moodoutlook)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   63.603      2.502   25.43   <2e-16 ***
#   PA            16.321      1.461   11.17   <2e-16 ***
confint(moodoutlook)
#               2.5 %   97.5 %
#   (Intercept) 58.67378 68.53301
# PA            13.44200 19.19970



# social self -------------------------------------------------------------




socialself <- svyglm(socialself ~ PA,
                      design = des,
                      family = gaussian())

summary(socialself)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     67.885      2.591  26.202   <2e-16 ***
#   PA            14.026      1.474   9.514   <2e-16 ***
confint(socialself)
#               2.5 %   97.5 %
#   (Intercept) 62.77923 72.99011
# PA            11.12054 16.93053



# mind body ---------------------------------------------------------------



mindbody <- svyglm(mindbody ~ PA,
                     design = des,
                     family = gaussian())

summary(mindbody)
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      67.178      2.058   32.64   <2e-16 ***
#   PA             19.807      1.297   15.28   <2e-16 ***
confint(mindbody)
#                 2.5 %   97.5 %
#   (Intercept)   63.12209 71.23442
# PA              17.25232 22.36218









