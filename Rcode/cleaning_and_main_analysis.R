
library(tidyverse)
library(naniar)
library(knitr)
library(WeightIt)
library(survey)
library(cobalt)


load("dat.RData")

load("rawdata.RData")
save(rawdata, file="rawdata.RData")

dat$Submit.Date..UTC. <- as.Date(dat$Submit.Date..UTC.)
str(dat$Submit.Date..UTC.)
range(dat$Submit.Date..UTC.)
#"2022-01-01" "2022-10-14"

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

summary(mhm$education)
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
        s.d.denom = "pooled",
        stats = c("m", "ks"),
        thresholds = c(m = .01))

# 15 balanced, 26 not balanced


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
#                       estimand = "ATC",
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
        stats = c("m"),
        s.d.denom = "control",
        thresholds = c(m = .01))

love.plot(gbm_weights, binary = "std", var.order = "un", stats = "m",
           thresholds = .01) + theme(legend.position = "top")


# Balance tally for mean differences
# count
# Balanced, <0.05        41
# Not Balanced, >0.05     0
############### ESTIMATING MAIN TREATMENT EFFECT ############


des <- svydesign(ids = ~country, weights = gbm_weights$weights,
                                  data = mhm)



mhq <- svyglm(mhq ~ PA,
              design = des,
              family = gaussian())

summary(mhq)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   PA            17.857      1.413   12.64   <2e-16 ***
confint(mhq)
#                  2.5 %   97.5 %
#   (Intercept) 41.86981 52.25683
# PA            15.07241 20.64148


#cohen's d caclulation
sd1 <- subset(mhm, PA == 0)
sd1 <-  sd(sd1$mhq)
sd2 <- subset(mhm, PA == 1)
sd2 <-  sd(sd2$mhq)
17.86/(sqrt((sd1^2+sd2^2)/2)) # = 0.25




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
#                           Estimate   Std. Error t value Pr(>|t|)
# PA                          17.7350     0.9852  18.001  < 2e-16
confint(mhq_doublerobust)

# PA                                15.7919559  19.6779987

# core cognition ----------------------------------------------------------




cog <- svyglm(cog ~ PA,
              design = des,
              family = gaussian())

summary(cog)
# Coefficients:
#                  Estimate   Std. Error t value Pr(>|t|)    
#   PA            16.327      1.246      13.11   <2e-16 ***
confint(cog)
#                2.5 %   97.5 %
# PA          13.87242 18.78155


sd1 <- subset(mhm, PA == 0)
sd1 <-  sd(sd1$cog)
sd2 <- subset(mhm, PA == 1)
sd2 <-  sd(sd2$cog)
16.327/(sqrt((sd1^2+sd2^2)/2)) # = 0.25


# adapt / resilience ------------------------------------------------------



adaptresil <- svyglm(adaptresil ~ PA,
              design = des,
              family = gaussian())

summary(adaptresil)
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#   PA            17.569      1.391   12.63   <2e-16 ***
confint(adaptresil)
#              2.5 %   97.5 %
# PA          14.82828 20.30942

sd1 <- subset(mhm, PA == 0)
sd1 <-  sd(sd1$adaptresil)
sd2 <- subset(mhm, PA == 1)
sd2 <-  sd(sd2$adaptresil)
17.569/(sqrt((sd1^2+sd2^2)/2)) # = 0.26

# drive / motivation ------------------------------------------------------




drivemotiv <- svyglm(drivemotiv ~ PA,
                     design = des,
                     family = gaussian())

summary(drivemotiv)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#    PA            15.863      1.521   10.43   <2e-16 ***
confint(drivemotiv)
#                 2.5 %   97.5 %
# PA          12.86640 18.85896

sd1 <- subset(mhm, PA == 0)
sd1 <-  sd(sd1$drivemotiv)
sd2 <- subset(mhm, PA == 1)
sd2 <-  sd(sd2$drivemotiv)
15.86/(sqrt((sd1^2+sd2^2)/2)) # = 0.24

# mood / outlook ----------------------------------------------------------



moodoutlook <- svyglm(moodoutlook ~ PA,
                     design = des,
                     family = gaussian())

summary(moodoutlook)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   PA            15.273      1.391   10.98   <2e-16 ***
confint(moodoutlook)
#               2.5 %   97.5 %
# PA           12.53141 18.0139

sd1 <- subset(mhm, PA == 0)
sd1 <-  sd(sd1$moodoutlook)
sd2 <- subset(mhm, PA == 1)
sd2 <-  sd(sd2$moodoutlook)
15.27/(sqrt((sd1^2+sd2^2)/2)) # = 0.22

# social self -------------------------------------------------------------




socialself <- svyglm(socialself ~ PA,
                      design = des,
                      family = gaussian())

summary(socialself)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#  PA             13.019      1.439   9.049   <2e-16 ***
confint(socialself)
#               2.5 %   97.5 %
# PA          10.18358 15.85395

sd1 <- subset(mhm, PA == 0)
sd1 <-  sd(sd1$socialself)
sd2 <- subset(mhm, PA == 1)
sd2 <-  sd(sd2$socialself)
13.019/(sqrt((sd1^2+sd2^2)/2)) # = 0.17

# mind body ---------------------------------------------------------------



mindbody <- svyglm(mindbody ~ PA,
                     design = des,
                     family = gaussian())

summary(mindbody)
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#   PA            19.247      1.314   14.65   <2e-16 ***
confint(mindbody)
#                 2.5 %   97.5 %
# PA          16.65850 21.83616

sd1 <- subset(mhm, PA == 0)
sd1 <-  sd(sd1$mindbody)
sd2 <- subset(mhm, PA == 1)
sd2 <-  sd(sd2$mindbody)
19.247/(sqrt((sd1^2+sd2^2)/2)) # = 0.31







