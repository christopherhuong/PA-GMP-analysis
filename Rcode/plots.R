library(tidyverse)
library(jtools)
library(interactions)
library(survey)
library(WeightIt)


load('mhm.RData')

# mhm <- mhm %>%
#     mutate(age_cat = case_when(age == 1 ~ "Young Adult",
#                              age == 2 ~ "Young Adult",
#                              age == 3 ~ "Middle Adult",
#                              age == 4 ~ "Middle Adult",
#                              age == 5 ~ "Middle Adult",
#                              age == 6 ~ "Senior",
#                              age == 7 ~ "Senior",
#                              age == 8 ~ "Senior"))
#   
# mhm$age_cat <- factor(mhm$age_cat)

# 
# gbm_interact <- weightit(PA*age ~ ### PA = integer 0/1, age = integer 1:8
#                          + sex
#                          + education
#                          + employment
#                          + relationship
#                          + socialize
#                          + sleep
#                          + meddiagnosis
#                          + mhseeking
#                          + childtrauma
#                          + adulttrauma,
#                          mhm,
#                          method = "gbm",
#                          estimand = "ATE",
#                          trim.at = 0.99,
#                          distribution = "gaussian")

load("gbm_interact.RData")

des_int <- svydesign(ids = ~country, weights = gbm_interact$weights,
                     data = mhm)



mhq_interact <- svyglm(mhq ~ PA*age,
                       design = des_int,
                       family = gaussian())


summary(mhq_interact)



interact_plot(mhq_interact,
              pred = age,
              modx = PA,
              x.label = "Age",
              y.label = "MHQ",
              interval = T,
              legend.main = " PA X Age",
              jitter = 0.9
              )





