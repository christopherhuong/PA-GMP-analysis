
library(tidyverse)
library(naniar)
library(knitr)
library(WeightIt)
library(survey)
library(cobalt)



load('mhm.rdata')
load('imp_overall.rdata')



gbm_weightsATC <- weightit(PA ~
                          age
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
                        mhm,
                        method = "gbm",
                        estimand = "ATC",
                        trim.at = 0.99,
                        distribution = "bernoulli")

save(gbm_weightsATC, file = "gbm_weightsATC.RData")













weightdat_overallATC <- weightthem(PA ~  
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
                                   imp_overall, 
                                   approach = 'within',  
                                   method = "cbps",    
                                   estimand = "ATC",
                                   trim = 0.99) 

save(weightdat_overallATC, file = "weightdat_overallATC.RData")











gbm_interactATC <- weightit(PA*age ~ ### PA = integer 0/1, age = integer 1:8
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
                         mhm,
                         method = "gbm",
                         estimand = "ATC",
                         trim.at = 0.99,
                         distribution = "gaussian")


save(gbm_interactATC, file = "gbm_interactATC.RData")














gbm_weights <- weightit(PA ~
                        age
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
                      mhm,
                      method = "gbm",
                      estimand = "ATT",
                      trim.at = 0.99,
                      distribution = "bernoulli")

save(gbm_weights, file = "gbm_weights.RData")

