
require("gbm")
library(MatchThem)
library(WeightIt)
library(mice)

load("mhm.RData")
load("imp_overall.RData")



################ GBM ################
weightit_overall_gbm <- weightit(PA ~   
                                   age
                                 + sex
                                 + genderdiff
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
                                 approach = 'within',  
                                 method = "gbm",    
                                 estimand = "ATE",
                                 trim.at = 0.99,
                                 distribution = "multinomial")




######### gbm on multiply imputed data

weightdat_overall_gbm <- weightthem(PA ~   
                                      age
                                    + sex
                                    + genderdiff
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
                                    method = "gbm",    
                                    estimand = "ATE",
                                    trim.at = 0.99)



