


load("weightdat_male.RData")
load("weightdat_female.RData")



################################################
library(interactions)
library(survey)
library(MatchThem)

des <- svydesign(ids = ~country, weights = ~1,
                 data = imp_overall_long) 


mhq_female_int <-with(weightdat_female, svyglm(mhq ~ -1
                                              + PA*age
                                              + genderdiff
                                              + education
                                              + employment
                                              + relationship
                                              + socialize
                                              + sleep
                                              + meddiagnosis
                                              + childtrauma
                                              + adulttrauma,
                                                    
                                       design = des,
                                       family = gaussian())) 



kable(summary(pool(mhq_female_int)),
      digits = 3) 

####### PLOTTING 1ST ITERATION ##########

cat_plot(mhq_female_int[["analyses"]][[1]],
         pred = PA, modx = age, 
         geom = "line")


##################



mhq_male_int <-with(weightdat_male, svyglm(mhq ~ -1
                                               + PA*age
                                               + genderdiff
                                               + education
                                               + employment
                                               + relationship
                                               + socialize
                                               + sleep
                                               + meddiagnosis
                                               + childtrauma
                                               + adulttrauma,
                                               
                                               design = des,
                                               family = gaussian())) 



kable(summary(pool(mhq_male_int)),
      digits = 3) 

####### PLOTTING 1ST ITERATION ##########

cat_plot(mhq_male_int[["analyses"]][[1]],
         pred = PA, modx = age, 
         geom = "line")









