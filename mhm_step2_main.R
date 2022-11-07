library(tidyverse)
library(MatchThem) 
library(survey) 
library(knitr)


#load in multiply imputed, weighted data
#multinomial treatment = PA, focal = "Rarely/never"
#estimand = ATT, approach = "within", method = cbps
load("imp_overall_long.RData")  
load("weightdat_overall.RData")
############ SURVEY DESIGN 
#
des <- svydesign(ids = ~country, weights = ~1,
                                 data = imp_overall_long) 
# nest = T? try it out


#can remove all covariates from svyglm since they are balanced already?

# cannot do interaction effect with a covariate(age) because it was balanced?
# instead, split mice & cbps for different groups of interest



mhq_overall <-with(weightdat_overall, svyglm(mhq ~ -1 
                                             + PA 
                                             + age
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
                                             + adulttrauma
                                                 
                                                 design = des,
                                                 
                                                 family = gaussian())) 



kable(summary(pool(mhq_overall)),
      digits = 3) 















