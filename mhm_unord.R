library(MatchThem) 
library(survey) 
library(knitr)
library(car)
library(mice)
library(miceadds)
#load in multiply imputed, weighted data
#multinomial treatment = PA, focal = "Rarely/never"
#estimand = ATT, approach = "within", method = cbps
load("weightdat_multi_att_unord.RData")   
load("imp_unord.RData")
load("imp_long_unord.RData")
############ SURVEY DESIGN 
#
des_multi_att_unord <- svydesign(ids = ~country, weights = ~1,
                                 data = imp_long_unord) 
# nest = T? try it out


#can remove all covariates from svyglm since they are balanced already?

# cannot do interaction effect with a covariate(age) because it was balanced?



mhq_multi_att_unord <-with(weightdat_multi_att_unord, svyglm(mhq ~ 
                                                   PA,
                                                 
                                                 design = des_multi_att,
                                                 family = gaussian())) 



kable(summary(pool(mhq_multi_att_unord)),
      digits = 3) 


mitml::testModels(model=mod1$analyses, null.model=mod0$analyses, method="D1")
mitml::testModels(model=mod1$analyses, null.model=mod0$analyses, method="D2")



aov <- with(weightdat_multi_att_unord, aov(mhq ~ PA))

kable(summary(MatchThem::pool(aov)),
      digits = 3)





