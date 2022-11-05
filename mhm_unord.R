library(MatchThem) 
library(survey) 
library(knitr)
library(car)
#load in multiply imputed, weighted data
#multinomial treatment = PA, focal = "Rarely/never"
#estimand = ATT, approach = "within", method = cbps
load("weightdat_multi_att_unord.RData")   
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




anova <- anova.svyglm(mhq_multi_att_unord)












