library(tidyverse)
library(MatchThem) 
library(survey) 
library(knitr)

library(mice)
library(miceadds)
#load in multiply imputed, weighted data
#multinomial treatment = PA, focal = "Rarely/never"
#estimand = ATT, approach = "within", method = cbps
load("weightdat_multi_att_unord.RData")   
# load("imp_unord.RData")
load("imp_long_unord.RData")
############ SURVEY DESIGN 
#
des_multi_att_unord <- svydesign(ids = ~country, weights = ~1,
                                 data = imp_long_unord) 
# nest = T? try it out


#can remove all covariates from svyglm since they are balanced already?

# cannot do interaction effect with a covariate(age) because it was balanced?
# instead, split mice & cbps for different groups of interest



mhq_multi_att_unord <-with(weightdat_multi_att_unord, svyglm(mhq ~ -1 +
                                                   PA,
                                                 
                                                 design = des_multi_att,
                                                 
                                                 family = gaussian())) 



mhqpool <- pool(mhq_multi_att_unord)

kable(summary(mhqpool),
      digits = 3) 


library(jtools)
library(broom.mixed)

summ(mhq_multi_att_unord[["analyses"]][[1]])





plot_summs(mhq_multi_att_unord[["analyses"]][[1]],
           mhq_multi_att_unord[["analyses"]][[2]],
           mhq_multi_att_unord[["analyses"]][[3]],
           mhq_multi_att_unord[["analyses"]][[4]],
           mhq_multi_att_unord[["analyses"]][[5]]
           
         )

################################################
library(interactions)




mhq_multi_att_unord <-with(weightdat_multi_att_unord, svyglm(mhq ~ 
                                                               PA*age,
                                                             
                                                             design = des_multi_att,
                                                             
                                                             family = gaussian())) 




cat_plot(mhq_multi_att_unord[["analyses"]][[1]],
         pred = PA, modx = age,
         geom = "line")
         

##################



mhq_multi_att_unord <-with(weightdat_multi_att_unord, svyglm(mhq ~ 
                                                               PA*sex*age,
                                                             
                                                             design = des_multi_att,
                                                             
                                                             family = gaussian())) 




cat_plot(mhq_multi_att_unord[["analyses"]][[1]],
         pred = PA, modx = sex, mod2 = age,
         geom = "line")



with(mhq_multi_att_unord[["analyses"]], cat_plot(pred = PA, modx = sex, mod2 = age,
                                  geom = "line"))


########################################################













