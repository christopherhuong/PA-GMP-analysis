
library(MatchThem) 
library(survey) 
library(CBPS)
library(cobalt)
library(knitr)

#load in multiply imputed, weighted data
#multinomial treatment = PA, focal = "Rarely/never"
#estimand = ATT, approach = "within", method = cbps
load("weightdat_multi_att_ord.RData")   
load("imp_long.RData")
############ SURVEY DESIGN 
#
des_multi_att <- svydesign(ids = ~country, weights = ~1, data = imp_long) 
# nest = T? try it out


#can remove all covariates from svyglm since they are balanced already
#  double robust analysis
mhq_multi_att <-with(weightdat_multi_att, svyglm(mhq ~ 
                                                   PA
                                                 + age #hasnt age_cat been balanced?
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
                                                 design = des_multi_att,
                                                 family = gaussian())) 



mhq_multi_att <-with(weightdat_multi_att, svyglm(mhq ~ 
                                                   PA,
                                                 
                                                 design = des_multi_att,
                                                 family = gaussian())) 


#similar coefficients, different intercept


kable(summary(pool(mhq_multi_att)),
      digits = 3) 















