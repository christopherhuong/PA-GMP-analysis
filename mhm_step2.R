


#load in multiply imputed, weighted data
#multinomial treatment = PA, focal = "Rarely/never"
#estimand = ATT, approach = "within", method = cbps
load("weightdat_multi_att.RData")   

############ SURVEY DESIGN 
#
des_multi_att <- svydesign(ids = ~country, weights = ~1, data = imp_long) 
# nest = T? try it out


#can remove all covariates from svyglm since they are balanced already
#  double robust analysis
mhq_multi_att <-with(weightdat_multi_att, svyglm(mhq ~ 
                                                   PA
                                                 + age_cat
                                                 + PA:age_cat
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

kable(summary(pool(mhq_multi_att)),
      digits = 3) 




