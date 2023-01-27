library(WeightIt)
library(tidyverse)



load("mhm.RData")


# SECONDARY ANALYSIS: INTERACTIONS WITH GBM  ----------------------------------





gbm_interact <- weightit(PA*age ~ ### PA = integer 0/1, age = integer 1:8
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
                         estimand = "ATE",
                         trim.at = 0.99,
                         distribution = "gaussian")



save(gbm_interact, file = "gbm_interact.RData")





des_int <- svydesign(ids = ~country, weights = gbm_interact$weights,
                 data = mhm)



mhq_interact <- svyglm(mhq ~ PA*age,
                      design = des_int,
                      family = gaussian())


summary(mhq_interact)
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    4.3888     3.7979   1.156 0.249093    
#   PA          25.6385     2.6505   9.673  < 2e-16 ***
#   age          14.9358     0.7656  19.509  < 2e-16 ***
#   PA:age       -2.0408     0.5796  -3.521 0.000522 *** 
confint(mhq_interact)
#                    2.5 %     97.5 %
#   (Intercept)   -3.095626 11.8731520
# PA              20.415341 30.8617081
# age             13.427074 16.4445412
# PA:age          -3.183107 -0.8985421

library(interactions)
interactions::interact_plot(mhq_interact,
                            pred = age, modx = PA)
# shown in pdf




# SENSITIVITY ANALYSIS: MI + CBPS -----------------------------------------


library(broom.mixed)
library(MCMCglmm)
library(msm)
library(tmvtnorm)
install.packages("https://cran.r-project.org/src/contrib/Archive/linLIR/linLIR_1.1.tar.gz",
                 repos=NULL, method="libcurl")
install.packages("https://cran.r-project.org/src/contrib/Archive/hmi/hmi_1.0.0.tar.gz",
                 repos=NULL, method="libcurl")
library(hmi)  #not available
library(mice)
library(miceadds)






mhm$id <- 1:nrow(mhm)





predMatrix <- quickpred(mhm, mincor=0.10)
predMatrix[, c("id")] <- 0                        # id = 0
predMatrix[, c("country")] <- -2                  # country = -2
predMatrix[c("country","id"), "country"] <- 0     # id x country = 0
#In the predictor matrix, -2 denotes the class variable,
#a value 1 indicates a fixed effect and a value 2 indicates a random effect.


impMethod <- make.method(data = mhm, defaultMethod = "pmm")
impMethod[c("sex")] <- "polyreg"      
impMethod[c("education")] <- "polyreg"
impMethod[c("relationship")] <- "polyreg"
impMethod[c("meddiagnosis")] <- "logreg"
impMethod[c("mhseeking")] <- "logreg"
impMethod[c("childtrauma")] <- "logreg"
impMethod[c("adulttrauma")] <- "logreg"



imp_overall <- mice(mhm, method = impMethod,
                    predictorMatrix = predMatrix,
                    maxit = 5,
                    m = 5,
                    seed = 111)


save(imp_overall, file = "imp_overall.RData")


# 
# 
# 
# densityplot(imp_overall)
# plot(imp_overall)
# View(complete(imp_overall,2))
# summary(complete(imp_overall,1))
# 
# #randomly select 1 dataset for plotting purposes
# floor(runif(1, min=0, max=5))
# #[1] 4
# imp_full.4 <- complete(imp_overall,4)
# summary(imp_full.4)

imp_overall_long <- complete(imp_overall, action = 'long', include = TRUE)


save(imp_overall_long, file = "imp_overall_long.RData")








library(MatchThem) 
library(survey) 
library(CBPS)
library(cobalt)
library(knitr)
library(WeightIt)


weightdat_overall <- weightthem(PA ~  
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
                                estimand = "ATT",
                                trim = 0.99) 

save(weightdat_overall, file = "weightdat_overall.RData")



des_cbps <- svydesign(ids = ~country, weights = ~1, 
                                  data = imp_overall_long) 

mhq_cbps <- with(weightdat_overall, svyglm(mhq ~  PA,
                                              design = des_cbps,
                                              family = gaussian()))

kable(summary(pool(mhq_cbps), conf.int=T))
# |term        | estimate| std.error| statistic|       df| p.value|    2.5 %|   97.5 %|
# |:-----------|--------:|---------:|---------:|--------:|-------:|--------:|--------:|
# |(Intercept) | 63.58819| 0.2272083| 279.86741| 340383.0|       0| 63.14287| 64.03352|
# |PA          | 18.03639| 0.2745611|  65.69172| 341197.8|       0| 17.49826| 18.57452|




# making sure a slightly different code yields the same estimate for above
matched.models <- with(weightdat_overall,
                       svyglm(mhq ~ PA, family = gaussian()),
                       cluster = TRUE)
summary(pool(matched.models)) # yep, same estimates





mhq_cbps_doublerobust <- with(weightdat_overall, svyglm(mhq ~  PA
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
                                                        design = des_cbps,
                                                        family = gaussian()))

kable(summary(pool(mhq_cbps_doublerobust), conf.int=T))

# |term             |    estimate| std.error|  statistic|          df|   p.value|       2.5 %|      97.5 %|
#   |:--------------|-----------:|---------:|----------:|-----------:|---------:|-----------:|-----------:|
#   |(Intercept)    |  -4.3692217| 0.8538291|  -5.117209|  17040.7674| 0.0000003|  -6.0428148|  -2.6956285|
#   |PA             |  17.8738918| 0.2258155|  79.152620| 329220.4843| 0.0000000|  17.4312998|  18.3164837|





# SIMPLE LINEAR MODEL (no weights) -----------------------------------------------------




des_lm <- svydesign(ids = ~country, 
                      data = mhm) 

mhq_lm <- svyglm(mhq ~  PA
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
                        design = des_lm)

summary(mhq_lm)
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)      -3.2311     3.0760  -1.050  0.29483
# PA                18.0694     1.0739  16.825  < 2e-16
# age               11.5566     0.6762  17.089  < 2e-16
confint(mhq_lm)
#                      2.5 %       97.5 %
#   (Intercept)      -9.2975699   2.83540597
# PA                 15.9513466   20.18738800





