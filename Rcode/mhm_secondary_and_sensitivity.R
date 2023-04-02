library(WeightIt)
library(tidyverse)
library(survey)


load("mhm.RData")


# SECONDARY ANALYSIS: INTERACTIONS WITH GBM  ----------------------------------
sum(count(subset(mhm, age == 1)),
 count(subset(mhm, age == 2)),
 count(subset(mhm, age == 3)),
 count(subset(mhm, age == 4)),
 count(subset(mhm, age == 5)),
 count(subset(mhm, age == 6)),
 count(subset(mhm, age == 7)),
 count(subset(mhm, age == 8)))






gbm_interact1 <- weightit(PA ~ 
                           sex
                         + education
                         + employment
                         + relationship
                         + socialize
                         + sleep
                         + meddiagnosis
                         + mhseeking
                         + childtrauma
                         + adulttrauma,
                         subset(mhm, age==1),
                         method = "gbm",
                         estimand = "ATC",
                         trim.at = 0.99,
                         distribution = "gaussian")



save(gbm_interact1, file = "gbm_interact1.RData")



# 2 -----------------------------------------------------------------------





gbm_interact2 <- weightit(PA ~ 
                            sex
                          + education
                          + employment
                          + relationship
                          + socialize
                          + sleep
                          + meddiagnosis
                          + mhseeking
                          + childtrauma
                          + adulttrauma,
                          subset(mhm, age==2),
                          method = "gbm",
                          estimand = "ATC",
                          trim.at = 0.99,
                          distribution = "gaussian")



save(gbm_interact2, file = "gbm_interact2.RData")


# 3 -----------------------------------------------------------------------




gbm_interact3 <- weightit(PA ~ 
                            sex
                          + education
                          + employment
                          + relationship
                          + socialize
                          + sleep
                          + meddiagnosis
                          + mhseeking
                          + childtrauma
                          + adulttrauma,
                          subset(mhm, age==3),
                          method = "gbm",
                          estimand = "ATC",
                          trim.at = 0.99,
                          distribution = "gaussian")



save(gbm_interact3, file = "gbm_interact3.RData")


# 4 -----------------------------------------------------------------------




gbm_interact4 <- weightit(PA ~ 
                            sex
                          + education
                          + employment
                          + relationship
                          + socialize
                          + sleep
                          + meddiagnosis
                          + mhseeking
                          + childtrauma
                          + adulttrauma,
                          subset(mhm, age==4),
                          method = "gbm",
                          estimand = "ATC",
                          trim.at = 0.99,
                          distribution = "gaussian")



save(gbm_interact4, file = "gbm_interact4.RData")


# 5 -----------------------------------------------------------------------

gbm_interact5 <- weightit(PA ~ 
                            sex
                          + education
                          + employment
                          + relationship
                          + socialize
                          + sleep
                          + meddiagnosis
                          + mhseeking
                          + childtrauma
                          + adulttrauma,
                          subset(mhm, age==5),
                          method = "gbm",
                          estimand = "ATC",
                          trim.at = 0.99,
                          distribution = "gaussian")



save(gbm_interact5, file = "gbm_interact5.RData")




# 6 -----------------------------------------------------------------------


gbm_interact6 <- weightit(PA ~ 
                            sex
                          + education
                          + employment
                          + relationship
                          + socialize
                          + sleep
                          + meddiagnosis
                          + mhseeking
                          + childtrauma
                          + adulttrauma,
                          subset(mhm, age==6),
                          method = "gbm",
                          estimand = "ATC",
                          trim.at = 0.99,
                          distribution = "gaussian")



save(gbm_interact6, file = "gbm_interact6.RData")


# 7 -----------------------------------------------------------------------



gbm_interact7 <- weightit(PA ~ 
                            sex
                          + education
                          + employment
                          + relationship
                          + socialize
                          + sleep
                          + meddiagnosis
                          + mhseeking
                          + childtrauma
                          + adulttrauma,
                          subset(mhm, age==7),
                          method = "gbm",
                          estimand = "ATC",
                          trim.at = 0.99,
                          distribution = "gaussian")



save(gbm_interact7, file = "gbm_interact7.RData")





# 8 -----------------------------------------------------------------------

gbm_interact8 <- weightit(PA ~ 
                            sex
                          + education
                          + employment
                          + relationship
                          + socialize
                          + sleep
                          + meddiagnosis
                          + mhseeking
                          + childtrauma
                          + adulttrauma,
                          subset(mhm, age==8),
                          method = "gbm",
                          estimand = "ATC",
                          trim.at = 0.99,
                          distribution = "gaussian")



save(gbm_interact8, file = "gbm_interact8.RData")





# MHQ INTERACTION ---------------------------------------------------------


# 
# 
# des_int <- svydesign(ids = ~country, weights = gbm_interact$weights,
#                  data = mhm)
# 
# 
# 
# mhq_interact <- svyglm(mhq ~ PA*age,
#                       design = des_int,
#                       family = gaussian())
# 
# 
# summary(mhq_interact)
# # Coefficients:
# #              Estimate Std. Error t value Pr(>|t|)    
# # (Intercept)    4.3888     3.7979   1.156 0.249093    
# #   PA          25.6385     2.6505   9.673  < 2e-16 ***
# #   age          14.9358     0.7656  19.509  < 2e-16 ***
# #   PA:age       -2.0408     0.5796  -3.521 0.000522 *** 
# confint(mhq_interact)
# #                    2.5 %     97.5 %
# #   (Intercept)   -3.095626 11.8731520
# # PA              20.415341 30.8617081
# # age             13.427074 16.4445412
# # PA:age          -3.183107 -0.8985421
# 
# library(interactions)
# interactions::interact_plot(mhq_interact,
#                             pred = age, modx = PA)
# # shown in pdf
# 


# CORE COGNITION INTERACTION ----------------------------------------------
# 
# 
# 
# cog_interact <- svyglm(cog ~ PA*age,
#                        design = des_int,
#                        family = gaussian())
# 
# 
# summary(cog_interact)
# #   Estimate Std. Error t value Pr(>|t|)    
# #   (Intercept)  23.3558     2.9466   7.926 1.07e-13 ***
# #   PA           23.9955     2.3172  10.355  < 2e-16 ***
# #   age          13.7309     0.6392  21.482  < 2e-16 ***
# #   PA:age       -2.0901     0.5115  -4.086 6.12e-05 ***
# confint(cog_interact)
# #               2.5 %      97.5 %
# # (Intercept) 17.549114   29.162417
# # PA          19.429110   28.561956
# # age         12.471257   14.990448
# # PA:age      -3.098151   -1.082094
# 
# 
# 
# # ADAPT RESILIENCE INTERACTION --------------------------------------------
# 
# 
# 
# adaptresil_interact <- svyglm(adaptresil ~ PA*age,
#                        design = des_int,
#                        family = gaussian())
# 
# summary(adaptresil_interact)
# #   Coefficients:
# #   Estimate Std. Error t value Pr(>|t|)    
# #   (Intercept)  37.7082     3.3839  11.143  < 2e-16 ***
# #   PA           21.6298     2.6617   8.126 3.03e-14 ***
# #   age          11.8276     0.7826  15.113  < 2e-16 ***
# #   PA:age       -1.1635     0.5790  -2.009   0.0457 * 
# confint(adaptresil_interact)
# #                  2.5 %      97.5 %
# # (Intercept) 31.039741    44.37673765
# # PA          16.384572    26.87508034
# # age         10.285315    13.36988039
# # PA:age      -2.304504   -0.02240528
# 
# 
# 
# # MOOD AND OUTLOOK INTERACTION --------------------------------------------
# 
# moodoutlook_interact <- svyglm(moodoutlook ~ PA*age,
#                               design = des_int,
#                               family = gaussian())
# 
# summary(moodoutlook_interact)
# #                  Estimate Std. Error t value Pr(>|t|)    
# #    (Intercept)   6.0473     3.4598   1.748 0.081858 .  
# #   PA           23.3328     2.5859   9.023  < 2e-16 ***
# #   age          14.5869     0.6836  21.337  < 2e-16 ***
# #   PA:age       -2.0453     0.5881  -3.478 0.000607 ***
# confint(moodoutlook_interact)
# #                  2.5 %     97.5 %
# #   (Intercept) -0.7707047   12.8653970
# # PA            18.2368748   28.4288105
# # age           13.2396330   15.9341062
# # PA:age        -3.2042770   -0.8864145
# 
# 
# 
# # DRIVE AND MOTIVATION INTERACTION ----------------------------------------
# 
# drivemotiv_interact <- svyglm(drivemotiv ~ PA*age,
#                                design = des_int,
#                                family = gaussian())
# 
# summary(drivemotiv_interact)
# #                Estimate Std. Error t value Pr(>|t|)    
# # (Intercept)    27.8938     3.4279   8.137 2.82e-14 ***
# #   PA           23.8058     2.8727   8.287 1.08e-14 ***
# #   age          13.0728     0.8971  14.573  < 2e-16 ***
# #   PA:age       -2.1501     0.6424  -3.347 0.000958 ***
# confint(drivemotiv_interact)
# #                  2.5 %    97.5 %
# #   (Intercept) 21.138597   34.648941
# # PA            18.144754   29.466836
# # age           11.305025   14.840630
# # PA:age        -3.415949   -0.884159
# 
# 
# 
# # SOCIAL SELF INTERACTION -------------------------------------------------
# 
# socialself_interact <- svyglm(socialself ~ PA*age,
#                               design = des_int,
#                               family = gaussian())
# 
# summary(socialself_interact)
# #                 Estimate Std. Error t value Pr(>|t|)    
# #   (Intercept)  13.6556     4.3417   3.145  0.00189 ** 
# #   PA           16.8927     2.7404   6.164 3.28e-09 ***
# #   age          13.9698     0.8524  16.389  < 2e-16 ***
# #   PA:age       -1.0527     0.5798  -1.815  0.07079 . 
# confint(socialself_interact)
# #                  2.5 %        97.5 %
# #   (Intercept)  5.099643      22.21154227
# # PA             11.492352     22.29297674
# # age            12.290075     15.64962073
# # PA:age        -2.195314      0.08997785
# interactions::interact_plot(socialself_interact,
#                             pred = age, modx = PA)
# 
# 
# 
# # MIND BODY INTERACTION lol -----------------------------------------------
# 
# mindbody_interact <- svyglm(mindbody ~ PA*age,
#                          design = des_int,
#                          family = gaussian())
# 
# summary(mindbody_interact)
# #   Estimate Std. Error t value Pr(>|t|)    
# # (Intercept)  26.8849     3.1432   8.553 1.93e-15 ***
# #   PA           28.3230     2.7484  10.305  < 2e-16 ***
# #   age           9.7027     0.8022  12.095  < 2e-16 ***
# #   PA:age       -2.2009     0.6556  -3.357 0.000927 ***
# confint(mindbody_interact)
# #                  2.5 %         97.5 %
# #   (Intercept) 20.690722    33.0790530
# #   PA          22.906916    33.7391577
# #   age          8.121834    11.2836220
# #   PA:age      -3.492921    -0.9088769
# 
# 
# 

# SENSITIVITY ANALYSIS: MI + CBPS -----------------------------------------


library(broom.mixed)
library(MCMCglmm)
library(msm)
library(tmvtnorm)
install.packages("https://cran.r-project.org/src/contrib/Archive/linLIR/linLIR_1.1.tar.gz",
                 repos=NULL, method="libcurl")
install.packages("https://cran.r-project.org/src/contrib/Archive/hmi/hmi_1.0.0.tar.gz",
                 repos=NULL, method="libcurl")
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
                                estimand = "ATC",
                                trim = 0.99) 

save(weightdat_overall, file = "weightdat_overall.RData")




bal.tab(weightdat_overall,
        stats = c("m"),
        s.d.denom = "control",
        thresholds = c(m = .01))


love.plot(weightdat_overall, binary = "std", var.order = "un", stats = "m",
          thresholds = .01) + theme(legend.position = "top")









des_cbps <- svydesign(ids = ~country, weights = ~1, 
                                  data = imp_overall_long) 

mhq_cbps <- with(weightdat_overall, svyglm(mhq ~  PA,
                                              design = des_cbps,
                                              family = gaussian()))

kable(summary(pool(mhq_cbps), conf.int=T))
# |term        | estimate| std.error| statistic|       df| p.value|    2.5 %|   97.5 %|
# |:-----------|--------:|---------:|---------:|--------:|-------:|--------:|--------:|
# |PA          | 18.15347| 0.2756795|   65.8499| 276060.1|       0| 17.61315| 18.69379|



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
#   |PA             |  17.8668408| 0.2229397|  80.142037| 226896.0008| 0.0000000|  17.4298847|  18.3037969|








# mi + gbm ----------------------------------------------------------------


weightdat_gbm       <- weightthem(PA ~  
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
                                method = "gbm",    
                                estimand = "ATC",
                                trim = 0.99) 


save(weightdat_gbm, file='weightdat_gbm.rdata')

load('weightdat_gbm.RData')


des_mi_gbm <- svydesign(ids = ~country, weights = ~1, 
                      data = imp_overall_long) 

mhq_mi_gbm <- with(weightdat_gbm, svyglm(mhq ~  PA,
                                           design = des_mi_gbm,
                                           family = gaussian()))

kable(summary(pool(mhq_mi_gbm), conf.int = T))


# |term        | estimate| std.error| statistic|       df| p.value|    2.5 %|   97.5 %|
# |:-----------|--------:|---------:|---------:|--------:|-------:|--------:|--------:|
# |(Intercept) | 63.65506| 0.2449686| 259.84991| 4290.462|       0| 63.17480| 64.13533|
# |PA          | 17.75436| 0.2754886|  64.44682| 189117.5|       0| 17.21441| 18.29431|

mi_gbm_doublerobust <- with(weightdat_gbm, svyglm(mhq ~  PA
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
                                                        design = des_mi_gbm,
                                                        family = gaussian()))

kable(summary(pool(mi_gbm_doublerobust), conf.int=T))


# |term        |   estimate| std.error|  statistic|          df|   p.value|       2.5 %|      97.5 %|
# |:-----------|----------:|---------:|----------:|-----------:|---------:|-----------:|-----------:|
# |PA          |  17.770391| 0.2255080|  78.801587| 194756.9779| 0.0000000|  17.3284011|  18.2123818|



