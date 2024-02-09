library(WeightIt)
library(tidyverse)
library(survey)


load("mhm.RData")




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



