library(tidyverse)
library(survey)
library(WeightIt)
library(cobalt)

load("mhm.RData")

load("weightit_overall_gbm.Rdata")
load("weightit_overall_gbm_binary.Rdata")

load("weightdat_overall.RData")



mhm <- mhm %>%
  mutate(PA_cont = as.integer(mhm$PA)-1)

### check weights and balance
weightit_overall_gbm
summary(weightit_overall_gbm)


bal.tab(weightit_overall_gbm, stats = c("m", "v"), thresholds = c(m = .05))
# Balance tally for target mean differences
# count
# Balanced, <0.05        48
# Not Balanced, >0.05     0

gbm_des <- svydesign(ids = ~country, weights = weightit_overall_gbm$weights,
                                 data = mhm)


gbm_cont_model <- svyglm(mhq ~ PA_cont,
               design = gbm_des)

summary(gbm_cont_model)

# Call:
#   svyglm(formula = mhq ~ PA_cont, design = gbm_des)
# 
# Survey design:
#   svydesign(ids = ~country, weights = weightit_overall_gbm$weights, 
#             data = mhm)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  53.9956     2.7676   19.51   <2e-16 ***
#   PA_cont       6.7374     0.5372   12.54   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 5156.082)
# 
# Number of Fisher Scoring iterations: 2







############################### BINARY ###########


mhm <- mhm %>%
  mutate(PA_binary = case_when(PA == "Rarely/Never" ~ 0,
                               PA == "Less than once a week" ~ 0,
                               PA == "Once a week" ~ 0,
                               PA == "Few days a week" ~ 1,
                               PA == "Every day" ~ 1))


### check weights and balance
weightit_overall_gbm_binary
summary(weightit_overall_gbm_binary)


bal.tab(weightit_overall_gbm_binary, stats = c("m", "v"), thresholds = c(m = .05))
# Balance tally for target mean differences
# count
# Balanced, <0.05        49
# Not Balanced, >0.05     0

gbm_des_binary <- svydesign(ids = ~country, weights = weightit_overall_gbm_binary$weights,
                     data = mhm)


gbm_binary_model <- svyglm(mhq ~ PA_binary,
                         design = gbm_des_binary)

summary(gbm_binary_model)

# Call:
#   svyglm(formula = mhq ~ PA_binary, design = gbm_des_binary)
# 
# Survey design:
#   svydesign(ids = ~country, weights = weightit_overall_gbm_binary$weights, 
#             data = mhm)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   71.545      2.628   27.22   <2e-16 ***
#   PA_binary     14.774      1.408   10.49   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 5001.8)
# 
# Number of Fisher Scoring iterations: 2





library(twang)
load("mhm.RData")

str(mhm$PA)

gbm_mod <- mnps(PA ~ age + sex + country + education + employment +
                relationship + socialize + sleep + meddiagnosis +
                mhseeking + childtrauma + adulttrauma,
              data = mhm,
              n.trees = 5000,
              interaction.depth = 3,
              shrinkage = 0.01,
              estimand = "ATE",
              stopMethods = c("es.max"),
              version = "xgboost",
              verbose = T)

# Diagnosis of unweighted analysis
# Calculating standardized differences
# Optimizing with es.max.ATE stopping rule
# Optimized at 4997 
# Diagnosis of es.max.ATE weights
























####### MI + WEIGHTING ###############################




load("weightdat_overall.RData")

des <- svydesign(ids = ~country, weights = ~1, 
                                  data = imp_overall_long) 






















