library(tidyverse)
library(MatchThem)
library(mice)
library(naniar)

load("temp_data1.RData")   
#load the multiply imputed data set (5 iterations, 5 imputations)
#with all outcomes and all covariates
midat <- complete(temp_data1, action = "long") 
#extract dataframe from mids in long format to paste covariate equation
covs <- midat %>%
  select(sex, identity.diff, education, employment, relationship.status, sleep,
         socialize, disability, seek.mh.treatment, childhood.trauma)

# do we include variables that can be affected by exposure?

ps_eq <- paste("exercise ~ ", paste(names(covs), collapse = " + "))
ps_eq <- as.formula(ps_eq)   
### this step was just to make it so i didn't have to type in entire equation
### into matchthem/weightthem. will probably delete this later



# matchdat <- matchthem(ps_eq, temp_data,
#           approach = "within",
#           method = "nearest",
#           distance = "logit"
#           )

#save(matchdat, file = "matchdat.RData")
load("matchdat.RData")



###### estimate weights (which will function as survey weights) using IPW
## inverse probability weighting uses logistic regression to estimate probability
## of the exposure observed for a subject, this probability (propensity score)
## is used as a quasi-survey weight in further analysis

weightdat <- weightthem(ps_eq, temp_data1,
                         approach = "within",
                        method = "ps",    #ps = propensity score weighting using generalized linear models
                        estimand = "ATT"
                        )

save(weightdat, file = "weightdat.RData")
load("weightdat.RData")

weightdatlong <- complete(weightdat, action = "long")


#checking balance
library(cobalt)

bal.tab(weightdat, stats = c("m", "ks"),
       imp.fun = "max" )
# largest ASMD and KS should be close to zero

love.plot(weightdat, binary = "std", 
         var.order = "un", stats = c("m", "ks"),
         thresholds = c(.10, .05)) + theme(legend.position = "top")

#looks good i guess
library(survey)
library(knitr)
mhqweightmodel <- with(weightdat,      #########ADD COVARIATES?
                svyglm(mhq ~ exercise),  #survey weighted generalized linear model
                cluster = T)     #family = for continuous outcome?

mhqweightresults <- pool(mhqweightmodel)
summary(mhqweightresults, conf.int = T)  %>%
  kable(digits = 3)
# |term        | estimate| std.error| statistic|        df| p.value|  2.5 %| 97.5 %|
#   |:-----------|--------:|---------:|---------:|---------:|-------:|------:|------:|
#   |(Intercept) |   20.018|     0.470|    42.627|  7413.007|       0| 19.097| 20.938|
#   |exercise1   |   20.548|     0.758|    27.124| 25155.135|       0| 19.063| 22.033|



mhqweightmodel1 <- with(weightdat,
                       svyglm(mhq ~ exercise
                              + sex
                              + identity.diff
                              + education
                              + employment
                              + relationship.status
                              + sleep
                              + disability
                              + seek.mh.treatment
                              + childhood.trauma),
                       cluster = T)

mhqweightresults1 <- pool(mhqweightmodel1)
summary(mhqweightresults1, conf.int = T)  %>%
  kable(digits = 3)

#intercept = 39.420
#exercise = 19.213  (17.918-20.507)


#multi level model attempt

des <- svydesign(ids = ~country, 
                 weights = ~weights, #variable with weights from weightthat
                 nest = T,
                 data = weightdatlong)



mhqweightmodel2 <- with(weightdat,
                        svyglm(mhq ~ exercise
                               + sex
                               + identity.diff
                               + education
                               + employment
                               + relationship.status
                               + sleep
                               + disability
                               + seek.mh.treatment
                               + childhood.trauma,
                               family = gaussian(),
                    design = des))
                       

mhqweightresults2 <- pool(mhqweightmodel2)
summary(mhqweightresults2, conf.int = T)  %>%
  kable(digits = 3)

summary(mhqweightmodel2)





########################
library(lme4)
mlm <- glmer(mhq ~ exercise 
             + sex
             + identity.diff
             + education
             + employment
             + relationship.status
             + sleep
             + disability
             + seek.mh.treatment
             + childhood.trauma + (1 | country), 
             data = weightdatlong)




library(multilevelPSA)











core.cogweightmodel <- with(weightdat,
                            svyglm(core.cog ~ exercise),
                            cluster = T)
core.cogweightresults <- pool(core.cogweightmodel)
summary(core.cogweightresults, conf.int = T) %>%
  kable(digits = 3)
# |term        | estimate| std.error| statistic|        df| p.value|  2.5 %| 97.5 %|
#   |:-----------|--------:|---------:|---------:|---------:|-------:|------:|------:|
#   |(Intercept) |   37.156|     0.428|    86.818|  9118.391|       0| 36.317| 37.995|
#   |exercise1   |   20.557|     0.690|    29.801| 27733.115|       0| 19.205| 21.910|
 

core.cogweightmodel1 <- with(weightdat,
                             svyglm(core.cog ~ exercise
                                    + sex
                                    + identity.diff
                                    + education
                                    + employment
                                    + relationship.status
                                    + sleep
                                    + disability
                                    + seek.mh.treatment
                                    + childhood.trauma),
                             cluster = T)

core.cogweightresults1 <- pool(core.cogweightmodel1)
summary(core.cogweightresults1, conf.int = T) %>%
  kable(digits = 3)

#intercept = 50.328
#exercise = 19.414  (18.191-20.636)





complex.cogweightmodel <- with(weightdat,
                                 svyglm(complex.cog ~ exercise),
                                 cluster = T)
complex.cogweightresults <- pool(complex.cogweightmodel)
summary(complex.cogweightresults, conf.int = T)  %>%
  kable(digits = 3)
# |term        | estimate| std.error| statistic|       df| p.value|  2.5 %| 97.5 %|
#   |:-----------|--------:|---------:|---------:|--------:|-------:|------:|------:|
#   |(Intercept) |   45.811|     0.463|    98.970| 11317.81|       0| 44.904| 46.718|
#   |exercise1   |   23.718|     0.722|    32.838| 29216.04|       0| 22.302| 25.133|



complex.cogweightmodel1 <- with(weightdat,
                                svyglm(complex.cog ~ exercise
                                       + sex
                                       + identity.diff
                                       + education
                                       + employment
                                       + relationship.status
                                       + sleep
                                       + disability
                                       + seek.mh.treatment
                                       + childhood.trauma),
                                cluster = T)

complex.cogweightresults1 <- pool(complex.cogweightmodel1)
summary(complex.cogweightresults1, conf.int = T)  %>%
  kable(digits = 3)

# intercept = 67.159
# exercise = 22.582 (21.290-23.875)




drive.motivationweightmodel <- with(weightdat,
                                    svyglm(drive.motivation ~ exercise),
                                    cluster = T)
drive.motivationweightresults <- pool(drive.motivationweightmodel)
summary(drive.motivationweightresults, conf.int = T)   %>%
  kable(digits = 3)
# |term        | estimate| std.error| statistic|       df| p.value|  2.5 %| 97.5 %|
#   |:-----------|--------:|---------:|---------:|--------:|-------:|------:|------:|
#   |(Intercept) |   41.475|     0.440|    94.198| 11466.99|       0| 40.612| 42.338|
#   |exercise1   |   20.538|     0.699|    29.364| 30043.56|       0| 19.167| 21.909|



drive.motivationweightmodel1 <- with(weightdat,
                                     svyglm(drive.motivation ~ exercise
                                            + sex
                                            + identity.diff
                                            + education
                                            + employment
                                            + relationship.status
                                            + sleep
                                            + disability
                                            + seek.mh.treatment
                                            + childhood.trauma),
                                     cluster = T)


drive.motivationweightresults1 <- pool(drive.motivationweightmodel1)
summary(drive.motivationweightresults1, conf.int = T)   %>%
  kable(digits = 3)

# intercept = 53.376
# exercise= 19.364 (18.130 - 20.598)









mood.outlookweightmodel <- with(weightdat,
                                 svyglm(mood.outlook ~ exercise),
                                 cluster = T)
mood.outlookweightresults <- pool(mood.outlookweightmodel)
summary(mood.outlookweightresults, conf.int = T)   %>%
  kable(digits = 3)
# |term        | estimate| std.error| statistic|       df| p.value|  2.5 %| 97.5 %|
#   |:-----------|--------:|---------:|---------:|--------:|-------:|------:|------:|
#   |(Intercept) |   24.065|     0.447|    53.845| 10207.78|       0| 23.189| 24.941|
#   |exercise1   |   16.979|     0.724|    23.458| 29318.29|       0| 15.560| 18.398|


mood.outlookweightmodel1 <- with(weightdat,
                                 svyglm(mood.outlook ~ exercise
                                        + sex
                                        + identity.diff
                                        + education
                                        + employment
                                        + relationship.status
                                        + sleep
                                        + disability
                                        + seek.mh.treatment
                                        + childhood.trauma),
                                 cluster = T)

mood.outlookweightresults1 <- pool(mood.outlookweightmodel1)
summary(mood.outlookweightresults1, conf.int = T)   %>%
  kable(digits = 3)

# intercept = 42.770
# exercise = 15.679 (14.440 - 16.919)







social.selfweightmodel <- with(weightdat,
                               svyglm(social.self ~ exercise),
                               cluster = T)
social.selfweightresults <- pool(social.selfweightmodel)
summary(social.selfweightresults, conf.int = T)  %>%
  kable(digits = 3)
# |term        | estimate| std.error| statistic|        df| p.value|  2.5 %| 97.5 %|
#   |:-----------|--------:|---------:|---------:|---------:|-------:|------:|------:|
#   |(Intercept) |   23.453|     0.484|    48.422|  6681.254|       0| 22.503| 24.402|
#   |exercise1   |   13.567|     0.775|    17.508| 23490.420|       0| 12.048| 15.086|




social.selfweightmodel1 <- with(weightdat,
                                svyglm(social.self ~ exercise
                                       + sex
                                       + identity.diff
                                       + education
                                       + employment
                                       + relationship.status
                                       + sleep
                                       + disability
                                       + seek.mh.treatment
                                       + childhood.trauma),
                                cluster = T)

social.selfweightresults1 <- pool(social.selfweightmodel1)
summary(social.selfweightresults1, conf.int = T)  %>%
  kable(digits = 3)


#intercept = 43.474
# exercise = 12.223 (10.860 - 13.587)








mind.bodyweightmodel <- with(weightdat,
                             svyglm(mind.body ~ exercise),
                             cluster = T)
mind.bodyweightresults <- pool(mind.bodyweightmodel)
summary(mind.bodyweightresults, conf.int = T)  %>%
  kable(digits = 3)
# |term        | estimate| std.error| statistic|       df| p.value|  2.5 %| 97.5 %|
#   |:-----------|--------:|---------:|---------:|--------:|-------:|------:|------:|
#   |(Intercept) |   42.448|     0.425|    99.941| 12132.05|       0| 41.615| 43.281|
#   |exercise1   |   23.113|     0.683|    33.844| 31162.26|       0| 21.774| 24.452|



mind.bodyweightmodel1 <- with(weightdat,
                              svyglm(mind.body ~ exercise
                                     + sex
                                     + identity.diff
                                     + education
                                     + employment
                                     + relationship.status
                                     + sleep
                                     + disability
                                     + seek.mh.treatment
                                     + childhood.trauma),
                              cluster = T)


mind.bodyweightresults1 <- pool(mind.bodyweightmodel1)
summary(mind.bodyweightresults1, conf.int = T)  %>%
  kable(digits = 3)


#intercept = 66.240
# exercise = 21.947 (20.799-23.095)












