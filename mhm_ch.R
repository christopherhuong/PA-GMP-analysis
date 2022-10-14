library(tidyverse)


dat1 <- read.csv("mhm_data_2022-07-21_19-46-52.csv", 
                 stringsAsFactors = T, na.strings = "")


#############################################################


tapply(dat1$Country, dat1$Household.Income, summary)
#check summary statistics by group. seems like only US, india, and germany have income data





###### select relevant variables and rename to new DF. probably a cleaner way to do this
dat1$mhq <- dat1$Overall.MHQ       #rename Overall.MHQ -> mhq
mhm <- select(dat1, mhq)          #create new df with column mhq


mhm <- mhm %>%                    #add and rename relevant variables to new df
  add_column(
             core.cog = dat1$Core.Cognition,
             complex.cog = dat1$Complex.Cognition,
             drive.motivation = dat1$Drive...Motivation,
             mood.outlook = dat1$Mood...Outlook,
             social.self = dat1$Social...Self,
             mind.body = dat1$Mind.Body.Connection,
             exercise = dat1$How.regularly.to.you.engage.in.physical.exercise..30.minutes.or.more..,
             sex = dat1$Biological.Sex,
             identity.diff = dat1$Is.identity.different.from.biological.sex,
             country = dat1$Country,
             education = dat1$Education,
             employment = dat1$Employment,
             ethnicity = dat1$Ethnicity,
             relationship.status = dat1$What.is.your.current.family.situation.,
             sleep = dat1$In.general..I.get.as.much.sleep.as.I.need.,
             socialize = dat1$How.regularly.do.you.socialize.with.friends.in.person.,
             disability = dat1$Do.you.have.a.diagnosed.medical.disorder.that.significantly.impacts.your.way.of.life.,
             seek.mh.treatment = dat1$Are.you.currently.seeking.treatment.for.any.mental.health.concerns.,
             childhood.trauma = dat1$Chilhood.traumas)


summary(mhm)

### data recoding
mhm[mhm == "Prefer not to say"] <- NA   

mhm$exercise[mhm$exercise %in% "Einmal in der Woche"] <- "Once a week"

 

mhm <- mhm %>% mutate(exercise = case_when(exercise == "Rarely/Never" ~ "No",
                                           exercise == "Less than once a week" ~ "No",
                                           exercise == "Once a week" ~ "No",
                                           exercise == "Few days a week" ~ "Yes",
                                           exercise == "Every day" ~ "Yes"
                                                                               ))

mhm$exercise <- ifelse(mhm$exercise == "Yes", "1", "0")  
mhm$exercise <- as.numeric(mhm$exercise)
nrow(mhm[mhm$exercise == 1,])
nrow(mhm[mhm$exercise == 0,])
##    0(No)   1(Yes) 
### 27416 15811



mhm[mhm == "|Prefer not to say"] <- NA  
### due to age of sample, only use childhood trauma and leave out adult trauma?
mhm$childhood.trauma <- 
  if_else((mhm$childhood.trauma == "|I did not experience any of the above during my childhood")|(mhm$childhood.trauma == "|None of the above"), 
          "No", "Yes")
mhm$childhood.trauma <- as.factor(mhm$childhood.trauma)
#  No   Yes  NA's 
#11817 29506  1904 




mhm <- mhm %>%                 #rename
  mutate(education = case_when(education == "Some High School" ~ "less.hs",
                               education == "Primary Education" ~ "less.hs",
                               education == "High School" ~ "hs",
                               education == "Vocational certification" ~ "vocational",
                               education == "Associateâ€™s Degree" ~ "assoc.deg",
                               education == "Bachelor's Degree" ~ "bach.deg",
                               education == "Master's Degree" ~ "grad.deg",
                               education == "Ph.D. or higher" ~ "grad.deg",
                               education == "J.D" ~ "grad.deg",
                               education == "M.D." ~ "grad.deg"))
  
mhm$education <- as.factor(mhm$education)
# mhm$education[mhm$education == "Other"] <- NA
# any variable value not included in case_when automatically becomes NA?

mhm <- mhm %>%               #reorder
  mutate(education = fct_relevel(education, "less.hs",
                                 "hs",
                                 "vocational",
                                 "assoc.deg",
                                 "bach.deg",
                                 "grad.deg"
                                 )) 

#less.hs         hs vocational  assoc.deg   bach.deg   grad.deg       NA's 
#3440      17102       1159       2153      12288       2115       4970 
 



##################### MLM ################
library(lme4)
library(nlme)


lm <-lm(mhq ~ exercise
            + sex
            + country
            + identity.diff
            + education
            + employment
            + relationship.status
            + sleep
            + disability
            + seek.mh.treatment
            + childhood.trauma,

            data = mhm)



#check linearity and normality
standardized <- rstudent(lm)
qqnorm(standardized)
abline(0,1) 

hist(standardized)



### intraclass correlation = variance accounted for by grouping variable
# used to justify MLM. cut off to use MLM is ICC higher than 0.1
library(psychometric)
ICC <- ICC1.lme(mhq, country, mhm)
ICC  #about 10% of variance
1-ICC


#compare random intercept model to fixed intercept model
#if random intercept model fits better, need MLM


#fixed intercept model
model1 <- gls(mhq ~ 1,
              data = mhm,
              method = "ML",
              na.action = na.omit)

summary(model1)
#intercept = average y score, significantly different from 0 (pvalue)


model2 <- lme(mhq ~ 1,
              data = mhm,
              method = "ML",
              na.action = "na.omit",
              random = ~1|country)   ##allows different intercept across countries
summary(model2)

anova(model1, model2)
# significantly different, model 2 better fit, go ahead with MLM





pa_plot <- lmer(mhq ~ exercise 
                + sex
                + identity.diff
                + education
                + employment
                + relationship.status
                + sleep
                + disability
                + seek.mh.treatment
                + childhood.trauma + (1 | country), data = mhm)

anova(pa_plot) 





### multiple imputation + matching and weighting (have to check for assumptions first) ##
# https://meghapsimatrix.com/post/mi_ps/
# https://journal.r-project.org/archive/2021/RJ-2021-073/RJ-2021-073.pdf
#That score is defined as the probability that a unit in the full
# sample receives the treatment, given a set of observed variables. If all
# information relevant to participation and outcomes is observable to the
# researcher, the propensity score will produce valid matches for
# estimating the impact of an intervention.

library(MatchThem)
library(mice)
library(naniar)

gg_miss_var(mhm, show_pct = T)
# percent missing by variable

mhm1 <- mhm %>%  #new df with outcomes + covariates left out country and ethnicity for sake of MI
  select(mhq, core.cog, complex.cog, drive.motivation, mood.outlook, social.self, mind.body,
         exercise, sex, identity.diff, country, education, employment, relationship.status, sleep,
         socialize, disability, seek.mh.treatment, childhood.trauma
         )


#system.time(temp_data <- mice(mhm1, m = 5, maxit = 5, seed = 1111))
#system.time(temp_data1 <- mice(mhm1, m = 3, maxit = 3, seed = 1112))
# default mice imputation methods

#save(temp_data, file = "temp_data.RData")
#save(temp_data1, file = "temp_data1.Rdata")



load("temp_data.RData")

imp <- complete(temp_data, action = "long")
### new imputed dataset, 5 imputations

##################################
covs <- imp %>%
  select(sex, identity.diff, education, employment, relationship.status, sleep,
         socialize, disability, seek.mh.treatment, childhood.trauma)


ps_eq <- paste("exercise ~ ", paste(names(covs), collapse = " + "))
ps_eq
# character string of propensity score model equation by grouping variable (exercise)


#function that runs logistic regression on pasted equation and creates column for 
# logit of propensity score and propensity score
estimate_ps <- function(equation, dat){
  
  ps_model <- glm(as.formula(equation), family = binomial, data = dat)
  
  dat <- dat %>%
    mutate(ps_logit = predict(ps_model, type = "link"),
           ps = predict(ps_model, type = "response"))
  
  return(dat)
  
}

# groups by imputation number, runs above function on each imputed datatset
imp_dat_ps <- imp %>%
  group_by(.imp) %>%
  do(estimate_ps(ps_eq, .)) %>%
  ungroup()










## combining propensity scores from imputed data sets to estimate ATT
    ## across = averaging scores from across data sets to calculate weights
    ## within = running analysis with scores from each data set

imp_dat_ps$exercise <- as.numeric(paste(imp_dat_ps$exercise))





imp_dat_ps <- imp_dat_ps %>%
  group_by(.id) %>%
  mutate(ps_across = mean(ps)) %>%
  ungroup() %>%
  mutate(att_wt_across = exercise + (1 - exercise) * ps_across/(1 - ps_across),
         att_wt_within = exercise + (1 - exercise) * ps/(1 - ps))


imp_dat_ps %>%
  select(.imp, ps, ps_across, att_wt_across, att_wt_within)

#calculating weights using across and within method



imp_dat_ps %>%
  mutate(drop = if_else(exercise == 1, "Non-exercisers", "Exercisers"),
         ps_across_logit = log(ps_across/ (1 - ps_across))) %>%
  filter(.imp == 1) %>%
  ggplot(aes(x = ps_across_logit, fill = drop)) +
  geom_density(alpha = .5) + 
  labs(x = "Logit Propensity Scores", y = "Density", fill = "") + 
  ggtitle("Common Support: Across Method") +
  theme_bw()


# propensity scores for both groups overlap
# satisfying 'common support assumption' for across method




imp_dat_ps %>%
  mutate(drop = if_else(exercise == 1, "Non-exercisers", "Exercisers")) %>%
  ggplot(aes(x = ps_logit, fill = drop)) +
  geom_density(alpha = .5) + 
  labs(x = "Logit Propensity Scores", y = "Density", fill = "") + 
  ggtitle("Common Support: Within Method") + 
  facet_wrap(~ .imp, ncol = 2) + 
  theme_bw()

# propensity scores overlap for each imputation, satisfying common support assumption
# for within method



####       check balances on MI data sets ###########
library(cobalt)





#### estimating average treatment effect on the treated (ATT)
library(estimatr)
library(broom)
library(knitr)

estimate_ATT <- function(equation, dat, wts){
  
  wts <- dat %>% pull({{wts}})
  
  model <- lm_robust(as.formula(equation), data = dat, weights = wts)
  
  res <- model %>%
    tidy() %>%
    filter(term == "exercise") %>%
    select(term, estimate, se = std.error, ci_low = conf.low, ci_high = conf.high, df = df)
  
  return(res)
}

#function that estimates ATT

equation_ancova <- paste("mhq ~ exercise + ", paste(names(covs), collapse = " + "))
equation_ancova
# paste model equation


across_res <- imp_dat_ps %>%
  group_by(.imp) %>%
  do(estimate_ATT(equation = equation_ancova, dat = ., wts = att_wt_across)) %>%
  ungroup()

within_res <- imp_dat_ps %>%
  group_by(.imp) %>%
  do(estimate_ATT(equation = equation_ancova, dat = ., wts = att_wt_within)) %>%
  ungroup()
#run regression function using weights from across and within methods on each imputed data set


###### pooling results #########


calc_pooled <- function(dat, est, se, df){
  
  dat <- dat %>%
    mutate(est = dat %>% pull({{est}}),
           se = dat %>%pull({{se}}),
           df = dat %>% pull({{df}}))
  
  pooled <- dat %>%
    summarize(m = n(),
              B = var(est),  # between imputation var
              beta_bar = mean(est), # mean of estimated reg coeffs
              V_bar = mean(se^2), # mean of var - hc corrected   within imp var
              eta_bar = mean(df)) %>%   # mean of df
    mutate(
      
      V_total = V_bar + B * (m + 1) / m,  #between and within var est
      gamma = ((m + 1) / m) * B / V_total,  
      df_m = (m - 1) / gamma^2,
      df_obs = eta_bar * (eta_bar + 1) * (1 - gamma) / (eta_bar + 3),
      df = 1 / (1 / df_m + 1 / df_obs),
      
      # output
      se = sqrt(V_total),
      ci_lower = beta_bar - se * qt(0.975, df = df),
      ci_upper = beta_bar + se * qt(0.975, df = df)) %>%
    
    select(est = beta_bar, se, df, ci_lower, ci_upper) 
  
  return(pooled)
  
}

### function to pool results across imputations

across_pooled <- calc_pooled(dat = across_res, est = estimate, se = se, df = df)
across_pooled %>%
  kable(digits = 3)

#output:
# |    est|    se|      df| ci_lower| ci_upper|
#   |------:|-----:|-------:|--------:|--------:|
#   | 19.148| 0.647| 34225.9|    17.88|   20.417|


within_pooled <- calc_pooled(dat = within_res, est = estimate, se = se, df = df)
within_pooled %>%
  kable(digits = 3)

# #output:
# |    est|    se|       df| ci_lower| ci_upper|
#   |------:|-----:|--------:|--------:|--------:|
#   | 19.159| 0.648| 31297.22|   17.889|   20.429|

#beautiful
### exercisers (exercise = yes/1) are predicted to score ~ 19pts higher on mhq 
### compared to non exercisers (exercise = no/0)

##balancing & nested model
## running model on each subdomain









# ###### extra stuff 
# ########################################################################
# dat2 <- select(dat1, Adaptability.to.Change:Adult.traumas)
# 
# dat2 <- dat2[,-which(sapply(dat2, class) == "factor")]
# dat2 <- select(dat2, -c(49:54))
# #removes all factor variables for correlation matrix
# 
# cormax <- cor(dat2, use = "pairwise.complete.obs")
# KMO(dat2)
# cortest.bartlett(dat2)
# det(cormax)
# 
# ### Normality w MVN package
# #https://cran.r-project.org/web/packages/MVN/vignettes/MVN.pdf
# library(MVN)
# result <- mvn(dat2, mvnTest = "mardia")
# result$multivariateNormality
# 
# 
# parallel <- 
#   fa.parallel(dat2, 
#               fm="ml", fa="fa") 
# 
# 
# 
# obs = data.frame(parallel$fa.values)
# obs$type = c('Observed Data')
# obs$num = c(row.names(obs))
# obs$num = as.numeric(obs$num)
# colnames(obs) = c('eigenvalue', 'type', 'num')
# 
# View(obs)















































