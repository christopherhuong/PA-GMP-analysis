library(tidyverse)

setwd("C:/Users/denve/OneDrive - University of Texas at San Antonio/UTSA/1 - Research/Mental Health Million/Exercise and Subdomains of Mental Health among Young Adults")

dat1 <- read.csv("mhm_data_2022-07-21_19-46-52.csv", 
                 stringsAsFactors = T, na.strings = "")


# Data Wrangling ----------------------------------------------------------



tapply(dat1$Country, dat1$Household.Income, summary)
#check summary statistics by group. seems like only US, india, and germany have income data


dat1$id <- 1:nrow(dat1)


###### select relevant variables and rename to new DF. probably a cleaner way to do this
dat1$mhq <- dat1$Overall.MHQ       #rename Overall.MHQ -> mhq
mhm <- select(dat1, mhq)          #create new df with column mhq


mhm <- mhm %>%                    #add and rename relevant variables to new df
  add_column(id = dat1$id,
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

mhm$exercise <- as.factor(mhm$exercise)



mhm[mhm == "|Prefer not to say"] <- NA  
### due to age of sample, only use childhood trauma and leave out adult trauma?
mhm$childhood.trauma <- 
  if_else((mhm$childhood.trauma == "|I did not experience any of the above during my childhood")|(mhm$childhood.trauma == "|None of the above"), 
          "No", "Yes")
mhm$childhood.trauma <- as.factor(mhm$childhood.trauma)
#  No   Yes  NA's 
#11817 29506  1904 

table(mhm$education)


mhm <- mhm %>%                 #rename  #made mistake######################
  mutate(education = case_when(education == "Some High School" ~ "less.hs",
                               education == "Primary Education" ~ "less.hs",
                               education == "High School" ~ "hs",
                               education == "Vocational certification" ~ "vocational",
                               education == "Associateâ€™s Degree" ~ "assoc.deg",
                               education == "Bachelor's Degree" ~ "bach.deg",
                               education == "Master's Degree" ~ "grad.deg",
                               education == "Ph.D. or higher" ~ "grad.deg",
                               education == "J.D" ~ "grad.deg",
                               education == "M.D." ~ "grad.deg",
                               education == "Other" ~ "other")) ##added
  
mhm$education <- as.factor(mhm$education)

# any variable value not included in case_when automatically becomes NA?
table(mhm$education)
mhm <- mhm %>%               #reorder
  mutate(education = fct_relevel(education, "other",
                                 "less.hs",
                                 "hs",
                                 "vocational",
                                 "assoc.deg",
                                 "bach.deg",
                                 "grad.deg"
                                 )) 

table(mhm$education)


impute <- mhm %>%  #new df with outcomes + covariates including country
  select(id, country, mhq, core.cog, complex.cog, drive.motivation, mood.outlook, social.self, mind.body,
         exercise, sex, identity.diff, country, education, employment, relationship.status, sleep,
         socialize, disability, seek.mh.treatment, childhood.trauma
         )

# Multilevel Imputation Model ---------------------------------------------
impute$country <- as.integer(impute$country)

library(broom.mixed)
library(MCMCglmm)
library(msm)
library(tmvtnorm)
install.packages("https://cran.r-project.org/src/contrib/Archive/linLIR/linLIR_1.1.tar.gz",
                 repos=NULL, method="libcurl")
install.packages("https://cran.r-project.org/src/contrib/Archive/hmi/hmi_1.0.0.tar.gz",
                 repos=NULL, method="libcurl")
library(hmi)
library(mice)
library(miceadds)

dat_long <- impute %>% 
  select(id, country, mhq, core.cog, complex.cog, drive.motivation, mood.outlook, social.self, mind.body,
         exercise, sex, identity.diff, country, education, employment, relationship.status, sleep,
         socialize, disability, seek.mh.treatment, childhood.trauma
  )

summary(dat_long)

dat_long$sex <- droplevels(dat_long$sex)
dat_long$identity.diff <- droplevels(dat_long$identity.diff)
dat_long$disability <- droplevels(dat_long$disability)
dat_long$seek.mh.treatment <- droplevels(dat_long$seek.mh.treatment)

summary(dat_long)


predMatrix <- quickpred(dat_long, mincor=0.10)
predMatrix[, c("id")] <- 0
predMatrix[, c("country")] <- -2
predMatrix[c("country","id"), "country"] <- 0

impMethod <- make.method(data = dat_long, defaultMethod = "pmm")
impMethod[c("sex")] <- "polyreg"                  #polyreg-> 3+ nominal (unordered) factors
impMethod[c("identity.diff")] <- "logreg"         #logreg-> binary factor
impMethod[c("education")] <- "polyreg"
impMethod[c("relationship.status")] <- "polyreg"
impMethod[c("disability")] <- "logreg"
impMethod[c("seek.mh.treatment")] <- "logreg"
impMethod[c("childhood.trauma")] <- "logreg"


#test the imputation model
imp <- mice(dat_long, method = impMethod, 
            predictorMatrix = predMatrix, 
            maxit = 2,
            m = 2)

densityplot(imp)

plot(imp)

View(complete(imp,2))

summary(complete(imp,1))

#run full imputation model
imp_full <- mice(dat_long, method = impMethod, 
                 predictorMatrix = predMatrix, 
                 maxit = 5,
                 m = 5)

plot(imp_full)

densityplot(imp_full)

View(complete(imp_full,2))

summary(complete(imp_full,1))

#randomly select 1 dataset for plotting purposes
floor(runif(1, min=0, max=5))
#[1] 4
imp_full.4 <- complete(imp_full,4)
summary(imp_full.4)

imp_long <- complete(imp_full, action = 'long', include = TRUE)
write.csv(imp_long,"C:/Users/denve/OneDrive - University of Texas at San Antonio/UTSA/1 - Research/Mental Health Million/Exercise and Subdomains of Mental Health among Young Adults//imp_long_5.csv", row.names = FALSE)

##subset multiply imputed datasets
#Step 1
###start here to begin with imputed dataset
imp_long <- read_csv('imp_long_5.csv')

#Step 2 - reformat into multiply imputed datasets
imp_mids <- as.mids(imp_long)

# Objective 1  - MHQ -------------------------------------------------------------


library(MatchThem) 
library(survey) 
library(CBPS)


#Weighting the multiply imputed datasets 
weighted.datasets_mhq <-weightthem(exercise ~
                                   sex
                                   + identity.diff
                                   + education
                                   + employment
                                   + relationship.status
                                   + sleep
                                   + disability
                                   + seek.mh.treatment
                                   + childhood.trauma, 
                                     imp_mids, 
                                     approach = 'within',
                                     method = "cbps",
                                     estimand = "ATT") 

#plots - mean balance is very good, no need to square continuous variables
library(cobalt)
love.plot(weighted.datasets_mhq, binary = "std", var.order = "un", stats = c("m", "ks"),
          thresholds = c(.10, .05)) + theme(legend.position = "top")

############ SURVEY DESIGN 
#
des <- svydesign(ids = ~country, weights = ~1, data = imp_long) 

#  double robust analysis
mhq <-with(weighted.datasets_mhq, svyglm(mhq ~ exercise
                                           + sex
                                           + identity.diff
                                           + education
                                           + employment
                                           + relationship.status
                                           + sleep
                                           + disability
                                           + seek.mh.treatment
                                           + childhood.trauma,
                                                      design = des,
                                                      family = gaussian())) 

results_mhq <- pool(mhq) #pool results from analysis of m datasets

summary(results_mhq)

# Objective 1a  - Core Cognition -------------------------------------------------------------

#Weighting the multiply imputed datasets 
weighted.datasets_core.cog <-weightthem(exercise ~
                                   + sex
                                   + identity.diff
                                   + education
                                   + employment
                                   + relationship.status
                                   + sleep
                                   + disability
                                   + seek.mh.treatment
                                   + childhood.trauma, 
                                   imp_mids,
                                   approach = 'within',
                                   method = "cbps",
                                   estimand = "ATT") 

#plots - mean balance is very good, no need to square continuous variables
library(cobalt)
love.plot(weighted.datasets_core.cog, binary = "std", var.order = "un", stats = c("m", "ks"),
          thresholds = c(.10, .05)) + theme(legend.position = "top")

############ SURVEY DESIGN 
#
des <- svydesign(ids = ~country, weights = ~1, data = imp_long) 

#  double robust analysis
core.cog <-with(weighted.datasets_core.cog, svyglm(core.cog ~ exercise
                                         + sex
                                         + identity.diff
                                         + education
                                         + employment
                                         + relationship.status
                                         + sleep
                                         + disability
                                         + seek.mh.treatment
                                         + childhood.trauma,
                                         design = des,
                                         family = gaussian())) 

results_core.cog <- pool(core.cog) #pool results from analysis of m datasets

summary(results_core.cog)

# Objective 1b  - Complex Cognition -------------------------------------------------------------

#Weighting the multiply imputed datasets 
weighted.datasets_complex.cog <-weightthem(exercise ~
                                   sex
                                   + country
                                   + identity.diff
                                   + education
                                   + employment
                                   + relationship.status
                                   + sleep
                                   + disability
                                   + seek.mh.treatment
                                   + childhood.trauma, 
                                   imp_mids,
                                   approach = 'within',
                                   method = "cbps",
                                   estimand = "ATT")

############ SURVEY DESIGN 
#
des <- svydesign(ids = ~country, weights = ~1, data = imp_long) 

#  double robust analysis
complex.cog <-with(weighted.datasets_complex.cog, svyglm(complex.cog ~ exercise
                                                   + sex
                                                   + identity.diff
                                                   + education
                                                   + employment
                                                   + relationship.status
                                                   + sleep
                                                   + disability
                                                   + seek.mh.treatment
                                                   + childhood.trauma,
                                                   design = des,
                                                   family = gaussian())) 

results_complex.cog <- pool(complex.cog) #pool results from analysis of m datasets

summary(results_complex.cog)

# Objective 1c  - Drive Motivation -------------------------------------------------------------

#Weighting the multiply imputed datasets 
weighted.datasets_drive.motivation <-weightthem(exercise ~
                                   sex
                                   + country
                                   + identity.diff
                                   + education
                                   + employment
                                   + relationship.status
                                   + sleep
                                   + disability
                                   + seek.mh.treatment
                                   + childhood.trauma, 
                                   imp_mids,
                                   approach = 'within',
                                   method = "cbps",
                                   estimand = "ATT")

############ SURVEY DESIGN 
#
des <- svydesign(ids = ~country, weights = ~1, data = imp_long) 

#  double robust analysis
drive.motivation <-with(weighted.datasets_drive.motivation, svyglm(drive.motivation ~ exercise
                                                         + sex
                                                         + identity.diff
                                                         + education
                                                         + employment
                                                         + relationship.status
                                                         + sleep
                                                         + disability
                                                         + seek.mh.treatment
                                                         + childhood.trauma,
                                                         design = des,
                                                         family = gaussian())) 

results_drive.motivation <- pool(drive.motivation) #pool results from analysis of m datasets

summary(results_drive.motivation)

# Objective 1d  - Mood & Outlook -------------------------------------------------------------

#Weighting the multiply imputed datasets 
weighted.datasets_mood.outlook <-weightthem(exercise ~
                                   sex
                                   + country
                                   + identity.diff
                                   + education
                                   + employment
                                   + relationship.status
                                   + sleep
                                   + disability
                                   + seek.mh.treatment
                                   + childhood.trauma, 
                                   imp_mids, 
                                   approach = 'within',
                                   method = "cbps",
                                   estimand = "ATT")

############ SURVEY DESIGN 
#
des <- svydesign(ids = ~country, weights = ~1, data = imp_long) 

#  double robust analysis
mood.outlook <-with(weighted.datasets_mood.outlook, svyglm(mood.outlook ~ exercise
                                                                   + sex
                                                                   + identity.diff
                                                                   + education
                                                                   + employment
                                                                   + relationship.status
                                                                   + sleep
                                                                   + disability
                                                                   + seek.mh.treatment
                                                                   + childhood.trauma,
                                                                   design = des,
                                                                   family = gaussian())) 

results_mood.outlook <- pool(mood.outlook) #pool results from analysis of m datasets

summary(results_mood.outlook)


# Objective 1e  - Social Self -------------------------------------------------------------

#Weighting the multiply imputed datasets 
weighted.datasets_social.self <-weightthem(exercise ~
                                            sex
                                            + country
                                            + identity.diff
                                            + education
                                            + employment
                                            + relationship.status
                                            + sleep
                                            + disability
                                            + seek.mh.treatment
                                            + childhood.trauma, 
                                            imp_mids, 
                                            approach = 'within',
                                            method = "cbps",
                                            estimand = "ATT")

############ SURVEY DESIGN 
#
des <- svydesign(ids = ~country, weights = ~1, data = imp_long) 

#  double robust analysis
social.self <-with(weighted.datasets_social.self, svyglm(social.self ~ exercise
                                                           + sex
                                                           + identity.diff
                                                           + education
                                                           + employment
                                                           + relationship.status
                                                           + sleep
                                                           + disability
                                                           + seek.mh.treatment
                                                           + childhood.trauma,
                                                           design = des,
                                                           family = gaussian())) 

results_social.self <- pool(social.self) #pool results from analysis of m datasets

summary(results_social.self)

# Objective 1f  -  Mind & Body -------------------------------------------------------------

#Weighting the multiply imputed datasets 
weighted.datasets_mind.body <-weightthem(exercise ~
                                           sex
                                           + country
                                           + identity.diff
                                           + education
                                           + employment
                                           + relationship.status
                                           + sleep
                                           + disability
                                           + seek.mh.treatment
                                           + childhood.trauma, 
                                           imp_mids,
                                           approach = 'within',
                                           method = "cbps",
                                           estimand = "ATT")

############ SURVEY DESIGN 
#
des <- svydesign(ids = ~country, weights = ~1, data = imp_long) 

#  double robust analysis
mind.body <-with(weighted.datasets_mind.body, svyglm(mind.body ~ exercise
                                                         + sex
                                                         + identity.diff
                                                         + education
                                                         + employment
                                                         + relationship.status
                                                         + sleep
                                                         + disability
                                                         + seek.mh.treatment
                                                         + childhood.trauma,
                                                         design = des,
                                                         family = gaussian())) 

results_mind.body <- pool(mind.body) #pool results from analysis of m datasets

summary(results_mind.body)


# Create Table 1 ############################################
####table 1 package
install.packages("table1")
library(table1)

imp_long_0removed <- imp_long %>% 
  filter(.imp != 0)


table1(~ factor(country) + factor(exercise) + factor(sex) + factor(identity.diff) + factor(education)
       + factor(employment) + factor(relationship.status) + factor(sleep) 
       + factor(disability) + factor(seek.mh.treatment) + factor(childhood.trauma),
       data=imp_long_0removed) # "| SuicThink_choice" to stratify by suicidal ideation item

