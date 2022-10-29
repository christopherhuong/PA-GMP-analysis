library(tidyverse)


dat <- read.csv("mhm_data_2022-10-14_14-49-18.csv")


#########################               #################
#########################               ################# 
#########################    WRANGLING  #################
#########################               #################
#########################               #################

dat$id <- 1:nrow(dat)


mhm <-data.frame()[1:nrow(dat), ]  # create new DF with n rows

mhm <- mhm %>%                    #add and rename relevant variables to new df
  add_column( id = dat$id,
              mhq = dat$Overall.MHQ,
              cog = dat$Cognition,
              adaptresil = dat$Adaptability...Resilence,
              drivemotiv = dat$Drive...Motivation,  #differs from the response item
              moodoutlook = dat$Mood...Outlook,
              socialself = dat$Social...Self,
              mindbody = dat$Mind.Body.Connection,
              PA = dat$Frequency.of.doing.exercise,
              age = dat$Age,
              sex = dat$Biological.Sex,
              genderdiff = dat$Different.gender.from.biological.sex,
              country = dat$Country,
              ethnicity = dat$Ethnicity,
              education = dat$Education,
              employment = dat$Employment,
              socialize = dat$Frequency.of.Socializing,
              sleep = dat$Frequency.of.getting.a.good.nights.sleep,
              meddiagnosis = dat$Presence.Absence.of.Diagnosed.Medical.Disorder,
              mhseeking = dat$Mental.Health.Treatment.Status,
              childtrauma = dat$Childhood.traumas,
              adulttrauma = dat$Adult.traumas
               )


##################### REMOVE BLANKS AND NAs ##############
sum(mhm == "Prefer not to say")
sum(mhm == "")
mhm[mhm == "Prefer not to say"] <- NA 
mhm[mhm == ""] <- NA
sum(is.na(mhm))


######################### PHYSICAL ACTIVITY ###############
# only keep english responses, removes 2 rows which had arabic or something
mhm <- mhm %>%
  subset(PA == "Every day" |
           PA == "Few days a week" |
           PA == "Less than once a week" |
           PA == "Once a week" |
           PA == "Rarely/Never")

mhm$PA <- factor(mhm$PA, order = T, 
                   levels = c("Rarely/Never", 
                              "Less than once a week",
                              "Once a week",
                              "Few days a week",
                              "Every day"))
summary(mhm$PA)
######################## AGE ##############
mhm <- mhm %>%
  mutate(age = case_when(age == "18-24" ~ "young.adult",
                         age == "25-34" ~ "young.adult",
                         age == "35-44" ~ "middle.adult",
                         age == "45-54" ~ "middle.adult",
                         age == "55-64" ~ "middle.adult",
                         age == "65-74" ~ "senior",
                         age == "75-84" ~ "senior",
                         age == "85+"   ~ "senior"
                         ))

mhm$age <- factor(mhm$age, order = T,
                  levels = c("young.adult",
                             "middle.adult",
                             "senior"))

summary(mhm$age)
###################### SEX AND GENDER DIFF ###############
mhm$sex <- as.factor(mhm$sex)
summary(mhm$sex)        

mhm$genderdiff <- as.factor(mhm$genderdiff)
summary(mhm$genderdiff)

##################### COUNTRY ###########
mhm$country <- as.factor(mhm$country)
summary(mhm$country)

#################### ETHNICITY ###########
mhm$ethnicity <- as.factor(mhm$ethnicity)
summary(mhm$ethnicity)

################### EDUCATION ##########
table(mhm$education)   ####### WHAT IS "Médio completo" ?????
mhm <- mhm %>%              
mutate(education = case_when(education == "Primary Education" ~ "less.hs",
                             education == "Some High School" ~ "less.hs",
                             education == "Médio completo" ~ "less.hs",
                             education == "High School" ~ "hs",
                             education == "Ensino técnico" ~ "vocational",
                             education == "Ensino profissionalizante" ~ "vocational",
                             education == "Vocational certification" ~ "vocational",
                             education == "Associate’s Degree" ~ "assoc.deg",
                             education == "Bachelor's Degree" ~ "bach.deg",
                             education == "Master's Degree" ~ "grad.deg",
                             education == "PhD" ~ "grad.deg",
                             education == "J.D" ~ "grad.deg",
                             education == "J.D. (Direito)" ~ "grad.deg",
                             education == "M.D." ~ "grad.deg",
                             education == "M.D. (Medicina)" ~ "grad.deg",
                             education == "Other" ~ "other"))

mhm$education[mhm$education == "other"] <- NA

mhm$education <- factor(mhm$education, order = T,
                        levels = c("less.hs",
                                   "hs",
                                   "vocational",
                                   "assoc.deg",
                                   "bach.deg",
                                   "grad.deg"))

summary(mhm$education)
############################# EMPLOYMENT ###############
table(mhm$employment)
mhm$employment <- factor(mhm$employment, order = T,
                         levels = c("Employed",
                                    "Unemployed",
                                    "Not able to work",
                                    "Studying",
                                    "Homemaker",
                                    "Retired"
                                    ))
summary(mhm$employment)
######################## SOCIALIZE AND SLEEP  ############
table(mhm$socialize)
mhm$socialize <- factor(mhm$socialize, order = T,
                        levels = c("Rarely/Never",
                                   "1-3 times a month",
                                   "Once a week",
                                   "Several days a week"))
summary(mhm$socialize)

table(mhm$sleep)
mhm$sleep <- factor(mhm$sleep, order = T,
                    levels = c("Hardly ever",
                               "Some of the time",
                               "Most of the time",
                               "All of the time"))
summary(mhm$sleep)
####################### MEDICAL DIAGNOSIS AND MENTAL HEALTH ########
table(mhm$meddiagnosis)
mhm$meddiagnosis <- as.factor(mhm$meddiagnosis)
summary(mhm$meddiagnosis)

table(mhm$mhseeking)
mhm$mhseeking <- as.factor (mhm$mhseeking)
summary(mhm$mhseeking)
##################### TRAUMAS ############  should we combine??
mhm$childtrauma <- 
  if_else((mhm$childtrauma == "|I did not experience any of the above during my childhood")|(mhm$childtrauma == "|None of the above"), 
          "No", "Yes")
mhm$childtrauma <- as.factor(mhm$childtrauma)
summary(mhm$childtrauma)
####
mhm$adulttrauma <- 
  if_else((mhm$adulttrauma == "|I did not experience any of the above during my childhood")|(mhm$adulttrauma == "|None of the above"), 
          "No", "Yes")
mhm$adulttrauma <- as.factor(mhm$adulttrauma)
summary(mhm$adulttrauma)

##########################                 #####################
##########################                 #####################
##########################     IMPUTATION  #####################
##########################                 #####################
##########################                 #####################



impute <- mhm %>%  #new df with outcomes + covariates including country
  select(colnames(mhm))

impute$country <- as.integer(impute$country)

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

summary(impute)












