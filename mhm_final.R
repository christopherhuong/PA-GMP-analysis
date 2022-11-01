library(tidyverse)


#diagnosis of MH disorder


# ### CHRIS LAB COMP
dat3 <- read.csv("C:/Users/shg100/Documents/INCH/MHM_PA/mhm_data_2022-10-14_14-49-18.csv")
# ### CHRIS HOME COMP
# dat <- read.csv("C:/Users/Chris/OneDrive/Documents/INCH/MHM/mhm_data_2022-10-14_14-49-18.csv")


#save(dat, file = "dat.RData")
load("dat.RData")



#########################               #################
#########################               ################# 
#########################    WRANGLING  #################
#########################               #################
#########################               #################

dat$id <- 1:nrow(dat)




dat1 <-data.frame()[1:nrow(dat), ]  # create new DF with n rows

dat1 <- dat1 %>%                    #add and rename relevant variables to new df
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
              relationship = dat$Current.Family.Situation,
              socialize = dat$Frequency.of.Socializing,
              sleep = dat$Frequency.of.getting.a.good.nights.sleep,
              meddiagnosis = dat$Presence.Absence.of.Diagnosed.Medical.Disorder,
              mhseeking = dat$Mental.Health.Treatment.Status,
              mhdiagnosis = dat$Diagnosed.mental.health.disorders,
              childtrauma = dat$Childhood.traumas,
              adulttrauma = dat$Adult.traumas
               )

##################### REMOVE BLANKS AND NAs ##############
sum(dat1 == "Prefer not to say")
sum(dat1 == "")
dat1[dat1 == "Prefer not to say"] <- NA 
dat1[dat1 == ""] <- NA
sum(is.na(dat1))

mhm <- dat1

library(naniar)
gg_miss_var(mhm, show_pct = TRUE)

######################### PHYSICAL ACTIVITY ###############
# only keep english responses, removes 2 rows which had arabic or something
mhm <- mhm %>%
  subset(PA == "Every day" |
           PA == "Few days a week" |
           PA == "Less than once a week" |
           PA == "Once a week" |
           PA == "Rarely/Never")

mhm$PA <- factor(mhm$PA, order = T,     #factor() automatically drops unused levels
                   levels = c("Rarely/Never", 
                              "Less than once a week",
                              "Once a week",
                              "Few days a week",
                              "Every day"))
summary(mhm$PA)
######################## AGE ##############

mhm$age_cat <- mhm$age

mhm <- mhm %>%
  mutate(age_cat = case_when(age_cat == "18-24" ~ "young.adult",
                         age_cat == "25-34" ~ "young.adult",
                         age_cat == "35-44" ~ "middle.adult",
                         age_cat == "45-54" ~ "middle.adult",
                         age_cat == "55-64" ~ "middle.adult",
                         age_cat == "65-74" ~ "senior",
                         age_cat == "75-84" ~ "senior",
                         age_cat == "85+"   ~ "senior"
                         ))

mhm$age_cat <- factor(mhm$age_cat, order = T,
                  levels = c("young.adult",
                             "middle.adult",
                             "senior"))

summary(mhm$age_cat)

################################
summary(mhm$age)
mhm$age <- factor(mhm$age, order = T,
                  levels = c("18-24",
                             "25-34",
                             "35-44",
                             "45-54",
                             "55-64",
                             "65-74",
                             "75-84",
                             "85+"))

summary(mhm$age)

###################### SEX AND GENDER DIFF ###############
mhm$sex <- factor(mhm$sex, order = F)
summary(mhm$sex)        

mhm$genderdiff <- factor(mhm$genderdiff, order = F)
summary(mhm$genderdiff)

##################### COUNTRY AND ETHNICITY ###########
mhm$country <- factor(mhm$country, order = F)
summary(mhm$country)

table(mhm$ethnicity)
mhm$ethnicity <- factor(mhm$ethnicity)
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

mhm$education <- factor(mhm$education, order = F)

summary(mhm$education)
############################# EMPLOYMENT AND RELATIONSHIP STATUS ###############
table(mhm$employment)
mhm$employment <- factor(mhm$employment, order = F)
summary(mhm$employment)

table(mhm$relationship)
mhm$relationship[mhm$relationship == "Other"] <- NA
mhm$relationship <- factor(mhm$relationship, order = F)
summary(mhm$relationship)
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
####################### MEDICAL DIAGNOSIS ############
table(mhm$meddiagnosis)
mhm$meddiagnosis <- factor(mhm$meddiagnosis, order = F)
summary(mhm$meddiagnosis)


########################## MH SEEKING AND MH DIAGNOSIS ############
table(mhm$mhseeking)
mhm$mhseeking <- factor(mhm$mhseeking, order = F)
summary(mhm$mhseeking)

table(mhm$mhdiagnosis)
mhm$mhdiagnosis <- factor(mhm$mhdiagnosis, order = F)
summary(mhm$mhdiagnosis)
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



############# plot missingness
gg_miss_var(mhm, show_pct = TRUE)


############# return percent of missingness
percentmiss <- function(x){
  sum(is.na(x)) / length(x) * 100} 

apply(mhm, 2, percentmiss)   ####percent missingness per col
table(apply(mhm, 1, percentmiss))  ###number of subjects with missing values by percent 


##############
mhm <- mhm %>%
  subset(select = -c(ethnicity, mhdiagnosis))

###### drop due to missingness
###### effects of ethnicity may be somewhat attenuated by country nesting
###### mhdiagnosis is redundant with mhseeking, drop


##########################                 #####################
##########################                 #####################
##########################     IMPUTATION  #####################
##########################                 #####################
##########################                 #####################
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




library(mice)




impute <- mhm
impute$country <- as.integer(impute$country)
summary(impute)




predMatrix <- quickpred(impute, mincor=0.10)
predMatrix[, c("id")] <- 0                        # id = 0
predMatrix[, c("country")] <- -2                  # country = -2
predMatrix[c("country","id"), "country"] <- 0     # id x country = 0
#In the predictor matrix, -2 denotes the class variable,
#a value 1 indicates a fixed effect and a value 2 indicates a random effect.


impMethod <- make.method(data = impute, defaultMethod = "pmm")
impMethod[c("sex")] <- "polyreg"      
impMethod[c("genderdiff")] <- "logreg"
impMethod[c("education")] <- "polyreg"
impMethod[c("relationship")] <- "polyreg"
impMethod[c("meddiagnosis")] <- "logreg"
impMethod[c("mhseeking")] <- "logreg"
impMethod[c("childtrauma")] <- "logreg"
impMethod[c("adulttrauma")] <- "logreg"



# system.time(imp <- mice(impute, method = impMethod,
#                  predictorMatrix = predMatrix,
#                  maxit = 5,
#                  m = 5,
#                  seed = 123))



# save(imp, file = "imp.RData")

load("imp.RData")



densityplot(imp)
plot(imp)
View(complete(imp,2))
summary(complete(imp,1))

#randomly select 1 dataset for plotting purposes
floor(runif(1, min=0, max=5))
#[1] 4
imp_full.4 <- complete(imp,4)
summary(imp_full.4)

imp_long <- complete(imp, action = 'long', include = TRUE)


# save(imp_long, file = "imp_long.RData")

load("imp_long.RData")



#########################                            ###############
#########################    PROPENSITY SCORING      ###############
#########################                            ###############


library(MatchThem) 
library(survey) 
library(CBPS)
library(cobalt)
library(knitr)

# estimand = the desired estimand. 
# For binary and multi-category treatments, can be "ATE",
# "ATT", "ATC"


# Focal = when multi-category treatments are used and ATT weights are requested, which
# group to consider the "treated" or focal group. This group will not be weighted,
# and the other groups will be weighted to be more like the focal group. 
# If specified, estimand will automatically be set to "ATT".


system.time(
  weightdat_multi_att <-weightthem(PA ~   
                       age   
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
           imp, 
           approach = 'within',    #calculating distance measures within each imputed dataset
                                   #and weighting observations based on them 
           method = "cbps",        #covariate balancing PS
           estimand = "ATT",
           focal = "Rarely/Never") )

save(weightdat_multi_att, file = "weightdat_multi_att.RData")

load("weightdat_multi_att.RData")



love.plot(weightdat_multi_att, binary = "std", var.order = "un", stats = "m",
           thresholds = c(.10, .05)) + theme(legend.position = "top")






############ SURVEY DESIGN 
#
des_multi_att <- svydesign(ids = ~country, weights = ~1, data = imp_long) 

#  double robust analysis
mhq_multi_att <-with(weightdat_multi_att, svyglm(mhq ~ 
                               PA*age
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





##################################################################



