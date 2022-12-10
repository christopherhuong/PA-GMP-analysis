# ### CHRIS LAB COMP
# dat3 <- read.csv("C:/Users/shg100/Documents/INCH/MHM_PA/mhm_data_2022-10-14_14-49-18.csv")
# ### CHRIS HOME COMP
# dat <- read.csv("C:/Users/Chris/OneDrive/Documents/INCH/MHM/mhm_data_2022-10-14_14-49-18.csv")


#save(dat, file = "dat.RData")
library(tidyverse)

load("dat.RData")



#########################               #################
#########################               ################# 
#########################    WRANGLING  #################
#########################               #################
#########################               #################

dat$id <- 1:nrow(dat)
##################



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

##############

mhm <- dat1

library(naniar)
gg_miss_var(mhm, show_pct = TRUE)   # no NAs due to missingness = " " in dataframe


mhm <- mhm %>%
  subset(select = -c(ethnicity, mhdiagnosis))

###### drop due to missingness
###### effects of ethnicity may be somewhat attenuated by country nesting
###### mhdiagnosis is redundant with mhseeking, drop


######################### PHYSICAL ACTIVITY ###############
# only keep english responses, removes 2 rows which had arabic or something
summary(mhm$PA)

mhm <- mhm %>%
  subset(PA == "Every day" |
           PA == "Few days a week" |
           PA == "Less than once a week" |
           PA == "Once a week" |
           PA == "Rarely/Never")

#dropped 2 rows

mhm$PA <- factor(mhm$PA, order = F,     #factor() automatically drops unused levels
                 levels = c("Rarely/Never", 
                            "Less than once a week",
                            "Once a week",
                            "Few days a week",
                            "Every day"))



summary(mhm$PA)
######################## AGE ##############
summary(mhm$age)
str(mhm$age)



################################

###################### SEX AND GENDER DIFF ###############
summary(mhm$sex)



#dropped 819 rows

mhm$sex <- factor(mhm$sex, order = F)
summary(mhm$sex)        




mhm$genderdiff <- factor(mhm$genderdiff, order = F)
summary(mhm$genderdiff)

##################### COUNTRY ###########
mhm$country <- factor(mhm$country, order = F)
summary(mhm$country)



################### EDUCATION ##########
summary(mhm$education)   ####### WHAT IS "Médio completo" ?????
mhm <- mhm %>%              
  mutate(education = case_when(education == "Primary Education" ~ "less.hs",
                               education == "Some High School" ~ "less.hs",
                               education == "MÃ©dio completo" ~ "less.hs",
                               education == "High School" ~ "hs",
                               education == "Ensino tÃ©cnico" ~ "vocational",
                               education == "Ensino profissionalizante" ~ "vocational",
                               education == "Vocational certification" ~ "vocational",
                               education == "Associateâ€™s Degree" ~ "assoc.deg",
                               education == "Bachelor's Degree" ~ "bach.deg",
                               education == "Master's Degree" ~ "grad.deg",
                               education == "PhD" ~ "grad.deg",
                               education == "J.D" ~ "grad.deg",
                               education == "J.D. (Direito)" ~ "grad.deg",
                               education == "M.D." ~ "grad.deg",
                               education == "M.D. (Medicina)" ~ "grad.deg",
                               education == "Other" ~ "other",
                               education == "Prefer not to say" ~ "Prefer not to say"))

table(mhm$education)




mhm$education <- factor(mhm$education, order = F)

summary(mhm$education)
############################# EMPLOYMENT AND RELATIONSHIP STATUS ###############
table(mhm$employment)
mhm$employment <- factor(mhm$employment, order = F)
summary(mhm$employment)

table(mhm$relationship)

mhm$relationship <- factor(mhm$relationship, order = F)
summary(mhm$relationship)
######################## SOCIALIZE AND SLEEP  ############
table(mhm$socialize)
mhm$socialize <- factor(mhm$socialize, order = F,
                        levels = c("Rarely/Never",
                                   "1-3 times a month",
                                   "Once a week",
                                   "Several days a week"))
summary(mhm$socialize)

table(mhm$sleep)
mhm$sleep <- factor(mhm$sleep, order = F,
                    levels = c("Hardly ever",
                               "Some of the time",
                               "Most of the time",
                               "All of the time"))
summary(mhm$sleep)
####################### MEDICAL DIAGNOSIS ############
table(mhm$meddiagnosis)
mhm$meddiagnosis <- factor(mhm$meddiagnosis, order = F)
summary(mhm$meddiagnosis)


########################## MH SEEKING  ############
table(mhm$mhseeking)
mhm$mhseeking <- factor(mhm$mhseeking, order = F)
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


##############


mhm[mhm == "Prefer not to say"] <- NA 
mhm[mhm == ""] <- NA
sum(is.na(mhm))

mhm <- droplevels(mhm)  #get rid of "Prefer not to say"
mhm$country <- as.integer(mhm$country)
summary(mhm)


save(mhm, file = "mhm.RData")


############# plot missingness
library(naniar)
library(knitr)
gg_miss_var(mhm, show_pct = TRUE)


############# return percent of missingness
percentmiss <- function(x){
  sum(is.na(x)) / length(x) * 100} 

kable(apply(mhm, 2, percentmiss),
      digits = 3)   ####percent missingness per col
kable(table(apply(mhm, 1, percentmiss)),
      digits = 3)###number of subjects with missing values by percent 





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









predMatrix <- quickpred(mhm, mincor=0.10)
predMatrix[, c("id")] <- 0                        # id = 0
predMatrix[, c("country")] <- -2                  # country = -2
predMatrix[c("country","id"), "country"] <- 0     # id x country = 0
#In the predictor matrix, -2 denotes the class variable,
#a value 1 indicates a fixed effect and a value 2 indicates a random effect.


impMethod <- make.method(data = mhm, defaultMethod = "pmm")
impMethod[c("sex")] <- "polyreg"      
impMethod[c("genderdiff")] <- "logreg"
impMethod[c("education")] <- "polyreg"
impMethod[c("relationship")] <- "polyreg"
impMethod[c("meddiagnosis")] <- "logreg"
impMethod[c("mhseeking")] <- "logreg"
impMethod[c("childtrauma")] <- "logreg"
impMethod[c("adulttrauma")] <- "logreg"



imp_overall <- mice(mhm, method = impMethod,
                    predictorMatrix = predMatrix,
                    maxit = 10,
                    m = 10,
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



weightdat_overall <- weightthem(PA ~   
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
                                imp_overall, 
                                approach = 'within',  
                                method = "cbps",    
                                estimand = "ATT",
                                focal = "Rarely/Never") 

save(weightdat_overall, file = "weightdat_overall.RData")










# love.plot(weightdat_overall, binary = "std", var.order = "un", stats = "m",
#            thresholds = c(.10, .05)) + theme(legend.position = "top")
# 
# 


rm(dat)
rm(dat1)
gc()
###################### WEIGHTING PA*AGE*MHSEEK INTERACTIONS ############################################
#############
############# SPLIT SAMPLE BY SEX

# load("mhm.Rdata")

mhm_female <- mhm %>%
  subset(sex == "Female")



mhm_female <- mhm_female %>%
  mutate(age = case_when(age == "18-24" ~ "young.adult",
                         age == "25-34" ~ "young.adult",
                         age == "35-44" ~ "middle.adult",
                         age == "45-54" ~ "middle.adult",
                         age == "55-64" ~ "middle.adult",
                         age == "65-74" ~ "senior",
                         age == "75-84" ~ "senior",
                         age == "85+"   ~ "senior"
  ))

mhm_female$age <- factor(mhm_female$age, order = F,
                         levels = c("young.adult",
                                    "middle.adult",
                                    "senior"))





predMatrix <- quickpred(mhm_female, mincor=0.10)
predMatrix[, c("id")] <- 0                       
predMatrix[, c("country")] <- -2                  
predMatrix[c("country","id"), "country"] <- 0    


impMethod <- make.method(data = mhm_female, defaultMethod = "pmm")
impMethod[c("genderdiff")] <- "logreg"
impMethod[c("education")] <- "polyreg"
impMethod[c("relationship")] <- "polyreg"
impMethod[c("meddiagnosis")] <- "logreg"
impMethod[c("mhseeking")] <- "logreg"
impMethod[c("childtrauma")] <- "logreg"
impMethod[c("adulttrauma")] <- "logreg"



imp_female <- mice(mhm_female, method = impMethod,
                   predictorMatrix = predMatrix,
                   maxit = 10,
                   m = 10,
                   seed = 111)


save(imp_female, file = "imp_female.RData")


imp_female_long <- complete(imp_female, action = 'long', include = TRUE)


save(imp_female_long, file = "imp_female_long.RData")

################################################
####################  MALE              ########
################################################


mhm_male <- mhm %>%
  subset(sex == "Male")



mhm_male <- mhm_male %>%
  mutate(age = case_when(age == "18-24" ~ "young.adult",
                         age == "25-34" ~ "young.adult",
                         age == "35-44" ~ "middle.adult",
                         age == "45-54" ~ "middle.adult",
                         age == "55-64" ~ "middle.adult",
                         age == "65-74" ~ "senior",
                         age == "75-84" ~ "senior",
                         age == "85+"   ~ "senior"
  ))

mhm_male$age <- factor(mhm_male$age, order = F,
                       levels = c("young.adult",
                                  "middle.adult",
                                  "senior"))







predMatrix <- quickpred(mhm_male, mincor=0.10)
predMatrix[, c("id")] <- 0                        
predMatrix[, c("country")] <- -2                  
predMatrix[c("country","id"), "country"] <- 0     



impMethod <- make.method(data = mhm_male, defaultMethod = "pmm")
impMethod[c("genderdiff")] <- "logreg"
impMethod[c("education")] <- "polyreg"
impMethod[c("relationship")] <- "polyreg"
impMethod[c("meddiagnosis")] <- "logreg"
impMethod[c("mhseeking")] <- "logreg"
impMethod[c("childtrauma")] <- "logreg"
impMethod[c("adulttrauma")] <- "logreg"



imp_male <- mice(mhm_male, method = impMethod,
                 predictorMatrix = predMatrix,
                 maxit = 10,
                 m = 10,
                 seed = 111)


save(imp_male, file = "imp_male.RData")

imp_male_long <- complete(imp_male, action = 'long', include = TRUE)


save(imp_male_long, file = "imp_male_long.RData")

###############################
#################################### WEIGHTING INTERACTIONS
#########################################
library(MatchThem)
load("imp_female.RData")




weightdat_female <- weightthem(PA:age ~   
                               + mhseeking 
                               + genderdiff
                               + education
                               + employment
                               + relationship
                               + socialize
                               + sleep
                               + meddiagnosis
                               + childtrauma
                               + adulttrauma,
                               imp_female, 
                               approach = 'within',  
                               method = "cbps",        
                               estimand = "ATE")

save(weightdat_female, file = "weightdat_female.RData")


# Estimating weights     | dataset: #1Error: No missing values are allowed in the treatment variable.
#   In addition: Warning messages:
#   1: In Ops.factor(PA, age) : ‘*’ not meaningful for factors
# 2: In Ops.factor(PA, age) : ‘*’ not meaningful for factors 




# love.plot(weightdat_female, binary = "std", var.order = "un", stats = "m",
#           thresholds = c(.10, .05)) + theme(legend.position = "top")
# 

######################

load("imp_male.RData")

weightdat_male <- weightthem(PA:age ~   
                             + mhseeking
                             + genderdiff
                             + education
                             + employment
                             + relationship
                             + socialize
                             + sleep
                             + meddiagnosis
                             + childtrauma
                             + adulttrauma,
                             imp_male, 
                             approach = 'within',  
                             method = "cbps",        
                             estimand = "ATE") 

save(weightdat_male, file = "weightdat_male.RData")

# 
# 
# love.plot(weightdat_male, binary = "std", var.order = "un", stats = "m",
#           thresholds = c(.10, .05)) + theme(legend.position = "top")
# 
# 






