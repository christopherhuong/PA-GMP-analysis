library(tidyverse)
library(psych)
library(table1)




load('mhm.RData')


mhm$PA <- factor(mhm$PA, 
          labels = c("Inactive", "Active"))



mhm$age <- factor(mhm$age, 
           labels = c("18-24", "25-34", "35-44",
                      "45-54", "55-64", "65-74",
                      "75-84", "85+"))

mhm <- mhm %>%
  mutate(education = case_when(
    education == "less.hs" ~ "Less than High School",
    education == "hs" ~ "High School",
    education == "vocational" ~ "Vocational Certification",
    education == "assoc.deg" ~ "Associate's Degree",
    education == "bach.deg" ~ "Bachelor's Degree",
    education == "grad.deg" ~ "Graduate Degree",
    education == "other" ~ "Other"
  ))

mhm$education <- factor(mhm$education)

load("dat.RData")

dat <- dat %>%
  subset(Frequency.of.doing.exercise == "Every day" |
           Frequency.of.doing.exercise == "Few days a week" |
           Frequency.of.doing.exercise == "Less than once a week" |
           Frequency.of.doing.exercise == "Once a week" |
           Frequency.of.doing.exercise == "Rarely/Never")

mhm$country <- dat$Country
sort(table(mhm$country), decreasing = T)[1:10]




label(mhm$mhq) <- "MHQ*"
label(mhm$cog) <- "Core Cognition*"
label(mhm$adaptresil) <- "Adaptability and Resilience*"
label(mhm$drivemotiv) <- "Drive and Motivation*"
label(mhm$moodoutlook) <- "Mood and Outlook*"
label(mhm$socialself) <- "Social Self*"
label(mhm$mindbody) <- "Mind-Body*"
label(mhm$PA) <- "Physical Activity"
label(mhm$age) <- "Age"
label(mhm$sex) <- "Sex"
label(mhm$country) <- "Country"
label(mhm$education) <- "Education"
label(mhm$employment) <- "Employment"
label(mhm$relationship) <- "Relationship Status"
label(mhm$socialize) <- "Socialize Frequency"
label(mhm$sleep) <- "Adequate Sleep Frequency"
label(mhm$meddiagnosis) <- "Medical Diagnosis"
label(mhm$mhseeking) <- "Mental Health Treatment in Past Year"
label(mhm$childtrauma) <- "Experienced Childhood Trauma"
label(mhm$adulttrauma) <- "Experienced Adult Trauma"



mhm_summary <- mhm




save(mhm_summary, file = "mhm_summary.RData")














