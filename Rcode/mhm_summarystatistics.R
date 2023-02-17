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



dat <- dat %>%
  subset(Frequency.of.doing.exercise == "Every day" |
           Frequency.of.doing.exercise == "Few days a week" |
           Frequency.of.doing.exercise == "Less than once a week" |
           Frequency.of.doing.exercise == "Once a week" |
           Frequency.of.doing.exercise == "Rarely/Never")

mhm$country <- dat$Country
sort(table(mhm$country), decreasing = T)[1:10]


colnames(mhm) <- 
  c("MHQ", "Core Cognition", "Adaptability and Resilience",
    "Drive and Motivation", "Mood and Outlook", "Social Self",
    "Mind-Body", 
    "Physical Activity", "Age", "Sex", "Country", "Education",
    "Employment", "Relationship", "Socialize Frequency",
    "Adequate Sleep Frequency", "Medical Diagnosis",
    "Mental Health Treatment in Past Year", 
    "Experienced Childhood Trauma", "Experienced Adult Trauma")

















