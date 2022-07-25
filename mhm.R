library(readr)
library(curl)
library(tidyverse)
library(psych)
library(corrplot)
library(ggplot2)
library(car)
library(qgraph)
library(ppcor)

dat1 <- read.csv("mhm_data_2022-07-21_19-46-52.csv", 
                 stringsAsFactors = T, na.strings = "")


#############################################################
summary(dat1$Biological.Sex)
summary(dat1$Is.identity.different.from.biological.sex)
summary(dat1$Country)
summary(dat1$What.is.your.current.family.situation.)
summary(dat1$Ethnicity)
summary(dat1$Education)
summary(dat1$Employment)
summary(dat1$Household.Income)






summary(dat1$Overall.MHQ)
summary(dat1$Core.Cognition)
summary(dat1$Complex.Cognition)
summary(dat1$Drive...Motivation)
summary(dat1$Mood...Outlook)
summary(dat1$Social...Self)
summary(dat1$Mind.Body.Connection)


tapply(dat1$Country, dat1$Household.Income, summary)
#check summary statistics by group. seems like only US, india, and germany have income data











########################################################################
dat2 <- select(dat1, Adaptability.to.Change:Adult.traumas)

dat2 <- dat2[,-which(sapply(dat2, class) == "factor")]
dat2 <- select(dat2, -c(49:54))
#removes all factor variables

cormax <- cor(dat2, use = "pairwise.complete.obs")
KMO(dat2)
cortest.bartlett(dat2)
det(cormax)

### Normality w MVN package
#https://cran.r-project.org/web/packages/MVN/vignettes/MVN.pdf
library(MVN)
result <- mvn(dat2, mvnTest = "mardia")
result$multivariateNormality


parallel <- 
  fa.parallel(dat2, 
              fm="ml", fa="fa") 



obs = data.frame(parallel$fa.values)
obs$type = c('Observed Data')
obs$num = c(row.names(obs))
obs$num = as.numeric(obs$num)
colnames(obs) = c('eigenvalue', 'type', 'num')

View(obs)





























