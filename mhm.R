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
summary(dat1$Age)
summary(dat1$Biological.Sex)        #138
summary(dat1$Is.identity.different.from.biological.sex)   #23743
summary(dat1$Country)            #0
summary(dat1$What.is.your.current.family.situation.)   #633
summary(dat1$Ethnicity)    #35323
summary(dat1$Education)     #0
summary(dat1$Employment)     #0
summary(dat1$In.general..I.get.as.much.sleep.as.I.need.)
summary(dat1$How.regularly.do.you.socialize.with.friends.in.person.)
summary(dat1$Do.you.have.a.diagnosed.medical.disorder.that.significantly.impacts.your.way.of.life.)
summary(dat1$Are.you.currently.seeking.treatment.for.any.mental.health.concerns.) #PNS to NA
summary(dat1$Chilhood.traumas) #code to Y/N/NA
summary(dat1$Diagnosed.mental.health.disorders) #lots of NA




summary(dat1$How.regularly.to.you.engage.in.physical.exercise..30.minutes.or.more..)

summary(dat1$Overall.MHQ)
summary(dat1$Core.Cognition)
summary(dat1$Complex.Cognition)
summary(dat1$Drive...Motivation)
summary(dat1$Mood...Outlook)
summary(dat1$Social...Self)
summary(dat1$Mind.Body.Connection)


tapply(dat1$Country, dat1$Household.Income, summary)
#check summary statistics by group. seems like only US, india, and germany have income data

#missingness
library(mice)




##################### MLM ################
library(nlme)



######
dat1$MHQ <- dat1$Overall.MHQ
mhm <- select(dat1, MHQ)


mhm <- mhm %>%
  add_column(
             Core.Cog = dat1$Core.Cognition,
             Complex.Cog = dat1$Complex.Cognition,
             Drive.Motivation = dat1$Drive...Motivation,
             Mood.Outlook = dat1$Mood...Outlook,
             Social.Self = dat1$Social...Self,
             Mind.Body = dat1$Mind.Body.Connection,
             Exercise = dat1$How.regularly.to.you.engage.in.physical.exercise..30.minutes.or.more..,
             Sex = dat1$Biological.Sex,
             Identity.Diff = dat1$Is.identity.different.from.biological.sex,
             Country = dat1$Country,
             Education = dat1$Education,
             Employment = dat1$Employment,
             Ethnicity = dat1$Ethnicity,
             Relationship.Status = dat1$What.is.your.current.family.situation.,
             Sleep = dat1$In.general..I.get.as.much.sleep.as.I.need.,
             Socialize = dat1$How.regularly.do.you.socialize.with.friends.in.person.,
             Disability = dat1$Do.you.have.a.diagnosed.medical.disorder.that.significantly.impacts.your.way.of.life.,
             Seek.MH.Treatment = dat1$Are.you.currently.seeking.treatment.for.any.mental.health.concerns.,
             Childhood.Trauma = dat1$Chilhood.traumas)





mhm[mhm == "Prefer not to say"] <- NA

mhm$Exercise[mhm$Exercise %in% "Einmal in der Woche"] <- "Once a week"

mhm$Exercise <- ifelse(mhm$Exercise == "Every day", 1, 0)

sum(mhm$Exercise == 1)


mhm$Childhood.Trauma <- if_else(mhm$Childhood.Trauma == "|I did not experience any of the above during my childhood", "No", "Yes")
mhm$Childhood.Trauma <- as.factor(mhm$Childhood.Trauma)







summary(lm(MHQ ~ Exercise, data = mhm))



ggplot(data=mhm, aes(x=Exercise, y=MHQ, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=len), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()




########################################################################
dat2 <- select(dat1, Adaptability.to.Change:Adult.traumas)

dat2 <- dat2[,-which(sapply(dat2, class) == "factor")]
dat2 <- select(dat2, -c(49:54))
#removes all factor variables for correlation matrix

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















































