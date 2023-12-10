library(tidyverse)
library(survey)
library(knitr)
library(ggplot2)



load('mhm.rdata')


load('gbm_interact1.rdata')
load('gbm_interact2.rdata')
load('gbm_interact3.rdata')
load('gbm_interact4.rdata')
load('gbm_interact5.rdata')
load('gbm_interact6.rdata')
load('gbm_interact7.rdata')
load('gbm_interact8.rdata')





interact <- function(a,b) {
  des_int <- svydesign(ids = ~country, weights = a$weights,
                       data = subset(mhm, age==b))
  
  mhq_interact <- svyglm(mhq ~ PA,
                         design = des_int,
                         family = gaussian())
  
}



a<-interact(gbm_interact1, 1)
a$coefficients
confint(a)
a<-interact(gbm_interact2, 2)
a$coefficients
confint(a)
a<-interact(gbm_interact3, 3)
a$coefficients
confint(a)
a<-interact(gbm_interact4, 4)
a$coefficients
confint(a)
a<-interact(gbm_interact5, 5)
a$coefficients
confint(a)
a<-interact(gbm_interact6, 6)
a$coefficients
confint(a)
a<-interact(gbm_interact7, 7)
a$coefficients
confint(a)
a<-interact(gbm_interact8, 8)
a$coefficients
confint(a)









# cog ---------------------------------------------------------------------


interact <- function(a,b) {
  des_int <- svydesign(ids = ~country, weights = a$weights,
                       data = subset(mhm, age==b))
  
  mhq_interact <- svyglm(cog ~ PA,
                         design = des_int,
                         family = gaussian())
  
  
}



a<-interact(gbm_interact1, 1)
a$coefficients
confint(a)
a<-interact(gbm_interact2, 2)
a$coefficients
confint(a)
a<-interact(gbm_interact3, 3)
a$coefficients
confint(a)
a<-interact(gbm_interact4, 4)
a$coefficients
confint(a)
a<-interact(gbm_interact5, 5)
a$coefficients
confint(a)
a<-interact(gbm_interact6, 6)
a$coefficients
confint(a)
a<-interact(gbm_interact7, 7)
a$coefficients
confint(a)
a<-interact(gbm_interact8, 8)
a$coefficients
confint(a)



# adaptresil --------------------------------------------------------------


interact <- function(a,b) {
  des_int <- svydesign(ids = ~country, weights = a$weights,
                       data = subset(mhm, age==b))
  
  mhq_interact <- svyglm(adaptresil ~ PA,
                         design = des_int,
                         family = gaussian())
  
  
}


a<-interact(gbm_interact1, 1)
a$coefficients
confint(a)
a<-interact(gbm_interact2, 2)
a$coefficients
confint(a)
a<-interact(gbm_interact3, 3)
a$coefficients
confint(a)
a<-interact(gbm_interact4, 4)
a$coefficients
confint(a)
a<-interact(gbm_interact5, 5)
a$coefficients
confint(a)
a<-interact(gbm_interact6, 6)
a$coefficients
confint(a)
a<-interact(gbm_interact7, 7)
a$coefficients
confint(a)
a<-interact(gbm_interact8, 8)
a$coefficients
confint(a)




# moodoutlook -------------------------------------------------------------



interact <- function(a,b) {
  des_int <- svydesign(ids = ~country, weights = a$weights,
                       data = subset(mhm, age==b))
  
  mhq_interact <- svyglm(moodoutlook ~ PA,
                         design = des_int,
                         family = gaussian())
  

}




a<-interact(gbm_interact1, 1)
a$coefficients
confint(a)
a<-interact(gbm_interact2, 2)
a$coefficients
confint(a)
a<-interact(gbm_interact3, 3)
a$coefficients
confint(a)
a<-interact(gbm_interact4, 4)
a$coefficients
confint(a)
a<-interact(gbm_interact5, 5)
a$coefficients
confint(a)
a<-interact(gbm_interact6, 6)
a$coefficients
confint(a)
a<-interact(gbm_interact7, 7)
a$coefficients
confint(a)
a<-interact(gbm_interact8, 8)
a$coefficients
confint(a)



# drive motivation --------------------------------------------------------

interact <- function(a,b) {
  des_int <- svydesign(ids = ~country, weights = a$weights,
                       data = subset(mhm, age==b))
  
  mhq_interact <- svyglm(drivemotiv ~ PA,
                         design = des_int,
                         family = gaussian())
  
  
}




a<-interact(gbm_interact1, 1)
a$coefficients
confint(a)
a<-interact(gbm_interact2, 2)
a$coefficients
confint(a)
a<-interact(gbm_interact3, 3)
a$coefficients
confint(a)
a<-interact(gbm_interact4, 4)
a$coefficients
confint(a)
a<-interact(gbm_interact5, 5)
a$coefficients
confint(a)
a<-interact(gbm_interact6, 6)
a$coefficients
confint(a)
a<-interact(gbm_interact7, 7)
a$coefficients
confint(a)
a<-interact(gbm_interact8, 8)
a$coefficients
confint(a)




# social self -------------------------------------------------------------


interact <- function(a,b) {
  des_int <- svydesign(ids = ~country, weights = a$weights,
                       data = subset(mhm, age==b))
  
  mhq_interact <- svyglm(socialself ~ PA,
                         design = des_int,
                         family = gaussian())
  

}



a<-interact(gbm_interact1, 1)
a$coefficients
confint(a)
a<-interact(gbm_interact2, 2)
a$coefficients
confint(a)
a<-interact(gbm_interact3, 3)
a$coefficients
confint(a)
a<-interact(gbm_interact4, 4)
a$coefficients
confint(a)
a<-interact(gbm_interact5, 5)
a$coefficients
confint(a)
a<-interact(gbm_interact6, 6)
a$coefficients
confint(a)
a<-interact(gbm_interact7, 7)
a$coefficients
confint(a)
a<-interact(gbm_interact8, 8)
a$coefficients
confint(a)


# mindbody ----------------------------------------------------------------



interact <- function(a,b) {
  des_int <- svydesign(ids = ~country, weights = a$weights,
                       data = subset(mhm, age==b))
  
  mhq_interact <- svyglm(mindbody ~ PA,
                         design = des_int,
                         family = gaussian())
  
  
}



a<-interact(gbm_interact1, 1)
a$coefficients
confint(a)
a<-interact(gbm_interact2, 2)
a$coefficients
confint(a)
a<-interact(gbm_interact3, 3)
a$coefficients
confint(a)
a<-interact(gbm_interact4, 4)
a$coefficients
confint(a)
a<-interact(gbm_interact5, 5)
a$coefficients
confint(a)
a<-interact(gbm_interact6, 6)
a$coefficients
confint(a)
a<-interact(gbm_interact7, 7)
a$coefficients
confint(a)
a<-interact(gbm_interact8, 8)
a$coefficients
confint(a)



# plots -------------------------------------------------------------------

library(ggplot2)

mhq <- data.frame(age = c('18-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
                  ATC = c(17.77,	19.79,	20.41,	18.65,	15.76,	12.37,	11.24,	23.37),
                  low = c(15.55,	17.13,	17.53,	15.15,	11.96,	9.65,	7.89,	12.47),
                  up = c(19.99,	22.45,	23.29,	22.15,	19.56,	15.10,	14.59,	34.28))


ggplot(mhq, aes(x=age, y=ATC, group=1)) + 
  geom_errorbar(aes(ymin=low, ymax=up), width=.2) +
  geom_line() + geom_point() + theme_minimal() +
  labs(title = "Effect of PA on MHQ with 95% CI's", y = "ATC", x = "Age")


a <- c('18-24','25-34','35-44','45-54','55-64','65-74','75-84','85+')
b <- c(17.29,	18.56,	18.79,	16.66,	13.78,	9.74,	  7.84,	  19.25,
       20.1,	18.71,	18.96,	16.87,	14.79,	14.5, 	12.18,	21.93,
       13.71,	16.64,	17.94,	16.81,	13.9, 	10.74,	9.98,  	19.4,
       18.27,	18.34,	17.94,	15.65,	11.71,	9.88,	  8.29,	  20.14,
       10.96,	14.33,	15.42,	14.84,	12.45,	9.04,	  8.02,	  19.22,
       19.54,	21.38,	21.12,	19.46,	16.67,	15.38,	15.36,	25.52)

c <- c(15.48,	16.36,	16.18,	13.80,	10.92,	7.33,	4.56,	10.71,
       17.41,	15.58,	15.80,	13.34,	11.05,	11.34,	8.70,  	14.04,
       11.62,	14.05,	15.09,	13.28,	9.88,	  8.05,	  7.10,	  9.27,
       15.88,	15.44,	14.67,	11.97,	7.91,	  6.71,	  4.60,	  12.04,
       8.43,	11.39,	12.49,	11.36,	8.82,	  6.29,  	4.54,	  7.85,
       17.50,	18.74,	18.59,	15.79,	12.39,	11.84,	10.91,	16.83)

d <- c(19.09,	20.76,	21.40,	19.52,	16.63,	12.14,	11.12,	27.78,
       22.80,	21.85,	22.12,	20.39,	18.53,	17.66,	15.65,	29.82,
       15.80,	19.23,	20.78,	20.35,	17.90,	13.42,	12.86,	29.53,
       20.66,	21.23,	21.22,	19.33,	15.51,	13.04,	11.98,	28.23,
       13.50,	17.26,	18.35,	18.32,	16.09,	11.79,	11.49,	30.59,
       21.58,	24.01,	23.66,	23.13,	20.95,	18.91,	19.80,	34.22)

e <- c("Cog", "Cog", "Cog", "Cog", "Cog", "Cog", "Cog", "Cog", 
       "Adapt/Resil", "Adapt/Resil", "Adapt/Resil", "Adapt/Resil", "Adapt/Resil", "Adapt/Resil", "Adapt/Resil", "Adapt/Resil", 
       "Mood/Outlook", "Mood/Outlook", "Mood/Outlook", "Mood/Outlook", "Mood/Outlook", "Mood/Outlook", "Mood/Outlook", "Mood/Outlook", 
       "Drive/Motivation", "Drive/Motivation", "Drive/Motivation", "Drive/Motivation", "Drive/Motivation", "Drive/Motivation", "Drive/Motivation", "Drive/Motivation", 
       "Social Self", "Social Self", "Social Self", "Social Self", "Social Self", "Social Self", "Social Self", "Social Self", 
       "Mind Body", "Mind Body", "Mind Body", "Mind Body", "Mind Body", "Mind Body", "Mind Body", "Mind Body")



subdomains <- data.frame(age = c(a,a,a,a,a,a),
                         ATC = b,
                         low = c,
                         up = d,
                         grp = e)


ggplot(subdomains, aes(x=age, y=ATC, group=grp, color = grp)) + 
  geom_errorbar(aes(ymin=low, ymax=up), width=.5, position=position_dodge(0.1)) +
  geom_line() + geom_point() + theme_minimal() +
  labs(title = "Effect of PA on Subdomains", y = "ATC", x = "Age")



# nope 6 plots -------------------------------------------------------------



# cog ---------------------------------------------------------------------


ggplot(subset(subdomains, grp == "Cog"), aes(x=age, y=ATC, group = 1)) + 
  geom_errorbar(aes(ymin=low, ymax=up), width=.5, position=position_dodge(0.1)) +
  geom_line() + geom_point() + theme_minimal() +
  labs(title = "Core Cognition", y = "ATC", x = "Age") +
  ylim(0 , 35)



# adapt/resil -------------------------------------------------------------

ggplot(subset(subdomains, grp == "Adapt/Resil"), aes(x=age, y=ATC, group = 1)) + 
  geom_errorbar(aes(ymin=low, ymax=up), width=.5, position=position_dodge(0.1)) +
  geom_line() + geom_point() + theme_minimal() +
  labs(title = "Adaptability and Resilience", y = "ATC", x = "Age") +
  ylim(0, 35)


# mood/outlook ------------------------------------------------------------

ggplot(subset(subdomains, grp == "Mood/Outlook"), aes(x=age, y=ATC, group = 1)) + 
  geom_errorbar(aes(ymin=low, ymax=up), width=.5, position=position_dodge(0.1)) +
  geom_line() + geom_point() + theme_minimal() +
  labs(title = "Mood and Outlook", y = "ATC", x = "Age") +
  ylim(0, 35)


# Drive and Motivation ----------------------------------------------------

ggplot(subset(subdomains, grp == "Drive/Motivation"), aes(x=age, y=ATC, group = 1)) + 
  geom_errorbar(aes(ymin=low, ymax=up), width=.5, position=position_dodge(0.1)) +
  geom_line() + geom_point() + theme_minimal() +
  labs(title = "Drive and Motivation", y = "ATC", x = "Age") +
  ylim(0, 35)



# social self -------------------------------------------------------------

ggplot(subset(subdomains, grp == "Social Self"), aes(x=age, y=ATC, group = 1)) + 
  geom_errorbar(aes(ymin=low, ymax=up), width=.5, position=position_dodge(0.1)) +
  geom_line() + geom_point() + theme_minimal() +
  labs(title = "Social Self", y = "ATC", x = "Age") +
  ylim(0, 35)


# mind body ---------------------------------------------------------------

ggplot(subset(subdomains, grp == "Mind Body"), aes(x=age, y=ATC, group = 1)) + 
  geom_errorbar(aes(ymin=low, ymax=up), width=.5, position=position_dodge(0.1)) +
  geom_line() + geom_point() + theme_minimal() +
  labs(title = "Mind Body", y = "ATC", x = "Age") +
  ylim(0, 35)



################################################

# average PA by age and activity --------------------------------------------------------------
library(tidyverse)

load('mhm.rdata')

mhm <- mhm %>%
  mutate(age1 = case_when(age == 1 ~ "18-24",
                   age == 2 ~ "25-34",
                   age == 3 ~ "35-44",
                   age == 4 ~ "45-54",
                   age == 5 ~ "55-64",
                   age == 6 ~ "65-74",
                   age == 7 ~ "75-84",
                   age == 8 ~ "85+"))
mhm$age1 <- as.factor(mhm$age1)
mhm$PA <- factor(mhm$PA,
                 labels = c("Inactive", "Active"))



  
library(lme4)
library(sjPlot)
library(effects)
library(ggplot2)




mod1 <- lmer(mhq~ -1 + PA*age1 +( 1 | country ),
             data = mhm)

summary(mod1)




eff.p1 <- effects::effect(term= "PA*age1", mod= mod1)
plot(eff.p1)



eff.p1 <- as.data.frame(eff.p1)
ggplot(eff.p1, aes(age1, linetype=factor(PA),
                   color = factor(PA))) +
  geom_line(aes(y = fit, group=factor(PA)), linewidth=1.2) +
  geom_line(aes(y = lower,
                group=factor(PA)), linetype =2) +
  geom_line(aes(y = upper,
                group=factor(PA)), linetype =2) +
  xlab("Age") +
  ylab("MHQ") +
  scale_colour_discrete("") +
  scale_linetype_discrete("") +
  labs(color='PA', title = "Marginal effects on MHQ by age for inactive and active groups") + theme_minimal()



# corecog -----------------------------------------------------------------


mod2 <- lmer(cog~ -1 + PA*age1 +( 1 | country ),
             data = mhm)

summary(mod2)




eff.p2 <- effects::effect(term= "PA*age1", mod= mod2)
plot(eff.p2)



eff.p2 <- as.data.frame(eff.p2)
ggplot(eff.p2, aes(age1, linetype=factor(PA),
                   color = factor(PA))) +
  geom_line(aes(y = fit, group=factor(PA)), linewidth=1.2) +
  geom_line(aes(y = lower,
                group=factor(PA)), linetype =2) +
  geom_line(aes(y = upper,
                group=factor(PA)), linetype =2) +
  xlab("Age") +
  ylab("Core Cognition") +
  ylim(0,150)+
  scale_colour_discrete("") +
  scale_linetype_discrete("") +
  labs(color='PA') + theme_minimal()





# adaptresil --------------------------------------------------------------

  
mod3 <- lmer(adaptresil~ -1 + PA*age1 +( 1 | country ),
               data = mhm)
  
summary(mod3)
  
  
  
  
eff.p3 <- effects::effect(term= "PA*age1", mod= mod3)

  
  
  
eff.p3 <- as.data.frame(eff.p3)
ggplot(eff.p3, aes(age1, linetype=factor(PA),
                     color = factor(PA))) +
    geom_line(aes(y = fit, group=factor(PA)), linewidth=1.2) +
    geom_line(aes(y = lower,
                  group=factor(PA)), linetype =2) +
    geom_line(aes(y = upper,
                  group=factor(PA)), linetype =2) +
    xlab("Age") +
    ylab("Adaptability and Resilience") +
    ylim(0,150)+
    scale_colour_discrete("") +
    scale_linetype_discrete("") +
    labs(color='PA') + theme_minimal()
  
  


# drivemotiv --------------------------------------------------------------



mod4 <- lmer(drivemotiv~ -1 + PA*age1 +( 1 | country ),
               data = mhm)
  
summary(mod4)
  
  
  
  
eff.p4 <- effects::effect(term= "PA*age1", mod= mod4)
  
  
  
  
eff.p4 <- as.data.frame(eff.p4)
ggplot(eff.p4, aes(age1, linetype=factor(PA),
                     color = factor(PA))) +
    geom_line(aes(y = fit, group=factor(PA)), linewidth=1.2) +
    geom_line(aes(y = lower,
                  group=factor(PA)), linetype =2) +
    geom_line(aes(y = upper,
                  group=factor(PA)), linetype =2) +
    xlab("Age") +
    ylab("Drive and Motivation") +
    ylim(0,150)+
    scale_colour_discrete("") +
    scale_linetype_discrete("") +
    labs(color='PA') + theme_minimal()
  
  

# Mood Outlook ------------------------------------------------------------



mod5 <- lmer(moodoutlook~ -1 + PA*age1 +( 1 | country ),
             data = mhm)

summary(mod5)




eff.p5 <- effects::effect(term= "PA*age1", mod= mod5)




eff.p5 <- as.data.frame(eff.p5)
ggplot(eff.p5, aes(age1, linetype=factor(PA),
                   color = factor(PA))) +
  geom_line(aes(y = fit, group=factor(PA)), linewidth=1.2) +
  geom_line(aes(y = lower,
                group=factor(PA)), linetype =2) +
  geom_line(aes(y = upper,
                group=factor(PA)), linetype =2) +
  xlab("Age") +
  ylab("Mood and Outlook") +
  ylim(0,150)+
  scale_colour_discrete("") +
  scale_linetype_discrete("") +
  labs(color='PA') + theme_minimal()



# social self -------------------------------------------------------------



mod6 <- lmer(socialself~ -1 + PA*age1 +( 1 | country ),
             data = mhm)

summary(mod6)




eff.p6 <- effects::effect(term= "PA*age1", mod= mod6)




eff.p6 <- as.data.frame(eff.p6)
ggplot(eff.p6, aes(age1, linetype=factor(PA),
                   color = factor(PA))) +
  geom_line(aes(y = fit, group=factor(PA)), linewidth=1.2) +
  geom_line(aes(y = lower,
                group=factor(PA)), linetype =2) +
  geom_line(aes(y = upper,
                group=factor(PA)), linetype =2) +
  xlab("Age") +
  ylab("Social Self") +
  ylim(0,150)+
  scale_colour_discrete("") +
  scale_linetype_discrete("") +
  labs(color='PA') + theme_minimal()




# mindbody ----------------------------------------------------------------

mod7 <- lmer(mindbody ~ -1 + PA*age1 +( 1 | country ),
             data = mhm)

summary(mod7)




eff.p7 <- effects::effect(term= "PA*age1", mod= mod7)




eff.p7 <- as.data.frame(eff.p7)
ggplot(eff.p7, aes(age1, linetype=factor(PA),
                   color = factor(PA))) +
  geom_line(aes(y = fit, group=factor(PA)), linewidth=1.2) +
  geom_line(aes(y = lower,
                group=factor(PA)), linetype =2) +
  geom_line(aes(y = upper,
                group=factor(PA)), linetype =2) +
  xlab("Age") +
  ylab("Mind-Body Connection") +
  ylim(0,150)+
  scale_colour_discrete("") +
  scale_linetype_discrete("") +
  labs(color='PA') + theme_minimal()



