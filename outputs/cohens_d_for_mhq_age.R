library(tidyverse)
load('mhm.rdata')





#cohen's d caclulation
sd1 <- subset(mhm, PA == 0)
sd1 <-  sd(sd1$mhq)
sd2 <- subset(mhm, PA == 1)
sd2 <-  sd(sd2$mhq)
17.86/(sqrt((sd1^2+sd2^2)/2)) # = 0.25



sd1 <- subset(mhm, age == 1 & PA == 0)
sd11 <-  sd(sd1$mhq)
sd2 <- subset(mhm, age == 1 & PA == 1)
sd22 <-  sd(sd2$mhq)
17.77/sqrt((sd11^2+sd22^2)/2)
#0.2624113 age 18-24





sd1 <- subset(mhm, age == 2 & PA == 0)
sd11 <-  sd(sd1$mhq)
sd2 <- subset(mhm, age == 2 & PA == 1)
sd22 <-  sd(sd2$mhq)
19.79/sqrt((sd11^2+sd22^2)/2)
#0.2914027 age 25-34


sd1 <- subset(mhm, age == 3 & PA == 0)
sd11 <-  sd(sd1$mhq)
sd2 <- subset(mhm, age == 3 & PA == 1)
sd22 <-  sd(sd2$mhq)
20.41/sqrt((sd11^2+sd22^2)/2)
#0.3049566 age 35-44


sd1 <- subset(mhm, age == 4 & PA == 0)
sd11 <-  sd(sd1$mhq)
sd2 <- subset(mhm, age == 4 & PA == 1)
sd22 <-  sd(sd2$mhq)
18.65/sqrt((sd11^2+sd22^2)/2)
#0.2809114 age 45-54

sd1 <- subset(mhm, age == 5 & PA == 0)
sd11 <-  sd(sd1$mhq)
sd2 <- subset(mhm, age == 5 & PA == 1)
sd22 <-  sd(sd2$mhq)
15.76/sqrt((sd11^2+sd22^2)/2)
#0.2414 age 55-64


sd1 <- subset(mhm, age == 6 & PA == 0)
sd11 <-  sd(sd1$mhq)
sd2 <- subset(mhm, age == 6 & PA == 1)
sd22 <-  sd(sd2$mhq)
12.37/sqrt((sd11^2+sd22^2)/2)
#0.204 65-74



sd1 <- subset(mhm, age == 7 & PA == 0)
sd11 <-  sd(sd1$mhq)
sd2 <- subset(mhm, age == 7 & PA == 1)
sd22 <-  sd(sd2$mhq)
11.24/sqrt((sd11^2+sd22^2)/2)
#0.200



sd1 <- subset(mhm, age == 8 & PA == 0)
sd11 <-  sd(sd1$mhq)
sd2 <- subset(mhm, age == 8 & PA == 1)
sd22 <-  sd(sd2$mhq)
23.37/sqrt((sd11^2+sd22^2)/2)
#0.3484




# core cog ----------------------------------------------------------------



sd1 <- subset(mhm, age == 1 & PA == 0)
sd11 <-  sd(sd1$cog)
sd2 <- subset(mhm, age == 1 & PA == 1)
sd22 <-  sd(sd2$cog)
17.29/sqrt((sd11^2+sd22^2)/2)
#0.28 age 18-24





sd1 <- subset(mhm, age == 2 & PA == 0)
sd11 <-  sd(sd1$cog)
sd2 <- subset(mhm, age == 2 & PA == 1)
sd22 <-  sd(sd2$cog)
18.56/sqrt((sd11^2+sd22^2)/2)
#0.29 age 25-34


sd1 <- subset(mhm, age == 3 & PA == 0)
sd11 <-  sd(sd1$cog)
sd2 <- subset(mhm, age == 3 & PA == 1)
sd22 <-  sd(sd2$cog)
18.79/sqrt((sd11^2+sd22^2)/2)
#0.30 age 35-44


sd1 <- subset(mhm, age == 4 & PA == 0)
sd11 <-  sd(sd1$cog)
sd2 <- subset(mhm, age == 4 & PA == 1)
sd22 <-  sd(sd2$cog)
16.66/sqrt((sd11^2+sd22^2)/2)
#0.27 age 45-54

sd1 <- subset(mhm, age == 5 & PA == 0)
sd11 <-  sd(sd1$cog)
sd2 <- subset(mhm, age == 5 & PA == 1)
sd22 <-  sd(sd2$cog)
13.78/sqrt((sd11^2+sd22^2)/2)
#0.23 age 55-64


sd1 <- subset(mhm, age == 6 & PA == 0)
sd11 <-  sd(sd1$cog)
sd2 <- subset(mhm, age == 6 & PA == 1)
sd22 <-  sd(sd2$cog)
9.74/sqrt((sd11^2+sd22^2)/2)
#0.17 65-74



sd1 <- subset(mhm, age == 7 & PA == 0)
sd11 <-  sd(sd1$cog)
sd2 <- subset(mhm, age == 7 & PA == 1)
sd22 <-  sd(sd2$cog)
7.84/sqrt((sd11^2+sd22^2)/2)
#0.15 75-84



sd1 <- subset(mhm, age == 8 & PA == 0)
sd11 <-  sd(sd1$cog)
sd2 <- subset(mhm, age == 8 & PA == 1)
sd22 <-  sd(sd2$cog)
19.25/sqrt((sd11^2+sd22^2)/2)
#0.32  85+







# adaptability and resil --------------------------------------------------



sd1 <- subset(mhm, age == 1 & PA == 0)
sd11 <-  sd(sd1$adaptresil)
sd2 <- subset(mhm, age == 1 & PA == 1)
sd22 <-  sd(sd2$adaptresil)
20.10/sqrt((sd11^2+sd22^2)/2)
#0.30 age 18-24




sd1 <- subset(mhm, age == 2 & PA == 0)
sd11 <-  sd(sd1$adaptresil)
sd2 <- subset(mhm, age == 2 & PA == 1)
sd22 <-  sd(sd2$adaptresil)
18.71/sqrt((sd11^2+sd22^2)/2)
#0.28 age 25-34


sd1 <- subset(mhm, age == 3 & PA == 0)
sd11 <-  sd(sd1$adaptresil)
sd2 <- subset(mhm, age == 3 & PA == 1)
sd22 <-  sd(sd2$adaptresil)
18.96/sqrt((sd11^2+sd22^2)/2)
#0.30 age 35-44


sd1 <- subset(mhm, age == 4 & PA == 0)
sd11 <-  sd(sd1$adaptresil)
sd2 <- subset(mhm, age == 4 & PA == 1)
sd22 <-  sd(sd2$adaptresil)
16.87/sqrt((sd11^2+sd22^2)/2)
#0.27 age 45-54

sd1 <- subset(mhm, age == 5 & PA == 0)
sd11 <-  sd(sd1$adaptresil)
sd2 <- subset(mhm, age == 5 & PA == 1)
sd22 <-  sd(sd2$adaptresil)
14.79/sqrt((sd11^2+sd22^2)/2)
#0.24 age 55-64


sd1 <- subset(mhm, age == 6 & PA == 0)
sd11 <-  sd(sd1$adaptresil)
sd2 <- subset(mhm, age == 6 & PA == 1)
sd22 <-  sd(sd2$adaptresil)
14.50/sqrt((sd11^2+sd22^2)/2)
#0.25 65-74



sd1 <- subset(mhm, age == 7 & PA == 0)
sd11 <-  sd(sd1$adaptresil)
sd2 <- subset(mhm, age == 7 & PA == 1)
sd22 <-  sd(sd2$adaptresil)
12.18/sqrt((sd11^2+sd22^2)/2)
#0.24 75-84



sd1 <- subset(mhm, age == 8 & PA == 0)
sd11 <-  sd(sd1$adaptresil)
sd2 <- subset(mhm, age == 8 & PA == 1)
sd22 <-  sd(sd2$adaptresil)
21.93/sqrt((sd11^2+sd22^2)/2)
#0.37  85+




# mood outlook ------------------------------------------------------------




sd1 <- subset(mhm, age == 1 & PA == 0)
sd11 <-  sd(sd1$moodoutlook)
sd2 <- subset(mhm, age == 1 & PA == 1)
sd22 <-  sd(sd2$moodoutlook)
13.71/sqrt((sd11^2+sd22^2)/2)
#0.22 age 18-24


sd1 <- subset(mhm, age == 2 & PA == 0)
sd11 <-  sd(sd1$moodoutlook)
sd2 <- subset(mhm, age == 2 & PA == 1)
sd22 <-  sd(sd2$moodoutlook)
16.64/sqrt((sd11^2+sd22^2)/2)
#0.26 age 25-34


sd1 <- subset(mhm, age == 3 & PA == 0)
sd11 <-  sd(sd1$moodoutlook)
sd2 <- subset(mhm, age == 3 & PA == 1)
sd22 <-  sd(sd2$moodoutlook)
17.94/sqrt((sd11^2+sd22^2)/2)
#0.27 age 35-44


sd1 <- subset(mhm, age == 4 & PA == 0)
sd11 <-  sd(sd1$moodoutlook)
sd2 <- subset(mhm, age == 4 & PA == 1)
sd22 <-  sd(sd2$moodoutlook)
16.81/sqrt((sd11^2+sd22^2)/2)
#0.25 age 45-54

sd1 <- subset(mhm, age == 5 & PA == 0)
sd11 <-  sd(sd1$moodoutlook)
sd2 <- subset(mhm, age == 5 & PA == 1)
sd22 <-  sd(sd2$moodoutlook)
13.90/sqrt((sd11^2+sd22^2)/2)
#0.21 age 55-64


sd1 <- subset(mhm, age == 6 & PA == 0)
sd11 <-  sd(sd1$moodoutlook)
sd2 <- subset(mhm, age == 6 & PA == 1)
sd22 <-  sd(sd2$moodoutlook)
10.74/sqrt((sd11^2+sd22^2)/2)
#0.17 65-74



sd1 <- subset(mhm, age == 7 & PA == 0)
sd11 <-  sd(sd1$moodoutlook)
sd2 <- subset(mhm, age == 7 & PA == 1)
sd22 <-  sd(sd2$moodoutlook)
9.98/sqrt((sd11^2+sd22^2)/2)
#0.17 75-84



sd1 <- subset(mhm, age == 8 & PA == 0)
sd11 <-  sd(sd1$moodoutlook)
sd2 <- subset(mhm, age == 8 & PA == 1)
sd22 <-  sd(sd2$moodoutlook)
19.40/sqrt((sd11^2+sd22^2)/2)
#0.30  85+


# drive and motiv ---------------------------------------------------------



sd1 <- subset(mhm, age == 1 & PA == 0)
sd11 <-  sd(sd1$drivemotiv)
sd2 <- subset(mhm, age == 1 & PA == 1)
sd22 <-  sd(sd2$drivemotiv)
18.27/sqrt((sd11^2+sd22^2)/2)
#0.29 age 18-24


sd1 <- subset(mhm, age == 2 & PA == 0)
sd11 <-  sd(sd1$drivemotiv)
sd2 <- subset(mhm, age == 2 & PA == 1)
sd22 <-  sd(sd2$drivemotiv)
18.34/sqrt((sd11^2+sd22^2)/2)
#0.29 age 25-34


sd1 <- subset(mhm, age == 3 & PA == 0)
sd11 <-  sd(sd1$drivemotiv)
sd2 <- subset(mhm, age == 3 & PA == 1)
sd22 <-  sd(sd2$drivemotiv)
17.94/sqrt((sd11^2+sd22^2)/2)
#0.29 age 35-44


sd1 <- subset(mhm, age == 4 & PA == 0)
sd11 <-  sd(sd1$drivemotiv)
sd2 <- subset(mhm, age == 4 & PA == 1)
sd22 <-  sd(sd2$drivemotiv)
15.65/sqrt((sd11^2+sd22^2)/2)
#0.25 age 45-54

sd1 <- subset(mhm, age == 5 & PA == 0)
sd11 <-  sd(sd1$drivemotiv)
sd2 <- subset(mhm, age == 5 & PA == 1)
sd22 <-  sd(sd2$drivemotiv)
11.71/sqrt((sd11^2+sd22^2)/2)
#0.19 age 55-64


sd1 <- subset(mhm, age == 6 & PA == 0)
sd11 <-  sd(sd1$drivemotiv)
sd2 <- subset(mhm, age == 6 & PA == 1)
sd22 <-  sd(sd2$drivemotiv)
9.88/sqrt((sd11^2+sd22^2)/2)
#0.18 65-74



sd1 <- subset(mhm, age == 7 & PA == 0)
sd11 <-  sd(sd1$drivemotiv)
sd2 <- subset(mhm, age == 7 & PA == 1)
sd22 <-  sd(sd2$drivemotiv)
8.29/sqrt((sd11^2+sd22^2)/2)
#0.16 75-84



sd1 <- subset(mhm, age == 8 & PA == 0)
sd11 <-  sd(sd1$drivemotiv)
sd2 <- subset(mhm, age == 8 & PA == 1)
sd22 <-  sd(sd2$drivemotiv)
20.14/sqrt((sd11^2+sd22^2)/2)
#0.34  85+




# social self -------------------------------------------------------------




sd1 <- subset(mhm, age == 1 & PA == 0)
sd11 <-  sd(sd1$socialself)
sd2 <- subset(mhm, age == 1 & PA == 1)
sd22 <-  sd(sd2$socialself)
10.96/sqrt((sd11^2+sd22^2)/2)
#0.16 age 18-24


sd1 <- subset(mhm, age == 2 & PA == 0)
sd11 <-  sd(sd1$socialself)
sd2 <- subset(mhm, age == 2 & PA == 1)
sd22 <-  sd(sd2$socialself)
14.33/sqrt((sd11^2+sd22^2)/2)
#0.20 age 25-34


sd1 <- subset(mhm, age == 3 & PA == 0)
sd11 <-  sd(sd1$socialself)
sd2 <- subset(mhm, age == 3 & PA == 1)
sd22 <-  sd(sd2$socialself)
15.42/sqrt((sd11^2+sd22^2)/2)
#0.21 age 35-44


sd1 <- subset(mhm, age == 4 & PA == 0)
sd11 <-  sd(sd1$socialself)
sd2 <- subset(mhm, age == 4 & PA == 1)
sd22 <-  sd(sd2$socialself)
14.84/sqrt((sd11^2+sd22^2)/2)
#0.21 age 45-54

sd1 <- subset(mhm, age == 5 & PA == 0)
sd11 <-  sd(sd1$socialself)
sd2 <- subset(mhm, age == 5 & PA == 1)
sd22 <-  sd(sd2$socialself)
12.45/sqrt((sd11^2+sd22^2)/2)
#0.17 age 55-64


sd1 <- subset(mhm, age == 6 & PA == 0)
sd11 <-  sd(sd1$socialself)
sd2 <- subset(mhm, age == 6 & PA == 1)
sd22 <-  sd(sd2$socialself)
9.04/sqrt((sd11^2+sd22^2)/2)
#0.13 65-74



sd1 <- subset(mhm, age == 7 & PA == 0)
sd11 <-  sd(sd1$socialself)
sd2 <- subset(mhm, age == 7 & PA == 1)
sd22 <-  sd(sd2$socialself)
8.02/sqrt((sd11^2+sd22^2)/2)
#0.12 75-84



sd1 <- subset(mhm, age == 8 & PA == 0)
sd11 <-  sd(sd1$socialself)
sd2 <- subset(mhm, age == 8 & PA == 1)
sd22 <-  sd(sd2$socialself)
19.22/sqrt((sd11^2+sd22^2)/2)
#0.28  85+




# mind body ---------------------------------------------------------------







sd1 <- subset(mhm, age == 1 & PA == 0)
sd11 <-  sd(sd1$mindbody)
sd2 <- subset(mhm, age == 1 & PA == 1)
sd22 <-  sd(sd2$mindbody)
19.54/sqrt((sd11^2+sd22^2)/2)
#0.31 age 18-24


sd1 <- subset(mhm, age == 2 & PA == 0)
sd11 <-  sd(sd1$mindbody)
sd2 <- subset(mhm, age == 2 & PA == 1)
sd22 <-  sd(sd2$mindbody)
21.38/sqrt((sd11^2+sd22^2)/2)
#0.34 age 25-34


sd1 <- subset(mhm, age == 3 & PA == 0)
sd11 <-  sd(sd1$mindbody)
sd2 <- subset(mhm, age == 3 & PA == 1)
sd22 <-  sd(sd2$mindbody)
21.12/sqrt((sd11^2+sd22^2)/2)
#0.34 age 35-44


sd1 <- subset(mhm, age == 4 & PA == 0)
sd11 <-  sd(sd1$mindbody)
sd2 <- subset(mhm, age == 4 & PA == 1)
sd22 <-  sd(sd2$mindbody)
19.46/sqrt((sd11^2+sd22^2)/2)
#0.32 age 45-54

sd1 <- subset(mhm, age == 5 & PA == 0)
sd11 <-  sd(sd1$mindbody)
sd2 <- subset(mhm, age == 5 & PA == 1)
sd22 <-  sd(sd2$mindbody)
16.67/sqrt((sd11^2+sd22^2)/2)
#0.28 age 55-64


sd1 <- subset(mhm, age == 6 & PA == 0)
sd11 <-  sd(sd1$mindbody)
sd2 <- subset(mhm, age == 6 & PA == 1)
sd22 <-  sd(sd2$mindbody)
15.38/sqrt((sd11^2+sd22^2)/2)
#0.27 65-74



sd1 <- subset(mhm, age == 7 & PA == 0)
sd11 <-  sd(sd1$mindbody)
sd2 <- subset(mhm, age == 7 & PA == 1)
sd22 <-  sd(sd2$mindbody)
15.36/sqrt((sd11^2+sd22^2)/2)
#0.29 75-84



sd1 <- subset(mhm, age == 8 & PA == 0)
sd11 <-  sd(sd1$mindbody)
sd2 <- subset(mhm, age == 8 & PA == 1)
sd22 <-  sd(sd2$mindbody)
25.52/sqrt((sd11^2+sd22^2)/2)
#0.44  85+









