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


0.20-0.35































