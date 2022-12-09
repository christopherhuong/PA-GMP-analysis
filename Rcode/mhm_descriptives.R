
library(psych)






load("mhm.RData")



mhm$PA <- as.integer(mhm$PA)
mhm$country <- as.integer(mhm$country)


multi.hist(mhm[2:8],)


