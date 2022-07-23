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
                 stringsAsFactors = T)

# Count rows with missing values
row.has.na <- apply(dat1, 1, function(x){any(is.na(x))})
sum(row.has.na)
# Percent missing values by variable
percentmiss <- function(x){
  sum(is.na(x)) / length(x) * 100} 

apply(dat1, 2, percentmiss)


######## need to convert blank spaces to NA???



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





























