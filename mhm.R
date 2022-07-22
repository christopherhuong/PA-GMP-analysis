library(readr)
library(curl)
library(tidyverse)
library(dplyr)
library(psych)
library(corrplot)
library(ggplot2)
library(car)

dat1 <- read.csv("mhm_data_2022-07-21_19-46-52.csv", 
                 stringsAsFactors = T)


dat2 <- select(dat1, Adaptability.to.Change:Adult.traumas)

dat2 <- dat2[,-which(sapply(dat2, class) == "factor")]

cormax <- cor(dat2, use = "pairwise.complete.obs")
KMO(dat2)
cortest.bartlett(dat2)
det(cormax)
