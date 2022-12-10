
require("gbm")
library(MatchThem)
library(WeightIt)
library(mice)

load("mhm.RData")
load("imp_overall.RData")



################ GBM ################
weightit_overall_gbm <- weightit(PA ~   
                                   age
                                 + sex
                                 + genderdiff
                                 + education
                                 + employment
                                 + relationship
                                 + socialize
                                 + sleep
                                 + meddiagnosis
                                 + mhseeking
                                 + childtrauma
                                 + adulttrauma,
                                 mhm, 
                                 approach = 'within',  
                                 method = "gbm",    
                                 estimand = "ATE",
                                 trim.at = 0.99,
                                 distribution = "multinomial")




######### gbm on multiply imputed data

weightdat_overall_gbm <- weightthem(PA ~   
                                      age
                                    + sex
                                    + genderdiff
                                    + education
                                    + employment
                                    + relationship
                                    + socialize
                                    + sleep
                                    + meddiagnosis
                                    + mhseeking
                                    + childtrauma
                                    + adulttrauma,
                                    imp_overall, 
                                    approach = 'within',  
                                    method = "gbm",    
                                    estimand = "ATE",
                                    trim.at = 0.99)







#############
#############             IGNORE
#############
############# ordinal propensity score stratification





require("GPSCDF")

load("imp_overall.RData")

imp_overall_long <- complete(imp_overall, action = 'long', include = TRUE)





mhm$PA <- factor(mhm$PA, ordered = T)
levels(mhm$PA)


glm <- with(imp_overall, nnet::multinom(as.factor(PA)~ age + sex + genderdiff + education 
                     + employment + relationship + socialize + sleep
                     + meddiagnosis + mhseeking + childtrauma + adulttrauma))


# save(glm, file = "glm.RData")



probab <- round(predict(pool(glm), newdata=imp, type="probs"),digits=8)

gps <- cbind(probab[,1],probab[,2],1-probab[,1]-probab[,2])


fit <-GPSCDF(pscores=gps)

fit$ppar


























##########################
### Example: Create data example
N<- 100
set.seed(18201) # make sure data is repeatable
Sigma <- matrix(.2,4,4)
diag(Sigma) <- 1
data<-matrix(0, nrow=N, ncol=6,dimnames=list(c(1:N),
                                             c("Y","trt",paste("X",c(1:4),sep=""))))
data[,3:6]<-matrix(MASS::mvrnorm(N, mu=rep(0, 4), Sigma,
                                 empirical = FALSE) , nrow=N, ncol = 4)

                                 
dat<-as.data.frame(data)
#Create Treatment Variable
tlogits<-matrix(0,nrow=N,ncol=2)
tprobs<-matrix(0,nrow=N,ncol=3)
alphas<-c(0.25, 0.3)
strongbetas<-c(0.7, 0.4)
modbetas<-c(0.2, 0.3)
for(j in 1:2){
  tlogits[,j]<- alphas[j] + strongbetas[j]*dat$X1 + strongbetas[j]*dat$X2+
    modbetas[j]*dat$X3 + modbetas[j]*dat$X4
}
for(j in 1:2){
  tprobs[,j]<- exp(tlogits[,j])/(1 + exp(tlogits[,1]) + exp(tlogits[,2]))
  tprobs[,3]<- 1/(1 + exp(tlogits[,1]) + exp(tlogits[,2]))
}
set.seed(91187)
for(j in 1:N){
  data[j,2]<-sample(c(1:3),size=1,prob=tprobs[j,])
}
#Create Outcome Variable
ylogits<-matrix(0,nrow=N,ncol=1,dimnames=list(c(1:N),c("Logit(P(Y=1))")))
yprobs<-matrix(0,nrow=N,ncol=2,dimnames=list(c(1:N),c("P(Y=0)","P(Y=1)")))
for(j in 1:N){
  ylogits[j,1]<- -1.1 + 0.7*data[j,2] + 0.6*dat$X1[j] + 0.6*dat$X2[j] +
    0.4*dat$X3[j] + 0.4*dat$X4[j]
  yprobs[j,2]<- 1/(1+exp(-ylogits[j,1]))
  yprobs[j,1]<- 1-yprobs[j,2]
}
set.seed(91187)
for(j in 1:N){
  data[j,1]<-sample(c(0,1),size=1,prob=yprobs[j,])
}
dat<-as.data.frame(data)











glm<- nnet::multinom(as.factor(trt)~ X1+ X2+ X3+ X4, data=dat)
probab<- round(predict(glm, newdata=dat, type="probs"),digits=8)
gps<-cbind(probab[,1],probab[,2],1-probab[,1]-probab[,2])
#Create scalar balancing power parameter
fit<-GPSCDF(pscores=gps)

