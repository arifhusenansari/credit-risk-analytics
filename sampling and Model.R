wdpath <- "E:\\Arif\\Work\\Data Science Course\\Upgrad PGDDS\\Domain Elective\\BFSI\\Capstone Project\\"
setwd(wdpath)

#============================= sampling the data ==============================
# there are several technique for sampling.
# oversampling
# undersampling
# synthetical sampling

#------ We will use all those technique and evaluate model based on differet technique. 

library("ROSE")
library("MASS")
library(carData)
library(car) #-- used for vif
library(caret)#-- used to confusionmatrix and trainControl
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)#-- for fancyRpartPlot
library(ModelMetrics)
library(DMwR)
demographic.credit.woe.data <- read.csv("Data\\demographic_credit_woe_data.csv", header = T, sep =',')
#-- Step1: spliting data in train and test.
set.seed(100)

s <- sample.split(demographic.credit.woe.data$status, 0.70)

train.demographic.credit.woe.data <- demographic.credit.woe.data[s,]
test.demographic.credit.woe.data <- demographic.credit.woe.data[!s,]

prop.table(table(train.demographic.credit.woe.data$status))


# #====================== Sampling using ROSE package =============================
# 
# train.demographic.credit.woe.data.rose <-  ROSE(status~., data = train.demographic.credit.woe.data,seed  =1,p=0.4)$data
# prop.table(table( train.demographic.credit.woe.data.rose$status))

# #====================== Sampling using SMOTE   =============================

train.demographic.credit.woe.data.rose <-  SMOTE(status~., data = train.demographic.credit.woe.data)
prop.table(table( train.demographic.credit.woe.data.rose$status))

#======================= Logistic Regression ================================

# 1. with logit

model.1 <- glm(status ~ . , data = train.demographic.credit.woe.data.rose, family = "binomial")
summary(model.1)

stepAIC(model.1)


# model after stepAIC

model.2 <- glm(formula = status ~ app.id + age + gender + marital.status + 
                 dependents + income + education + profession + months.in.residence + 
                 months.in.company + num.60.dpd.or.worse.6.months + num.30.dpd.or.worse.6.months + 
                 num.90.dpd.or.worse.12.months + num.30.dpd.or.worse.12.months + 
                 avg.cc.utilization.12.months + open.trades.in.12.months + 
                 open.pl.trades.in.6.months + open.pl.trades.in.12.months + 
                 cc.inquiries.6.months + cc.inquiries.12.months + open.home.loan + 
                 outstanding.balanace, family = "binomial", data = train.demographic.credit.woe.data.rose)
summary(model.2)
max(vif(model.2))
which.max(vif(model.2))

# maximun vif is 2.18, seems good. 
# remove: months.in.residence

model.3 <- glm(formula = status ~ app.id + age + gender + marital.status + 
                 dependents + income + education + profession  + 
                 months.in.company + num.60.dpd.or.worse.6.months + num.30.dpd.or.worse.6.months + 
                 num.90.dpd.or.worse.12.months + num.30.dpd.or.worse.12.months + 
                 avg.cc.utilization.12.months + open.trades.in.12.months + 
                 open.pl.trades.in.6.months + open.pl.trades.in.12.months + 
                 cc.inquiries.6.months + cc.inquiries.12.months + open.home.loan + 
                 outstanding.balanace, family = "binomial", data = train.demographic.credit.woe.data.rose)
summary(model.3)


# remove: app.id, marital.status, open.home.loan

model.4 <- glm(formula = status ~ age + gender +  
                 dependents + income + education + profession  + 
                 months.in.company + num.60.dpd.or.worse.6.months + num.30.dpd.or.worse.6.months + 
                 num.90.dpd.or.worse.12.months + num.30.dpd.or.worse.12.months + 
                 avg.cc.utilization.12.months + open.trades.in.12.months + 
                 open.pl.trades.in.6.months + open.pl.trades.in.12.months + 
                 cc.inquiries.6.months + cc.inquiries.12.months + 
                 outstanding.balanace, family = "binomial", data = train.demographic.credit.woe.data.rose)
summary(model.4)
vif(model.4)


# model.4 is the final model. 
final.lr <- model.4


#================================== Model testing and evaulation =============================
pred <- predict(final.lr, test.demographic.credit.woe.data, type = 'response')

max(pred)
min(pred)

# max prob = 0.68

# cutoff: 0.34

pred.class <- as.factor(ifelse(pred >=0.34,1,0))

caret::confusionMatrix(as.factor(test.demographic.credit.woe.data$status), pred.class,positive="1")


c <- seq(0.16,0.68,0.01)

out.all<- matrix(0,length(c),5,4)

for ( i in c(1:length(c))){
  
  predict.test.class <- as.integer(ifelse(pred >= c[i],1,0))
  confmat <- caret::confusionMatrix(as.factor(predict.test.class),as.factor(test.demographic.credit.woe.data$status),positive = "1")
  #cutoff
  cutoff <- c[i]
  #Accuracy
  
  #Sensitivity: true positive ratio
  sens <- confmat$byClass[1]
  spec <- confmat$byClass[2]
  acc <- confmat$overall[1]
  auc <- ModelMetrics::auc(test.demographic.credit.woe.data$status,predict.test.class)
  out.all[i,] <- t(as.matrix(c(cutoff,acc,sens,spec,auc)))
  
}



(out.all)

s <- seq (1,nrow(out.all),1)
plot(s, out.all[,2],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,s,s,cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,out.all[,3],col="darkgreen",lwd=2)
lines(s,out.all[,4],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Accuracy","Sensitivity","Specificity"))

# KS-Statistics 

InformationValue::ks_stat(actuals = test.demographic.credit.woe.data$status, 
                          predictedScores = pred)

InformationValue::ks_plot(actuals = test.demographic.credit.woe.data$status, 
                          predictedScores = pred)

# at 0.43
# Accuracy: 65%  
# Sensitivity: 63%
# Specificity: 65%
# AUC: 64%
# KS Statistics: 0.28

# ================================================================================================

# 2.  probit.

model.1 <- glm(status ~ . , data = train.demographic.credit.woe.data.rose, family = binomial(link = "probit"))
summary(model.1)

stepAIC(model.1, direction = "both")

# below is final model using stepAIC

model.2 <- glm(formula = status ~ app.id + age + gender + marital.status + 
                 dependents + income + education + profession + months.in.residence + 
                 months.in.company + num.60.dpd.or.worse.6.months + num.30.dpd.or.worse.6.months + 
                 num.90.dpd.or.worse.12.months + num.30.dpd.or.worse.12.months + 
                 avg.cc.utilization.12.months + open.trades.in.12.months + 
                 open.pl.trades.in.6.months + open.pl.trades.in.12.months + 
                 cc.inquiries.6.months + cc.inquiries.12.months + open.home.loan + 
                 outstanding.balanace, family = binomial(link = "probit"), 
               data = train.demographic.credit.woe.data.rose)
summary(model.2)
vif(model.2)
max(vif(model.2))
which.max(vif(model.2))

# vif is good.
# remove: marital.status, app.id

model.3 <- glm(formula = status ~ age + gender +  
                 dependents + income + education + profession + months.in.residence + 
                 months.in.company + num.60.dpd.or.worse.6.months + num.30.dpd.or.worse.6.months + 
                 num.90.dpd.or.worse.12.months + num.30.dpd.or.worse.12.months + 
                 avg.cc.utilization.12.months + open.trades.in.12.months + 
                 open.pl.trades.in.6.months + open.pl.trades.in.12.months + 
                 cc.inquiries.6.months + cc.inquiries.12.months + open.home.loan + 
                 outstanding.balanace, family = binomial(link = "probit"), 
               data = train.demographic.credit.woe.data.rose)
summary(model.3)

# remove: months.in.residence 

model.4 <- glm(formula = status ~ age + gender +  
                 dependents + income + education + profession +  
                 months.in.company + num.60.dpd.or.worse.6.months + num.30.dpd.or.worse.6.months + 
                 num.90.dpd.or.worse.12.months + num.30.dpd.or.worse.12.months + 
                 avg.cc.utilization.12.months + open.trades.in.12.months + 
                 open.pl.trades.in.6.months + open.pl.trades.in.12.months + 
                 cc.inquiries.6.months + cc.inquiries.12.months + open.home.loan + 
                 outstanding.balanace, family = binomial(link = "probit"), 
               data = train.demographic.credit.woe.data.rose)
summary(model.4)

# model.4 is having all the variable significant

final.lr.probit <- model.4



#================================== Model testing and evaulation =============================

pred <- predict(final.lr.probit, test.demographic.credit.woe.data, type = 'response')

max(pred)
min(pred)

# max prob = 0.6846
# min prob = 0.1440

c <- seq(0.15,0.68,0.01)

out.all<- matrix(0,length(c),5,4)

for ( i in c(1:length(c))){
  
  predict.test.class <- as.integer(ifelse(pred >= c[i],1,0))
  confmat <- caret::confusionMatrix(as.factor(predict.test.class),as.factor(test.demographic.credit.woe.data$status),positive = "1")
  #cutoff
  cutoff <- c[i]
  #Accuracy
  
  #Sensitivity: true positive ratio
  sens <- confmat$byClass[1]
  spec <- confmat$byClass[2]
  acc <- confmat$overall[1]
  auc <- ModelMetrics::auc(test.demographic.credit.woe.data$status,predict.test.class)
  out.all[i,] <- t(as.matrix(c(cutoff,acc,sens,spec,auc)))
  
}

out.all



s <- seq (1,nrow(out.all),1)
plot(s, out.all[,2],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,s,s,cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,out.all[,3],col="darkgreen",lwd=2)
lines(s,out.all[,4],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Accuracy","Sensitivity","Specificity"))



# KS-Statistics 

InformationValue::ks_stat(actuals = test.demographic.credit.woe.data$status, 
                          predictedScores = pred)

InformationValue::ks_plot(actuals = test.demographic.credit.woe.data$status, 
                          predictedScores = pred)
# at 0.43 

# Accuracy: 65%
# Sensitivity: 63%
# Specificity: 65%
# AUC: 64%
# KS: 0.28

# ================================================================================================

# 3.  cloglog

model.1 <- glm(status ~ . , data = train.demographic.credit.woe.data.rose, family = binomial(link = "cloglog"))
summary(model.1)

stepAIC(model.1, direction = "both")

# below is final model using stepAIC

model.2 <- glm(formula = status ~ app.id + age + gender + marital.status + 
                 dependents + income + education + profession + months.in.company + 
                 num.90.dpd.or.worse.6.months + num.60.dpd.or.worse.6.months + 
                 num.30.dpd.or.worse.6.months + num.90.dpd.or.worse.12.months + 
                 num.30.dpd.or.worse.12.months + avg.cc.utilization.12.months + 
                 open.trades.in.12.months + open.pl.trades.in.6.months + open.pl.trades.in.12.months + 
                 cc.inquiries.6.months + cc.inquiries.12.months + outstanding.balanace, 
               family = binomial(link = "cloglog"), data = train.demographic.credit.woe.data.rose)
summary(model.2)
vif(model.2)
max(vif(model.2))
which.max(vif(model.2))

# vif is good.
# remove: marital.status, app.id

model.3 <- glm(formula = status ~ age + gender +  
                 dependents + income + education + profession + months.in.residence + 
                 months.in.company + num.60.dpd.or.worse.6.months + num.30.dpd.or.worse.6.months + 
                 num.90.dpd.or.worse.12.months + num.30.dpd.or.worse.12.months + 
                 avg.cc.utilization.12.months + open.trades.in.12.months + 
                 open.pl.trades.in.6.months + open.pl.trades.in.12.months + 
                 cc.inquiries.6.months + cc.inquiries.12.months + open.home.loan + 
                 outstanding.balanace, family = binomial(link = "probit"), 
               data = train.demographic.credit.woe.data.rose)
summary(model.3)

# remove: months.in.residence 

model.4 <- glm(formula = status ~ age + gender +  
                 dependents + income + education + profession +  
                 months.in.company + num.60.dpd.or.worse.6.months + num.30.dpd.or.worse.6.months + 
                 num.90.dpd.or.worse.12.months + num.30.dpd.or.worse.12.months + 
                 avg.cc.utilization.12.months + open.trades.in.12.months + 
                 open.pl.trades.in.6.months + open.pl.trades.in.12.months + 
                 cc.inquiries.6.months + cc.inquiries.12.months + open.home.loan + 
                 outstanding.balanace, family = binomial(link = "probit"), 
               data = train.demographic.credit.woe.data.rose)
summary(model.4)

# model.4 is having all the variable significant

final.lr.cloglog <- model.4



#================================== Model testing and evaulation =============================

pred <- predict(final.lr.cloglog, test.demographic.credit.woe.data, type = 'response')

max(pred)
min(pred)

# max prob = 0.6846
# min prob = 0.1440

c <- seq(0.15,0.68,0.01)

out.all<- matrix(0,length(c),5,4)

for ( i in c(1:length(c))){
  
  predict.test.class <- as.integer(ifelse(pred >= c[i],1,0))
  confmat <- caret::confusionMatrix(as.factor(predict.test.class),as.factor(test.demographic.credit.woe.data$status),positive = "1")
  #cutoff
  cutoff <- c[i]
  #Accuracy
  
  #Sensitivity: true positive ratio
  sens <- confmat$byClass[1]
  spec <- confmat$byClass[2]
  acc <- confmat$overall[1]
  auc <- ModelMetrics::auc(test.demographic.credit.woe.data$status,predict.test.class)
  out.all[i,] <- t(as.matrix(c(cutoff,acc,sens,spec,auc)))
  
}

out.all



s <- seq (1,nrow(out.all),1)
plot(s, out.all[,2],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,s,s,cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,out.all[,3],col="darkgreen",lwd=2)
lines(s,out.all[,4],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Accuracy","Sensitivity","Specificity"))



# KS-Statistics 

InformationValue::ks_stat(actuals = test.demographic.credit.woe.data$status, 
                          predictedScores = pred)

InformationValue::ks_plot(actuals = test.demographic.credit.woe.data$status, 
                          predictedScores = pred)
# at 0.43 

# Accuracy: 65%
# Sensitivity: 63%
# Specificity: 65%
# AUC: 64%
# KS: 0.28




# ================================= Decision Tree ============================


train.demographic.credit.woe.data.rose$status <- as.factor(train.demographic.credit.woe.data.rose$status)
tree.control <-  trainControl(method = "cv", number = 5)
tree.grid = expand.grid(cp = seq(0, 0.002, 0.0001))


# to find best minsplit value for model in tree.
splitvalues <- seq(50,1800,100)

cpmatrix <- matrix(0,length(splitvalues),4)
for ( i in c(1:length(splitvalues))){
  
  tree.model <- train(status ~ .,
                      data = train.demographic.credit.woe.data.rose,
                      method = "rpart",
                      metric = "Accuracy",
                      trControl = tree.control,
                      tuneGrid = tree.grid,
                      control = rpart.control(minsplit = splitvalues[i],
                                              minbucket = 20))
  results <- tree.model$results
  bestcp<- as.numeric(tree.model$bestTune)
  bestaccuracy<-round(results[results$cp  ==bestcp,c("Accuracy")]*100,1)
  cpmatrix [i,] <- t(as.matrix(c(i,bestcp,bestaccuracy,splitvalues[i])))
  
}

cpmatrix

# at minsplit of 450, we getting accuracy of 68% at c=0.005

#==================== Treee Model =========================

model.tree.1 <- rpart(status~., data = train.demographic.credit.woe.data.rose, method = "class",
                      control = rpart.control(cp=0.005,minsplit = 450))

#==================== Model testing and evaluation =====================

final.tree <- model.tree.1
dim(test.demographic.credit.woe.data)
pred <- predict(final.tree, test.demographic.credit.woe.data, type = "prob")
pred <- (pred[,1])

min(pred)
max(pred)

c <- seq(0.30,0.78,0.01)
out.all<- matrix(0,length(c),5,4)

for ( i in c(1:length(c))){
  
  predict.test.class <- as.integer(ifelse(pred >= c[i],1,0))
  confmat <- caret::confusionMatrix(as.factor(predict.test.class),as.factor(test.demographic.credit.woe.data$status),positive = "1")
  #cutoff
  cutoff <- c[i]
  #Accuracy
  
  #Sensitivity: true positive ratio
  sens <- confmat$byClass[1]
  spec <- confmat$byClass[2]
  acc <- confmat$overall[1]
  auc <- ModelMetrics::auc(test.demographic.credit.woe.data$status,predict.test.class)
  out.all[i,] <- t(as.matrix(c(cutoff,acc,sens,spec,auc)))
  
}

out.all
s <- seq (1,nrow(out.all),1)
plot(s, out.all[,2],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,s,s,cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,out.all[,3],col="darkgreen",lwd=2)
lines(s,out.all[,4],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Accuracy","Sensitivity","Specificity"))

# Decision tree is not performing well.

#=================================== Random Forest ===========================================

library(randomForest)

str(train.demographic.credit.woe.data.rose$status)
mtry <- floor(sqrt(dim(train.demographic.credit.woe.data.rose)[2]))
model.rf.1 <- randomForest(status ~. , data = train.demographic.credit.woe.data.rose, proximity = F, do.trace = T, mtry = mtry, ntree=600)


pred <- predict(model.rf.1, test.demographic.credit.woe.data , type = "prob")[,2]

max(pred)
min(pred)

#======================================== Model evaluation ========================================

c <- seq(0.03,0.84,0.01)
out.all<- matrix(0,length(c),5,4)

for ( i in c(1:length(c))){
  
  predict.test.class <- as.integer(ifelse(pred >= c[i],1,0))
  confmat <- caret::confusionMatrix(as.factor(predict.test.class),as.factor(test.demographic.credit.woe.data$status),positive = "1")
  #cutoff
  cutoff <- c[i]
  #Accuracy
  
  #Sensitivity: true positive ratio
  sens <- confmat$byClass[1]
  spec <- confmat$byClass[2]
  acc <- confmat$overall[1]
  auc <- ModelMetrics::auc(test.demographic.credit.woe.data$status,predict.test.class)
  out.all[i,] <- t(as.matrix(c(cutoff,acc,sens,spec,auc)))
  
}




# at 0.17
# Accuracy: 0.64
# Sensitivity: 0.63
# Specificity: 0.64
# AUC: 0.6367


s <- seq (1,nrow(out.all),1)
plot(s, out.all[,2],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,s,s,cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,out.all[,3],col="darkgreen",lwd=2)
lines(s,out.all[,4],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Accuracy","Sensitivity","Specificity"))

out.all










# ==================== final model selection ============================
# After all permutaion and testing below is selected final model. 


# at 0.43
# Accuracy: 65%  
# Sensitivity: 63%
# Specificity: 65%
# AUC: 64%
# KS Statistics: 0.28

model.4 <- glm(formula = status ~ age + gender +  
                 dependents + income + education + profession  + 
                 months.in.company + num.60.dpd.or.worse.6.months + num.30.dpd.or.worse.6.months + 
                 num.90.dpd.or.worse.12.months + num.30.dpd.or.worse.12.months + 
                 avg.cc.utilization.12.months + open.trades.in.12.months + 
                 open.pl.trades.in.6.months + open.pl.trades.in.12.months + 
                 cc.inquiries.6.months + cc.inquiries.12.months + 
                 outstanding.balanace, family = "binomial", data = train.demographic.credit.woe.data.rose)
