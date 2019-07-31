wdpath <- "E:\\Arif\\Work\\Data Science Course\\Upgrad PGDDS\\Domain Elective\\BFSI\\Capstone Project\\"
setwd(wdpath)

library(InformationValue)
library(ggplot2)
library(gridExtra)
library(MASS)
library(carData)
library(car)
library(caret)
library(ModelMetrics)
library(dummies)
library(caTools)
library(AUC)


#============================ Model Building Only demographics Data =========================

#======== Step: 1 

# Get data from iv file and only consider those columns where IV value between 0.02 to 0.5
# after getting final model, we can add those column to check whether they are improving model performance.

#-- Get IV Values

iv_values <- read.csv(file = paste(wdpath,"\\Data\\iv_values.csv",sep=""),header =T ,sep=',')

#-- filter column having IV between 0.02 and 0.5
columns <- iv_values[iv_values$IV >=0.02 & iv_values$IV <=0.5,c("Variable")]
columns



#=============================== BUild model using demographics ==============================
#-- load data from demographics.
demographic.woe.data <- read.csv(file = paste(wdpath,"Data\\demographic_woe_data.csv",sep=""),header = T, sep=",")
#-- get the column in the demographic file
data.column <- colnames(demographic.woe.data )
data.column

#-- significant columns from demographics based on iv.
#-- only three columns are important based on IV.
significant.column.demographic <- data.column [data.column %in% columns]
significant.column.demographic


#-- we will build model on all the columns of demographic.woe.data. 

str(demographic.woe.data)

#-- split data in testing and training.

set.seed(100)


train.demographic.woe.data <-  demographic.woe.data[sample.split(demographic.woe.data$status,SplitRatio =  0.70),]
test.demographic.woe.data <-  demographic.woe.data[!sample.split(demographic.woe.data$status,SplitRatio =  0.70),]



# model.1

model.1 <- glm(status~. , data = train.demographic.woe.data, family = binomial(link = cloglog))
summary(model.1)

# stepAIC

stepAIC(model.1, direction = "both")

# below model we got from stepAIC

# glm(formula = status ~ age + dependents + income + education + 
#       profession + months.in.residence + months.in.company, family = binomial(link = cloglog), 
#     data = train.demographic.woe.data)

model.2 <- glm(formula = status ~ age + dependents + income + education + 
                 profession + months.in.residence + months.in.company, family = binomial(link = cloglog), 
               data = train.demographic.woe.data)
summary(model.2)
summary(model.1)

vif(model.2)

# remove: education least significant

model.3 <- glm(formula = status ~ age + dependents + income + 
                 profession + months.in.residence + months.in.company, family = binomial(link = cloglog), 
               data = train.demographic.woe.data)
summary(model.3)

# all the variable are significant. model.3 is final model.

final.demographic <- model.3



#=============================== Model testing and evaluation ===============================

#------ Testing on test data ------

predict.test <- predict(final.demographic,test.demographic.woe.data,type = "response")
# max: 0.1162
summary(predict.test)

# set cutoff: 0.06

predict.test.class <- as.factor(ifelse(predict.test>=0.06,1,0))
summary(predict.test.class)

# check sensitivity, specificity and AUC

confmat <- caret::confusionMatrix(factor(test.demographic.woe.data$status),predict.test.class, positive="1")

auc <- ModelMetrics::auc(model.3)

confmat$overall[1]
confmat$byClass[1]
confmat$byClass[2]
auc

c <- seq(0.01,0.11,0.01)

out.demographic<- matrix(0,length(c),5,4)

for ( i in c(1:length(c))){
  
  predict.test.class <- as.integer(ifelse(predict.test >= c[i],1,0))
  confmat <- caret::confusionMatrix(as.factor(predict.test.class),as.factor(test.demographic.woe.data$status),positive = "1")
  #cutoff
  cutoff <- c[i]
  #Accuracy
  
  #Sensitivity: true positive ratio
  sens <- confmat$byClass[1]
  spec <- confmat$byClass[2]
  acc <- confmat$overall[1]
  auc <- auc(test.demographic.woe.data$status,predict.test.class)
  out.demographic[i,] <- t(as.matrix(c(cutoff,acc,sens,spec,auc)))
  
}

# conclusion: 

# at 0.04 cutoff all graphs are intercepts.
# as per below, matrix model is not that much good. 
# accuracy: 54.06%
# sensitivity: 63.24%
# specificity: 53.66%
# auc: 58.44%
out.demographic

s <- seq (1,nrow(out.demographic),1)
plot(s, out.demographic[,2],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,s,s,cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,out.demographic[,3],col="darkgreen",lwd=2)
lines(s,out.demographic[,4],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Accuracy","Sensitivity","Specificity"))


#======================= Model building using demographics and credit bureau data ========================


demographic.credit.woe.data <- read.csv("Data\\demographic_credit_woe_data.csv", header = T, sep =',')
#-- get the column in the  file
data.column <- colnames(demographic.credit.woe.data )
data.column

# significant columns from demographics and credit bureau based on iv.
# 18 columns are significant from combine data.
# We will build model only on those columns and after getting optimum model. Add other remaining columns one by one.
# to check whether they are making any impact.
significant.column.demographic.credit <- data.column [data.column %in% columns]
other.column.demographic.credit  <- data.column [!data.column %in% columns]
# z <- ''
# for ( i in significant.column.demographic.credit){
#   z <- paste (  z , i,sep = "+")
# }
# z

#============= Model builing =============

#-- Step1: spliting data in train and test.
set.seed(100)

s <- sample.split(demographic.credit.woe.data$status, 0.70)

train.demographic.credit.woe.data <- demographic.credit.woe.data[s,]
test.demographic.credit.woe.data <- demographic.credit.woe.data[!s,]


#-- Step2: train model on training data.


model.1 <- glm(status ~ income+months.in.residence+months.in.company+num.90.dpd.or.worse.6.months+num.60.dpd.or.worse.6.months+num.30.dpd.or.worse.6.months+num.90.dpd.or.worse.12.months+num.60.dpd.or.worse.12.months+num.30.dpd.or.worse.12.months+
                 avg.cc.utilization.12.months+open.trades.in.6.months+open.trades.in.12.months+open.pl.trades.in.6.months+open.pl.trades.in.12.months+cc.inquiries.6.months+cc.inquiries.12.months+outstanding.balanace+total.trades , data = train.demographic.credit.woe.data,
               family = binomial(cloglog))
summary(model.1)

# stepAIC
stepAIC(model.1 , direction = "both")


# final model after stepAIC

# glm(formula = status ~ months.in.company + num.30.dpd.or.worse.6.months + 
#       avg.cc.utilization.12.months + open.trades.in.12.months + 
#       cc.inquiries.12.months + outstanding.balanace, family = binomial(cloglog), 
#     data = train.demographic.credit.woe.data)

model.2 <- glm(formula = status ~ months.in.company + num.30.dpd.or.worse.6.months + 
                 avg.cc.utilization.12.months + open.trades.in.12.months + 
                 cc.inquiries.12.months + outstanding.balanace, family = binomial(cloglog), 
               data = train.demographic.credit.woe.data)
summary(model.2)

vif(model.2)


# remove: months.in.company least significant

model.3 <- glm(formula = status ~ num.30.dpd.or.worse.6.months + 
                 avg.cc.utilization.12.months + open.trades.in.12.months + 
                 cc.inquiries.12.months + outstanding.balanace, family = binomial(cloglog), 
               data = train.demographic.credit.woe.data)
summary(model.3)

# add age

model.4 <- glm(formula = status ~ num.30.dpd.or.worse.6.months + 
                 avg.cc.utilization.12.months + open.trades.in.12.months + 
                 cc.inquiries.12.months + outstanding.balanace + age, family = binomial(cloglog), 
               data = train.demographic.credit.woe.data)
summary(model.4)
model.4$aic
model.3$aic

# add: gender

model.5 <- glm(formula = status ~ num.30.dpd.or.worse.6.months + 
                 avg.cc.utilization.12.months + open.trades.in.12.months + 
                 cc.inquiries.12.months + outstanding.balanace + age+gender, family = binomial(cloglog), 
               data = train.demographic.credit.woe.data)
summary(model.5)

# remove: gender and add: marital.status

model.6 <- glm(formula = status ~ num.30.dpd.or.worse.6.months + 
                 avg.cc.utilization.12.months + open.trades.in.12.months + 
                 cc.inquiries.12.months + outstanding.balanace + age+marital.status, family = binomial(cloglog), 
               data = train.demographic.credit.woe.data)
summary(model.6)



# remove: marital.status and add: dependents

model.7 <- glm(formula = status ~ num.30.dpd.or.worse.6.months + 
                 avg.cc.utilization.12.months + open.trades.in.12.months + 
                 cc.inquiries.12.months + outstanding.balanace + age+dependents, family = binomial(cloglog), 
               data = train.demographic.credit.woe.data)
summary(model.7)


# remove: dependents and add: education (significant)
# final

model.8 <- glm(formula = status ~ num.30.dpd.or.worse.6.months + 
                 avg.cc.utilization.12.months + open.trades.in.12.months + 
                 cc.inquiries.12.months + outstanding.balanace + age+education, family = binomial(cloglog), 
               data = train.demographic.credit.woe.data)
summary(model.8)
model.8$aic
model.4$aic


# add: profession

model.9 <- glm(formula = status ~ num.30.dpd.or.worse.6.months + 
                 avg.cc.utilization.12.months + open.trades.in.12.months + 
                 cc.inquiries.12.months + outstanding.balanace + age+education+
                 profession, family = binomial(cloglog), 
               data = train.demographic.credit.woe.data)
summary(model.9)


# remove: profession and add: residence.type

model.10 <- glm(formula = status ~ num.30.dpd.or.worse.6.months + 
                 avg.cc.utilization.12.months + open.trades.in.12.months + 
                 cc.inquiries.12.months + outstanding.balanace + age+education+
                  residence.type, family = binomial(cloglog), 
               data = train.demographic.credit.woe.data)
summary(model.10)

# remove: residence.type and add: open.home.loan

model.11 <- glm(formula = status ~ num.30.dpd.or.worse.6.months + 
                  avg.cc.utilization.12.months + open.trades.in.12.months + 
                  cc.inquiries.12.months + outstanding.balanace + age+education+
                  open.home.loan, family = binomial(cloglog), 
                data = train.demographic.credit.woe.data)
summary(model.11)

# remove: open.home.loan and add: open.auto.loan

model.12 <- glm(formula = status ~ num.30.dpd.or.worse.6.months + 
                  avg.cc.utilization.12.months + open.trades.in.12.months + 
                  cc.inquiries.12.months + outstanding.balanace + age+education+
                  open.auto.loan, family = binomial(cloglog), 
                data = train.demographic.credit.woe.data)
summary(model.12)


# after all iteration we found model.8 is final model.

final.all <- model.8
summary(final.all)
vif(final.all)


#====================== Model testing and evaluation ===================

predict.test <- predict(final.all,test.demographic.credit.woe.data,type ="response")

summary(predict.test)
# max prob: 0.17776
#------ model performance evaluation ------
# set cutoff: 0.08
predict.test.class <- as.factor(ifelse(predict.test>=0.08,1,0))
confmat <- caret::confusionMatrix(as.factor(test.demographic.credit.woe.data$status),predict.test.class,positive="1")
auc <- auc(test.demographic.credit.woe.data$status,predict.test.class)
confmat$overall[1]
confmat$byClass[1]
confmat$byClass[2]
auc



#------ check for different cutoff value to get performance indicator at best cutoff -----------
c <- seq(0.01,0.17,0.01)

out.all<- matrix(0,length(c),5,4)

for ( i in c(1:length(c))){
  
  predict.test.class <- as.integer(ifelse(predict.test >= c[i],1,0))
  confmat <- caret::confusionMatrix(as.factor(predict.test.class),as.factor(test.demographic.credit.woe.data$status),positive = "1")
  #cutoff
  cutoff <- c[i]
  #Accuracy
  
  #Sensitivity: true positive ratio
  sens <- confmat$byClass[1]
  spec <- confmat$byClass[2]
  acc <- confmat$overall[1]
  auc <- 0
  auc <-  ModelMetrics::auc(test.demographic.credit.woe.data$status,predict.test.class)
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

# conclusion: 
# there is no improvement compare to logistic regression.
# at cutoff: 0.05. All graphs intercept.
# accuracy: 64.83%
# sensitivity: 59.16%
# specificity: 65.08%
# auc: 62% approx.

