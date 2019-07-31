wdpath <- "E:\\Arif\\Work\\Data Science Course\\Upgrad PGDDS\\Domain Elective\\BFSI\\Capstone Project\\"
setwd(wdpath)

library(InformationValue)
library(ggplot2)
library(gridExtra)
library(MASS)
library(car)
library(caret)
library(ModelMetrics)
library(caTools)



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
model.1  <- glm(status~., data = train.demographic.woe.data, family = "binomial")
summary(model.1)



# Use stepAIC on model.1 to eleminate columns.
stepAIC(model.1 , direction = "both")

# After stepAIC. We have got below model.
# glm(formula = status ~ age + dependents + income + education + 
#       profession + months.in.residence + months.in.company, family = "binomial", 
#     data = train.demographic.woe.data)
# we will remove column based on significance and VIF values.

# model.2: vif is good for all the columns. We will exclude columns based on significance
# exclude: education (p-value = 0.11)

model.2 <- glm(formula = status ~ age + dependents + income + education + 
      profession + months.in.residence + months.in.company, family = "binomial", 
    data = train.demographic.woe.data)
summary(model.2)

vif(model.2)

# model.3: 
# exclude: age (p-value = 0.09)

model.3 <- glm(formula = status ~ age + dependents + income + 
                 profession + months.in.residence + months.in.company, family = "binomial", 
               data = train.demographic.woe.data)
summary(model.3)


# model.4: 


model.4 <- glm(formula = status ~ dependents + income + 
                 profession + months.in.residence + months.in.company, family = "binomial", 
               data = train.demographic.woe.data)
summary(model.4)


#------ We have come up with model.4 as final model, with all significant columns -------- 


final.model <- model.4

#------ Predicting test data. 

predict.test <- predict(final.model, test.demographic.woe.data, type = "response")

# max: 0.11
summary(predict.test)

#======================== Model evaluation ==========================

# 1) cutoff: 0.05%
# at 5% cutoff. 
#specificity is 41% and accuracy is 73%. 


predict.test.class <- as.integer(ifelse(predict.test >= 0.05,1,0))

confmat <- confusionMatrix(as.factor(predict.test.class),as.factor(test.demographic.woe.data$status),positive = "1")
confmat
auc <- auc(test.demographic.woe.data$status, predict.test.class)
auc

#------ Try with different cutoff level to check for specificity 

# since max prob is 0.11. we will check cutoff till 0.11 

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

# optimum model, using demographics data does not have a good performace.
# we are not able to find any good cutoff where we do have optimum value for all the parameter.
# We are interested to predict positive (default) with high accuracy.
# at cutoff (0.03) Sensitivity is good 88.46% but specificity is only 0.21. 
# Accuracy is very low around 24%.
# that means our model is wrognly identifying good customer to defaulter, hence we can loss potential customers.
# Also, number of event that is 1 (default) are very less. So for model if will be difficult to learn
# property of defaulters. 
# we can also use over sampling technique and build the model.
# For now, we will build the model using combine data of demographics and credit bureau.

# at cutoff: 0.04. All graphs intercept and also auc is maximum (0.58).

(out.demographic)

s <- seq (1,nrow(out.demographic),1)
plot(s, out.demographic[,2],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,s,s,cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,out.demographic[,3],col="darkgreen",lwd=2)
lines(s,out.demographic[,4],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Accuracy","Sensitivity","Specificity"))






#======================= BUild model using demographics and credit bureau table  =========================

#-- load the data

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
# as expected, lot's of columns are multi colinear.

model.1 <- glm(status ~ income+months.in.residence+months.in.company+num.90.dpd.or.worse.6.months+num.60.dpd.or.worse.6.months+num.30.dpd.or.worse.6.months+num.90.dpd.or.worse.12.months+num.60.dpd.or.worse.12.months+num.30.dpd.or.worse.12.months+
                 avg.cc.utilization.12.months+open.trades.in.6.months+open.trades.in.12.months+open.pl.trades.in.6.months+open.pl.trades.in.12.months+cc.inquiries.6.months+cc.inquiries.12.months+outstanding.balanace+total.trades , data = train.demographic.credit.woe.data,
               family = "binomial")
summary(model.1)


#-- run stepAIC and get the final model.
# final model, we get from stepAIC 
# we will exclude column based on VIF and significance.
# glm(formula = status ~ income + months.in.company + num.30.dpd.or.worse.6.months + 
#       num.30.dpd.or.worse.12.months + avg.cc.utilization.12.months + 
#       open.trades.in.12.months + cc.inquiries.12.months + outstanding.balanace + 
#       total.trades, family = "binomial", data = train.demographic.credit.woe.data)
stepAIC(model.1, direction = 'both')



model.2 <-   glm(formula = status ~ income + months.in.company + num.30.dpd.or.worse.6.months +
                   num.30.dpd.or.worse.12.months + avg.cc.utilization.12.months +
                   open.trades.in.12.months + cc.inquiries.12.months + outstanding.balanace +
                   total.trades, family = "binomial", data = train.demographic.credit.woe.data)
summary(model.2)
vif(model.2)


# remove: num.30.dpd.or.worse.12.months
model.3 <- glm(formula = status ~ income + months.in.company + num.30.dpd.or.worse.6.months +
                 avg.cc.utilization.12.months +
                 open.trades.in.12.months + cc.inquiries.12.months + outstanding.balanace +
                 total.trades, family = "binomial", data = train.demographic.credit.woe.data)
summary(model.3)
vif(model.3)

# remove: income
model.3 <- glm(formula = status ~  months.in.company + num.30.dpd.or.worse.6.months +
                 avg.cc.utilization.12.months +
                 open.trades.in.12.months + cc.inquiries.12.months + outstanding.balanace +
                 total.trades, family = "binomial", data = train.demographic.credit.woe.data)
summary(model.3)
vif(model.3)



# 1)  age: not significant
# there is not changes in Null deviance and Residual deviance.
# AIC is increased. 

model.4 <- glm(formula = status ~  months.in.company + num.30.dpd.or.worse.6.months +
                 avg.cc.utilization.12.months +
                 open.trades.in.12.months + cc.inquiries.12.months + outstanding.balanace +
                 total.trades+age, family = "binomial", data = train.demographic.credit.woe.data)
summary(model.4)


# 2)  remove age and add gender

# there is no significance improvement.
# Null deviance: 17100
# Residual deviance: 16299
# AIC: 16318.

model.5 <- glm(formula = status ~  months.in.company + num.30.dpd.or.worse.6.months +
                 avg.cc.utilization.12.months +
                 open.trades.in.12.months + cc.inquiries.12.months + outstanding.balanace +
                 total.trades+gender, family = "binomial", data = train.demographic.credit.woe.data)

model.3$null.deviance
model.3$deviance
model.3$aic

model.5$null.deviance
model.5$deviance
model.5$aic


# 3) remove gender and add marital.status

# all the parameter are increasing as also, p value is high


model.5 <- glm(formula = status ~  months.in.company + num.30.dpd.or.worse.6.months +
                 avg.cc.utilization.12.months +
                 open.trades.in.12.months + cc.inquiries.12.months + outstanding.balanace +
                 total.trades+marital.status, family = "binomial", data = train.demographic.credit.woe.data)

summary(model.5)
model.3$null.deviance
model.3$deviance
model.3$aic

model.5$null.deviance
model.5$deviance
model.5$aic




# 4) remove marital.status and add dependents
# AIC is decreased significantly and also, p-value is good.


model.6 <- glm(formula = status ~  months.in.company + num.30.dpd.or.worse.6.months +
                 avg.cc.utilization.12.months +
                 open.trades.in.12.months + cc.inquiries.12.months + outstanding.balanace +
                 total.trades+dependents, family = "binomial", data = train.demographic.credit.woe.data)



model.3$null.deviance
model.3$deviance
model.3$aic

model.6$null.deviance
model.6$deviance
model.6$aic
summary(model.6)

# 5)  add education: not significant

# no improvement in parameter



model.7 <- glm(formula = status ~  months.in.company + num.30.dpd.or.worse.6.months +
                 avg.cc.utilization.12.months +
                 open.trades.in.12.months + cc.inquiries.12.months + outstanding.balanace +
                 total.trades+dependents+education, family = "binomial", data = train.demographic.credit.woe.data)
summary(model.7)

model.6$null.deviance
model.6$deviance
model.6$aic

model.7$null.deviance
model.7$deviance
model.7$aic

# 6)  remove education and add profession: significant
# improvement in parameters and also this is significant and p value is good.

model.8 <- glm(formula = status ~  months.in.company + num.30.dpd.or.worse.6.months +
                 avg.cc.utilization.12.months +
                 open.trades.in.12.months + cc.inquiries.12.months + outstanding.balanace +
                 total.trades+dependents+profession, family = "binomial", data = train.demographic.credit.woe.data)
summary(model.8)

model.6$null.deviance
model.6$deviance
model.6$aic

model.8$null.deviance
model.8$deviance
model.8$aic
summary(model.8)
vif(model.8)

# 7)  add residence.type: not significant

# no improvement in parameters


model.9 <- glm(formula = status ~  months.in.company + num.30.dpd.or.worse.6.months +
                 avg.cc.utilization.12.months +
                 open.trades.in.12.months + cc.inquiries.12.months + outstanding.balanace +
                 total.trades+dependents+ profession+residence.type, family = "binomial", data = train.demographic.credit.woe.data)
summary(model.9)
vif(model.9)


model.8$null.deviance
model.8$deviance
model.8$aic


model.9$null.deviance
model.9$deviance
model.9$aic


# 8)  remove residence.type and add open.home.loan: not significant

# no improvement in parameter
# p-value: 0.83

model.10 <- glm(formula = status ~  months.in.company + num.30.dpd.or.worse.6.months +
                 avg.cc.utilization.12.months +
                 open.trades.in.12.months + cc.inquiries.12.months + outstanding.balanace +
                 total.trades+dependents+profession+open.home.loan, family = "binomial", data = train.demographic.credit.woe.data)
summary(model.10)
vif(model.10)


model.8$null.deviance
model.8$deviance
model.8$aic


model.10$null.deviance
model.10$deviance
model.10$aic


# 9)  remove open.home.loan and add open.auto.loan: not significant



model.11 <- glm(formula = status ~  months.in.company + num.30.dpd.or.worse.6.months +
                  avg.cc.utilization.12.months +
                  open.trades.in.12.months + cc.inquiries.12.months + outstanding.balanace +
                  total.trades+dependents+profession+open.auto.loan, family = "binomial", data = train.demographic.credit.woe.data)
summary(model.11)
vif(model.11)


model.8$null.deviance
model.8$deviance
model.8$aic


model.11$null.deviance
model.11$deviance
model.11$aic

# model.8 is the final model using combine data.

final.model.combine <- model.8
final.model.combine
summary(final.model.combine)
vif(final.model.combine)


#------ Predict values using test data set.

pred <-  predict(final.model.combine,test.demographic.credit.woe.data,type = "response")

summary(pred)

# confusion matrix using cutoff: 0.06
pred.test <- as.integer(ifelse(pred >= 0.06,1,0))

confmat <- caret::confusionMatrix(as.factor(pred.test),as.factor(test.demographic.credit.woe.data$status),
                                  positive="1")
auc <- auc(test.demographic.credit.woe.data$status,pred.test)
confmat$byClass
confmat$overall[1]
auc
# since max prob is 0.12. we will check cutoff till 0.11 

c <- seq(0.01,0.11,0.01)

out<- matrix(0,length(c),5)


for ( i in c(1:length(c))){
  pred.test <- as.integer(ifelse(pred >= c[i],1,0))
  conf <- caret::confusionMatrix(as.factor(pred.test),as.factor(test.demographic.credit.woe.data$status),positive = "1")
  #cutoff
  cutoff <- c[i]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  acc <- conf$overall[1]
  
  auc <- auc(test.demographic.credit.woe.data$status,pred.test)
  out[i,] <- t(as.matrix(c(cutoff,acc,sens,spec,auc)))
  
}




# conclusion: 

# logistic regression doesn't have good preformance on combine data as well. 

# we are not able to find any good cutoff where we do have optimum value for all the parameter.
# We are interested to predict positive (default) with high accuracy.
# at cutoff (0.02) Sensitivity is good 91.40% but specificity is only 0.29.
# but over all accuracy is 32% (approx).

# Number of event that is 1 (default) are very less. 
# So for model if will be difficult to learn. 
# our accuracy is almost same as specificity due the reason that 0 events are very high and biased.

# property of defaulters.
# at cutoff: 0.05. All graphs intercept.
# accuracy: 65.30%
# sensitivity: 60.40%
# specificity: 65.53%
# auc: 63% approx.


out

s <- seq (1,nrow(out),1)
plot(s, out[,2],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,s,s,cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,out[,3],col="darkgreen",lwd=2)
lines(s,out[,4],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Accuracy","Sensitivity","Specificity"))



