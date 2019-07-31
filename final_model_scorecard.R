wdpath <- "E:\\Arif\\Work\\Data Science Course\\Upgrad PGDDS\\Domain Elective\\BFSI\\Capstone Project\\"
setwd(wdpath)

getwoevalue <- function (value,woedata,variableclass){
  if ( variableclass != "factor"){
    for ( i in woedata[,c(1)]){
      if ( i != 'NA'){
        from <- (as.integer(str_sub(i,2,gregexpr(pattern = ",",i)[[1]][1]-1 )))
        to <- as.integer( str_sub(i,gregexpr(pattern = ",",i)[[1]][1]+1,gregexpr(pattern = "]","i")[[1]][1]-1 ))
        
        if (value %in% seq(from,to)){
          
          return(woedata[,c("WOE")][which(woedata==i)[1]])       
          break
        }
      }else {
        if (is.na(value)){
          return(woedata[,c("WOE")][which(woedata=='NA')[1]])
          break
        }
      }
    }
  }else if ( variableclass == "factor"){
    for ( i in woedata[,c(1)]){
      if ( !is.na(i) && value != "" ) {
        
        if (value == i){
          return(woedata[,c("WOE")][which(woedata==i)[1]])
          break
        }
        
      }else {
        if (is.na(value) || value == "" ){
          return(woedata[,c("WOE")][which(is.na(woedata))[1]])
          break
        }
      }
    }
  }
  
}




#============================= sampling the data ==============================
# there are several technique for sampling.
# oversampling
# undersampling
# synthetical sampling

#------ We will use all those technique and evaluate model based on differet technique. 

library("ROSE")
library("MASS")
library(carData)
library(caret)#-- used to confusionmatrix and trainControl
library(caTools)
library(scorecard)
library(ggplot2)
library(gridExtra)
library(Information)
library(dplyr)
library(tidyr)
library(stringr)

demographic.credit.woe.data <- read.csv("Data\\demographic_credit_woe_data.csv", header = T, sep =',')
#-- Step1: spliting data in train and test.
set.seed(100)

s <- sample.split(demographic.credit.woe.data$status, 0.70)

train.demographic.credit.woe.data <- demographic.credit.woe.data[s,]
test.demographic.credit.woe.data <- demographic.credit.woe.data[!s,]

prop.table(table(train.demographic.credit.woe.data$status))


#====================== Sampling using ROSE package =============================

train.demographic.credit.woe.data.rose <-  ROSE(status~., data = train.demographic.credit.woe.data,seed  =1,p=0.4)$data
prop.table(table( train.demographic.credit.woe.data.rose$status))


#===================== We have already selected final model ==================================
# We will load final model again on the data and build the score card.

final <- glm(formula = status ~ age + gender +  
               dependents + income + education + profession  + 
               months.in.company + num.60.dpd.or.worse.6.months + num.30.dpd.or.worse.6.months + 
               num.90.dpd.or.worse.12.months + num.30.dpd.or.worse.12.months + 
               avg.cc.utilization.12.months + open.trades.in.12.months + 
               open.pl.trades.in.6.months + open.pl.trades.in.12.months + 
               cc.inquiries.6.months + cc.inquiries.12.months + 
               outstanding.balanace, family = "binomial", data = train.demographic.credit.woe.data.rose)
summary(final)

pred <- predict(object = final,test.demographic.credit.woe.data, type = "response")
test.demographic.credit.woe.data$pred <- pred
test.demographic.credit.woe.data$logodd <- log(pred/(1-pred))


# gainandlift <- function(pred,binwidthinpercentage, cutoff){

df <- test.demographic.credit.woe.data[order(test.demographic.credit.woe.data$pred,decreasing = T),c("pred","status")]
binwidthinpercentage <- 10
length(df$pred)
numberofbins <- ceiling(length(df$pred)/(length(df$pred)*(binwidthinpercentage/100)))
rowsinonebin <- (length(df$pred)*(binwidthinpercentage/100))
grp.num <- ntile(df$pred*-1,numberofbins)
df$bucket <- grp.num
str(df)

group_by(df,bucket) %>% summarise(d=sum(status))
gaintable <- as.data.frame(df %>% group_by(bucket)  %>%
  summarise_at(vars(status ), funs(total = n(),
                                   totaldefault=sum(., na.rm = TRUE))) %>%
  mutate(Cumresp = cumsum(totaldefault),
         Gain=Cumresp/sum(totaldefault)*100,
         Cumlift=Gain/(bucket*(100/numberofbins))))

gaintable
gaintable$randomedef <- sum(df$status)/numberofbins
write.csv(file = paste(wdpath,"gaintable.csv",sep= ""),x = gaintable)



# Lift : is %ofreponsescoveredbymodelinthatgroup/10%  ( here 10% is static for randome model.)
# Gain: cumulativereponsetillthatgroup/totalresponse
# Lift and gain does not have anything to deal with cutoff. Since we order data based on the prob of predicted model. 
# if model is good enough to predict reponses, we will get more gain in top 4 deciles. Because most of our reponses will fall in that decile


# Plot gain and lift table
gaintable
gain <- ggplot(data = gaintable, aes(x = bucket,y=Gain))+geom_line()+
  scale_x_continuous("bucket",labels =  as.character(gaintable$bucket),breaks = gaintable$bucket)+
  geom_text(size = 4, vjust = -0.5,label=round(gaintable$Gain,1) )+ggtitle("Gain Chart Compared to Randome Model")
gain
lift <- ggplot(data = gaintable, aes(x = bucket,y=Cumlift))+geom_line()+
  scale_x_continuous("bucket",labels =  as.character(gaintable$bucket),breaks = gaintable$bucket)+
  geom_text(size = 4, vjust = -0.5,label=round(gaintable$Cumlift,2) )+ggtitle("Lift Chart Compared to Randome Model")
lift

grid.arrange(nrow=1,ncol=2,gain,lift)


#================================= Build a Score Card =============================
# below score card is build using only prob of test data.

basepoint <- 400
odd <- 10
pdo <- 20



factor <- pdo/log(2)
offset <- basepoint - (factor * log(odd))

offset
factor

test.demographic.credit.woe.data$score <- offset -  (factor * test.demographic.credit.woe.data$logodd)
minscore <- floor(min(test.demographic.credit.woe.data$score))
maxscore <- ceiling(max(test.demographic.credit.woe.data$score))
minscore
maxscore

# min score: 311
# max score: 384

# prob at min and max score.
head(test.demographic.credit.woe.data[order(test.demographic.credit.woe.data$score,decreasing = F),c("pred","score")],1)
head(test.demographic.credit.woe.data[order(test.demographic.credit.woe.data$score,decreasing = T),c("pred","score")],1)
# default cutoff score at 0.43 (cutoff Probability)
head(test.demographic.credit.woe.data[round(test.demographic.credit.woe.data$pred,2)==0.43,c("pred","score")],1)

# score cutoff will be decided based on prob cutoff which is 0.43.
cutffscore <- offset -  (factor * (log(0.43 / ( 1 - 0.43))))
cutffscore
# cutoffscore is: 341


#================================ Impact of Model ===================================

# load woe data for all applications to calculate prediction and score based on that.
demographic.credit.woe.data <- read.csv("Data\\demographic_credit_woe_data.csv", header = T, sep =',')
#load actual data for other analysis
demographic.credit <- read.csv("Data\\demographic_credit.csv", header = T, sep =',')


all.pred <- predict(final, demographic.credit.woe.data, type = "response")
all.odds <- (all.pred/(1-all.pred))
all.logodds <- log(all.odds)
all.score <- offset - (factor * all.logodds)
min(all.score)
max(all.score)
#min score: 308
#max score: 384

demographic.credit.woe.data$pred <- all.pred
demographic.credit.woe.data$score <- all.score


# meage scores calculated with origional dataset.
dim(demographic.credit)
str(demographic.credit)
temp <- merge(demographic.credit,demographic.credit.woe.data,by.x = "app.id",by.y = "app.id")
# extract column only from origional dataset and scores from woe data set.
dim(temp)
str(temp)
temp[,59]
colnames(temp)
demographic.credit.scorecard <- temp[,c(1:29)]
demographic.credit.scorecard$pred <- temp$pred
demographic.credit.scorecard$score <- round(temp$score,0)
str(demographic.credit.scorecard)
colnames(demographic.credit.scorecard) <- c(colnames(demographic.credit),"pred","scores")
str(demographic.credit.scorecard)


#-------------------------------- Analysis Based on Default cut off score (342) ---------------------
# Credit loss in Random model. 

# Current Loss Credit Loss: 3,704,984,230
# Current Revenue Loss ( by not approving application): 0
sum(demographic.credit.scorecard[demographic.credit.scorecard$status==1,c("outstanding.balanace")],na.rm = T)

# Calculate Credit loss and Revenue loss after implementing model.

sum(demographic.credit.scorecard$outstanding.balanace,na.rm = T)
# Total Outstanding: 87,228,315,981

# credit loss Calculation based on default after application approval based on score.
sum(demographic.credit.scorecard[demographic.credit.scorecard$status==1 &
                                   demographic.credit.scorecard$scores >= 342
                                   ,c("outstanding.balanace")],na.rm = T)
# Credit loss: 1,476,485,921
# Credit loss improvement: old credit loss - new credit loss = 2,228,498,309

# Revenue loss Calculation: By not approving application of good customer due to low score.

sum(demographic.credit.scorecard[demographic.credit.scorecard$status==0 &
                                   demographic.credit.scorecard$scores < 342
                                 ,c("outstanding.balanace")],na.rm = T)

# Revenue loss: 28,608,708,471


# Approval Rate
nrow(demographic.credit.scorecard)
round((sum(demographic.credit.scorecard$scores>=342)*100)/nrow(demographic.credit.scorecard),0)

# Approval Rate: 64%
# Total Improvement in Credit Loss is: -2,228,498,309
# Total Reneue loss by not approving Application: -28,608,708,471
# With the approval rate of 64%, we have credit loss decrease by 2,228,498,309.
# But also, we are getting big loss in revenue by rejecting good customet, this loss is around: 28,608,708,471

#-------------------- Credit Loss improvement and Revenue loss Trade off for different Approval Rate ---------

# get cutoff score for different approval rate.
# 
# get.score.at.percentage <- function(data){
#   v <- vector("integer", 100)
#   total.application <- nrow(data)
#   percentage <- seq(1,100,1)
#   minscore <- min(data$scores)
#   maxscore <- max(data$scores)
#   scores <- seq(minscore,maxscore,by=1)
#   for ( p in percentage){
#     
#     for (s in scores){
#       if (p == round((sum(data$scores>=s)*100)/total.application,0)){
#         v[p]<- s
#         break;
#       }
#         
# 
#     }
#     
#   }
#   return(v)
# }

# get.score.at.percentage(demographic.credit.scorecard)

find.trade.off <- function (data){
  trade.off <- data.frame(seq=as.integer()
                          ,prob = as.integer()
                          ,score= as.integer()
                          ,approval.rate= as.integer()
                          ,credit.loss = as.integer()
                          ,revenue.loss = as.integer()
                          ,gain = as.integer()
                          ,rev.loss.per = as.integer()
                          ,gain.per = as.integer()
  )
  seq <- 0
  minscore <- min(data$scores)
  maxscore <- max(data$scores)
  scores <- seq(minscore,maxscore,by=1)
  total.application <- nrow(data)
  total.creditloss <- sum(data[data$status==1,c("outstanding.balanace")],na.rm = T)
  tota.rev <- sum(data[data$status==0,c("outstanding.balanace")],na.rm = T)
  for (s in scores){
    # s <- 352
    seq <- seq+1
    # approved.app <- sum(data$scores>=s)
    # rejected.app <- total.application-approved.app
    approval.rate <- round((sum(data$scores>=s)*100)/total.application,6)
    c.loss <- sum(data[data$status==1 & data$scores >= s,c("outstanding.balanace")],na.rm = T)
    r.loss <- sum(data[data$status==0 & data$scores < s,c("outstanding.balanace")],na.rm = T)
    gain <- total.creditloss - c.loss
    rev.loss.per <- round(r.loss*100/tota.rev,2)
    gain.per <- round(gain*100/total.creditloss,2)
    
    
    
    
    # r <- rbind(c(seq,s,approval.rate,
    #              formatC(c.loss,format="f", big.mark=",", digits=0)
    #              ,formatC(r.loss,format="f", big.mark=",", digits=0)
    #              ,formatC(gain,format="f", big.mark=",", digits=0)))
    r <- rbind(c(seq
                  ,s
                  ,approval.rate
                  ,c.loss
                  ,r.loss
                  ,gain
                  ,rev.loss.per
                  ,gain.per))
    
    colnames(r) <-  c("seq","score","approval.rate",
                      "credit.loss","revenue.loss","gain","rev.loss.per","gain.per")

    trade.off <- rbind(trade.off,r)


  }
  return(trade.off)
}

d <- find.trade.off(demographic.credit.scorecard)

# save the trade off result.
write.csv(file = paste(wdpath,"tradeoff.csv",sep= ""),x = d)

ggplot(d,aes(x=approval.rate))+
  geom_line(aes(y=rev.loss.per),color ="red")+
geom_line(aes(y=gain.per),color ="blue")+ylab("Percentage")+xlab("Approval Rate")

# Conclusion
# when we reject and accept the application using model. We will decrease our credit loss at the same 
# time we are also having revenue loss due to not giving loan to potential customers.
# looking at trade off data, at arppoval rate of 80%. we are lossing 17.4% of our revenue.
# Important point is that that 14% revenue loss is for current scenario, some of them might convert to credit losss.
# At the same point we are saving 35.1% or credit that is lost. Which is clear profilt. 
# Bank should taret customer at 80% of approval rate.

# Credit loss: 2,405,027,806
# Improvement : 1,215,030,661
# Revenue loss: 14,514,966,093


# -------------------------------- Rejected Application Analysis -----------------------------------


# load rejected application
rejected.applications <- read.csv("Data\\demographics_credit_na.csv", header = T, sep =',')
# we need data to generate woe value range to convert rejected application data to woe.
demographic.credit <- read.csv("Data\\demographic_credit.csv", header = T, sep =',')

colnames(rejected.applications)
rejected.applications$status <- as.factor(rejected.applications$status.y)
rejected.applications <- rejected.applications[,-c(12,30)]
colnames(rejected.applications)
dim(rejected.applications)

# rejection rate.
# Total Application: 71295
# rejected Application: 1425
# Rejection Rate: 1425/71295 = 1.99 ~ 2%



# convert unseen data to woe values for prediction.

rejected.applications.woe.data <- rejected.applications
colnames(rejected.applications.woe.data)
str(rejected.applications.woe.data)

iv_value <- create_infotables(demographic.credit,y = "status",bin =10)

df.iv.avg.cc.utilization.12.months <- iv_value$Tables$avg.cc.utilization.12.months
df.iv.avg.cc.utilization.12.months$seq <- c(1:nrow(df.iv.avg.cc.utilization.12.months))
woe_data <- sapply(rejected.applications.woe.data[,c("avg.cc.utilization.12.months")],getwoevalue,woedata=df.iv.avg.cc.utilization.12.months,"numeric")
rejected.applications.woe.data$avg.cc.utilization.12.months <- woe_data
min(rejected.applications.woe.data$avg.cc.utilization.12.months)
max(rejected.applications.woe.data$avg.cc.utilization.12.months)

df.iv.open.trades.in.12.months <- iv_value$Tables$open.trades.in.12.months
df.iv.open.trades.in.12.months$seq <- c(1:nrow(df.iv.open.trades.in.12.months))
woe_data <- sapply(rejected.applications.woe.data[,c("open.trades.in.12.months")],getwoevalue,woedata=df.iv.open.trades.in.12.months,"numeric")
rejected.applications.woe.data$open.trades.in.12.months <- woe_data
min(rejected.applications.woe.data$open.trades.in.12.months)
max(rejected.applications.woe.data$open.trades.in.12.months)


df.iv.open.pl.trades.in.12.months <- iv_value$Tables$open.pl.trades.in.12.months
df.iv.open.pl.trades.in.12.months$seq <- c(1:nrow(df.iv.open.pl.trades.in.12.months))
woe_data <- sapply(rejected.applications.woe.data[,c("open.pl.trades.in.12.months")],getwoevalue,woedata=df.iv.open.pl.trades.in.12.months,"numeric")
rejected.applications.woe.data$open.pl.trades.in.12.months <- woe_data
min(rejected.applications.woe.data$open.pl.trades.in.12.months)
max(rejected.applications.woe.data$open.pl.trades.in.12.months)

df.iv.cc.inquiries.12.months <- iv_value$Tables$cc.inquiries.12.months
df.iv.cc.inquiries.12.months$seq <- c(1:nrow(df.iv.cc.inquiries.12.months))
woe_data <- sapply(rejected.applications.woe.data[,c("cc.inquiries.12.months")],getwoevalue,woedata=df.iv.cc.inquiries.12.months,"numeric")
rejected.applications.woe.data$cc.inquiries.12.months <- woe_data
min(rejected.applications.woe.data$cc.inquiries.12.months)
max(rejected.applications.woe.data$cc.inquiries.12.months)

df.iv.outstanding.balanace <- iv_value$Tables$outstanding.balanace
df.iv.outstanding.balanace$seq <- c(1:nrow(df.iv.outstanding.balanace))
woe_data <- sapply(rejected.applications.woe.data[,c("outstanding.balanace")],getwoevalue,woedata=df.iv.outstanding.balanace,"numeric")
rejected.applications.woe.data$outstanding.balanace <- woe_data
min(rejected.applications.woe.data$outstanding.balanace)
max(rejected.applications.woe.data$outstanding.balanace)

df.iv.num.30.dpd.or.worse.6.months <- iv_value$Tables$num.30.dpd.or.worse.6.months
df.iv.num.30.dpd.or.worse.6.months$seq <- c(1:nrow(df.iv.num.30.dpd.or.worse.6.months))
woe_data <- sapply(rejected.applications.woe.data[,c("num.30.dpd.or.worse.6.months")],getwoevalue,woedata=df.iv.num.30.dpd.or.worse.6.months,"numeric")
rejected.applications.woe.data$num.30.dpd.or.worse.6.months <- woe_data
min(rejected.applications.woe.data$num.30.dpd.or.worse.6.months)
max(rejected.applications.woe.data$num.30.dpd.or.worse.6.months)

df.iv.total.trades <- iv_value$Tables$total.trades
df.iv.total.trades$seq <- c(1:nrow(df.iv.total.trades))
woe_data <- sapply(rejected.applications.woe.data[,c("total.trades")],getwoevalue,woedata=df.iv.total.trades,"numeric")
rejected.applications.woe.data$total.trades <- woe_data
min(rejected.applications.woe.data$total.trades)
max(rejected.applications.woe.data$total.trades)


df.iv.open.pl.trades.in.6.months <- iv_value$Tables$open.pl.trades.in.6.months
df.iv.open.pl.trades.in.6.months$seq <- c(1:nrow(df.iv.open.pl.trades.in.6.months))
woe_data <- sapply(rejected.applications.woe.data[,c("open.pl.trades.in.6.months")],getwoevalue,woedata=df.iv.open.pl.trades.in.6.months,"numeric")
rejected.applications.woe.data$open.pl.trades.in.6.months <- woe_data
min(rejected.applications.woe.data$open.pl.trades.in.6.months)
max(rejected.applications.woe.data$open.pl.trades.in.6.months)

df.iv.num.90.dpd.or.worse.12.months <- iv_value$Tables$num.90.dpd.or.worse.12.months
df.iv.num.90.dpd.or.worse.12.months$seq <- c(1:nrow(df.iv.num.90.dpd.or.worse.12.months))
df.iv.num.90.dpd.or.worse.12.months
woe_data <- sapply(rejected.applications.woe.data[,c("num.90.dpd.or.worse.12.months")],getwoevalue,woedata=df.iv.num.90.dpd.or.worse.12.months,"numeric")
rejected.applications.woe.data$num.90.dpd.or.worse.12.months <- woe_data
min(rejected.applications.woe.data$num.90.dpd.or.worse.12.months)
max(rejected.applications.woe.data$num.90.dpd.or.worse.12.months)

df.iv.num.60.dpd.or.worse.6.months <- iv_value$Tables$num.60.dpd.or.worse.6.months
df.iv.num.60.dpd.or.worse.6.months$seq <- c(1:nrow(df.iv.num.60.dpd.or.worse.6.months))
woe_data <- sapply(rejected.applications.woe.data[,c("num.60.dpd.or.worse.6.months")],getwoevalue,woedata=df.iv.num.60.dpd.or.worse.6.months,"numeric")
rejected.applications.woe.data$num.60.dpd.or.worse.6.months <- woe_data
min(rejected.applications.woe.data$num.60.dpd.or.worse.6.months)
max(rejected.applications.woe.data$num.60.dpd.or.worse.6.months)

df.iv.cc.inquiries.6.months <- iv_value$Tables$cc.inquiries.6.months
df.iv.cc.inquiries.6.months$seq <- c(1:nrow(df.iv.cc.inquiries.6.months))
woe_data <- sapply(rejected.applications.woe.data[,c("cc.inquiries.6.months")],getwoevalue,woedata=df.iv.cc.inquiries.6.months,"numeric")
rejected.applications.woe.data$cc.inquiries.6.months <- woe_data
min(rejected.applications.woe.data$cc.inquiries.6.months)
max(rejected.applications.woe.data$cc.inquiries.6.months)

df.iv.num.30.dpd.or.worse.12.months <- iv_value$Tables$num.30.dpd.or.worse.12.months
df.iv.num.30.dpd.or.worse.12.months$seq <- c(1:nrow(df.iv.num.30.dpd.or.worse.12.months))
woe_data <- sapply(rejected.applications.woe.data[,c("num.30.dpd.or.worse.12.months")],getwoevalue,woedata=df.iv.num.30.dpd.or.worse.12.months,"numeric")
rejected.applications.woe.data$num.30.dpd.or.worse.12.months <- woe_data
min(rejected.applications.woe.data$num.30.dpd.or.worse.12.months)
max(rejected.applications.woe.data$num.30.dpd.or.worse.12.months)

df.iv.open.trades.in.6.months <- iv_value$Tables$open.trades.in.6.months
df.iv.open.trades.in.6.months$seq <- c(1:nrow(df.iv.open.trades.in.6.months))
woe_data <- sapply(rejected.applications.woe.data[,c("open.trades.in.6.months")],getwoevalue,woedata=df.iv.open.trades.in.6.months,"numeric")
rejected.applications.woe.data$open.trades.in.6.months <- woe_data
min(rejected.applications.woe.data$open.trades.in.6.months)
max(rejected.applications.woe.data$open.trades.in.6.months)

df.iv.num.60.dpd.or.worse.12.months <- iv_value$Tables$num.60.dpd.or.worse.12.months
df.iv.num.60.dpd.or.worse.12.months$seq <- c(1:nrow(df.iv.num.60.dpd.or.worse.12.months))
woe_data <- sapply(rejected.applications.woe.data[,c("num.60.dpd.or.worse.12.months")],getwoevalue,woedata=df.iv.num.60.dpd.or.worse.12.months,"numeric")
rejected.applications.woe.data$num.60.dpd.or.worse.12.months <- woe_data
min(rejected.applications.woe.data$num.60.dpd.or.worse.12.months)
max(rejected.applications.woe.data$num.60.dpd.or.worse.12.months)

df.iv.num.90.dpd.or.worse.6.months <- iv_value$Tables$num.90.dpd.or.worse.6.months
df.iv.num.90.dpd.or.worse.6.months$seq <- c(1:nrow(df.iv.num.90.dpd.or.worse.6.months))
woe_data <- sapply(rejected.applications.woe.data[,c("num.90.dpd.or.worse.6.months")],getwoevalue,woedata=df.iv.num.90.dpd.or.worse.6.months,"numeric")
rejected.applications.woe.data$num.90.dpd.or.worse.6.months <- woe_data
min(rejected.applications.woe.data$num.90.dpd.or.worse.6.months)
max(rejected.applications.woe.data$num.90.dpd.or.worse.6.months)

df.iv.months.in.residence <- iv_value$Tables$months.in.residence
df.iv.months.in.residence$seq <- c(1:nrow(df.iv.months.in.residence))
woe_data <- sapply(rejected.applications.woe.data[,c("months.in.residence")],getwoevalue,woedata=df.iv.months.in.residence,"numeric")
rejected.applications.woe.data$months.in.residence <- woe_data
min(rejected.applications.woe.data$months.in.residence)
max(rejected.applications.woe.data$months.in.residence)

df.iv.income <- iv_value$Tables$income
df.iv.income$seq <- c(1:nrow(df.iv.income))
woe_data <- sapply(as.integer(rejected.applications.woe.data[,c("income")]),getwoevalue,woedata=df.iv.income,"numeric")
rejected.applications.woe.data$income <- woe_data
min(rejected.applications.woe.data$income)
max(rejected.applications.woe.data$income)

df.iv.months.in.company <- iv_value$Tables$months.in.company
df.iv.months.in.company$seq <- c(1:nrow(df.iv.months.in.company))
woe_data <- sapply(rejected.applications.woe.data[,c("months.in.company")],getwoevalue,woedata=df.iv.months.in.company,"numeric")
rejected.applications.woe.data$months.in.company <- woe_data
min(rejected.applications.woe.data$months.in.company)
max(rejected.applications.woe.data$months.in.company)


df.iv.age <- iv_value$Tables$age
df.iv.age$seq <- c(1:nrow(df.iv.age))
woe_data <- sapply(rejected.applications.woe.data[,c("age")],getwoevalue,woedata=df.iv.age,"numeric")
rejected.applications.woe.data$age <- woe_data
min(rejected.applications.woe.data$age)
max(rejected.applications.woe.data$age)

df.iv.dependents <- iv_value$Tables$dependents
df.iv.dependents$seq <- c(1:nrow(df.iv.dependents))
woe_data <- sapply(rejected.applications.woe.data[,c("dependents")],getwoevalue,woedata=df.iv.dependents,"numeric")
rejected.applications.woe.data$dependents <- woe_data
min(rejected.applications.woe.data$dependents)
max(rejected.applications.woe.data$dependents)


df.iv.open.home.loan <- iv_value$Tables$open.home.loan
df.iv.open.home.loan$seq <- c(1:nrow(df.iv.open.home.loan))
woe_data <- sapply(rejected.applications.woe.data[,c("open.home.loan")],getwoevalue,woedata=df.iv.open.home.loan,"numeric")
rejected.applications.woe.data$open.home.loan <- woe_data
min(rejected.applications.woe.data$open.home.loan)
max(rejected.applications.woe.data$open.home.loan)

# ----
df.iv.profession <- iv_value$Tables$profession
df.iv.profession$seq <- c(1:nrow(df.iv.profession))
woe_data <- sapply(rejected.applications.woe.data[,c("profession")],getwoevalue,woedata=df.iv.profession, "factor")
rejected.applications.woe.data$profession <- woe_data
min(rejected.applications.woe.data$profession)
max(rejected.applications.woe.data$profession)


df.iv.open.auto.loan <- iv_value$Tables$open.auto.loan
df.iv.open.auto.loan$seq <- c(1:nrow(df.iv.open.auto.loan))
woe_data <- sapply(rejected.applications.woe.data[,c("open.auto.loan")],getwoevalue,woedata=df.iv.open.auto.loan,"numeric")
rejected.applications.woe.data$open.auto.loan <- woe_data
min(rejected.applications.woe.data$open.auto.loan)
max(rejected.applications.woe.data$open.auto.loan)

df.iv.residence.type <- iv_value$Tables$residence.type
df.iv.residence.type$seq <- c(1:nrow(df.iv.residence.type))
woe_data <- sapply(rejected.applications.woe.data[,c("residence.type")],getwoevalue,woedata=df.iv.residence.type,"factor")
rejected.applications.woe.data$residence.type <- woe_data
min(rejected.applications.woe.data$residence.type)
max(rejected.applications.woe.data$residence.type)

# ----
df.iv.education <- iv_value$Tables$education
df.iv.education$seq <- c(1:nrow(df.iv.education))
woe_data <- sapply(rejected.applications.woe.data[,c("education")],getwoevalue,woedata=df.iv.education,"factor")
rejected.applications.woe.data$education <- woe_data
min(rejected.applications.woe.data$education)
max(rejected.applications.woe.data$education)

df.iv.gender <- iv_value$Tables$gender
df.iv.gender$seq <- c(1:nrow(df.iv.gender))
woe_data <- sapply(rejected.applications.woe.data[,c("gender")],getwoevalue,woedata=df.iv.gender,"factor")
rejected.applications.woe.data$gender <- woe_data
min(rejected.applications.woe.data$gender)
max(rejected.applications.woe.data$gender)

df.iv.marital.status <- iv_value$Tables$marital.status
df.iv.marital.status$seq <- c(1:nrow(df.iv.marital.status))
woe_data <- sapply(rejected.applications.woe.data[,c("marital.status")],getwoevalue,woedata=df.iv.marital.status,"factor")
rejected.applications.woe.data$marital.status <- woe_data
min(rejected.applications.woe.data$marital.status)
max(rejected.applications.woe.data$marital.status)

# check structure of data set
str(rejected.applications.woe.data)

# prediction 

rejected.pred <- predict(object = final, newdata = rejected.applications.woe.data,type = "response")
rejected.applications.woe.data$pred <- rejected.pred
rejected.applications.woe.data$logodds <- log(rejected.applications.woe.data$pred/(1-rejected.applications.woe.data$pred) )
max(rejected.applications.woe.data$pred)
min(rejected.applications.woe.data$pred)

#score evaluation for rejected application
basepoint <- 400
odd <- 10
pdo <- 20



factor <- pdo/log(2)
offset <- basepoint - (factor * log(odd))

offset
factor

rejected.applications.woe.data$score <- round(offset -  (factor * rejected.applications.woe.data$logodds),0)
min(rejected.applications.woe.data$score)
max(rejected.applications.woe.data$score)
rejected.applications.woe.data$status <- ifelse(rejected.applications.woe.data$score >=331,0,1)

# number of application approved using model.

sum(!rejected.applications.woe.data$status)

# new model will approve 191 application from total 1425 rejected application. 
# impact of revenue if we approve 191 application.
approved.app.by.model <- rejected.applications.woe.data[rejected.applications.woe.data$status==0, c("app.id")]

sum(rejected.applications[rejected.applications$app.id %in% approved.app.by.model,c("outstanding.balanace")])

# if we use model, we will generate additional revenue of $191,227,155

# previous loss due to not approving applications.

sum(rejected.applications$outstanding.balanace)


# previously we are clearly lossing aroung $1,489,483,933