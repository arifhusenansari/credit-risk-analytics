wdpath <- "E:\\Arif\\Work\\Data Science Course\\Upgrad PGDDS\\Domain Elective\\BFSI\\Capstone Project\\"
setwd(wdpath)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(Information)
library(stringr)

#================================== Business Problem ==================================

# CredX is a leading credit card provider that gets thousands of credit card applicants every year. 
# But in the past few years, it has experienced an increase in credit loss. 
# The CEO believes that the best strategy to mitigate credit risk is to 'acquire the right customers'.
# In this project, you will help CredX identify the right customers using predictive models. 
# Using past data of the bank's applicants, you need to determine the factors affecting credit risk,
# create strategies to mitigate the acquisition risk and assess the financial benefit of your project.


#================================== Business Problem Understanding ==================================

# Problem is related to credit loss and to mitigate this we are focusing on how to acquire right customers.
# We are interested in Acquisition risk analytics. Based on the data available build a scoring system.


# Two data files are available.
# 1.  Demographic data: 
#     Data related to customer demographics

# 2.  Credit bureau data: 
#     Data about behaviour of customer on credit card. DPS details since last 6 month and 12 months.
#     number of trades and other loans details.


#=============================== Global Function and Variables ================================

theme_title <- theme(plot.title = element_text(size = 10, face = "bold"))

plotBarChart <- function(d,col,colname){
  
  plot <- ggplot(data = d, aes(x= as.factor(col)))+geom_bar()
  plot <- plot + geom_text(stat = 'count',aes(label=..count.. , vjust=-1))+xlab(colname)
  plot
  
}

plotHistBox <- function(d,col,colname,binWidth=10){
  
  pHist <- ggplot(data= d , aes(x= col))+geom_histogram(binwidth = binWidth)+xlab(colname)
  
  pBox <- ggplot(data= d , aes(y= col))+geom_boxplot()+xlab(colname)
  grid.arrange(ncol=1,nrow=2,pHist,pBox)
  
}

plotHist <- function(d,col,colname,binWidth=10,title){
  pHist <- ggplot(data= d , aes(x= col))+geom_histogram(binwidth = binWidth)+xlab(colname)+ggtitle(title)
  return ( pHist)
}

plot_bar <- function(data,variable,variablename,title){
  
  aggr.data <- aggregate(status ~ variable,data,mean)
  count<- data.frame(table(as.factor(variable)))[,2]
  aggr.data$count <- count
  colnames(aggr.data) <- c("gender","default.rate","total.applicant")
  aggr.data$default.rate <- round(aggr.data$default.rate,4)*100
  return(ggplot(aggr.data,aes(x = aggr.data[,1] , y = total.applicant, label = default.rate))+
           geom_bar(stat =  "identity")+theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
           geom_text(size = 4, vjust = -0.5)+xlab(variablename)+ylab("count")+ggtitle(title))
}

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




#================================== Demographic Details ==================================

#=========================== Data Understanding and Data Cleaning ==========================



#---- Read Data from File
demographic <- read.csv("Demographic data.csv",header=TRUE, sep=",")

#---- give meaningful names
colnames(demographic) <- c("app.id","age","gender","marital.status","dependents","income","education","profession"
                           ,"residence.type","months.in.residence","months.in.company","status")


#---- Data gathering 
# Data is provided by upgrade to analyse and build a model to acquire good customers.

#---- Data Description
# 1)  File has 71295 observation and 11 independent and 1 dependent (output) variable.
# 2)  File have both type of columns categorical and numeric.
# 3)  There are some independent variables having missing values or invalid values.
# 4)  dependent variable has 1425 missing values, we will remove missing status data.

#     No duplicate data.


dim(demographic)
str(demographic)
summary(demographic)
sum(duplicated(demographic))



# 1425 observation has NA in Status column. (2% of total data).
# removing records with missing Status from actual dataset and store in other frame.

summary(as.factor(demographic$status))
demographic.na <- (demographic[is.na(demographic$status),])
demographic <- (demographic[!is.na(demographic$status),])
summary(as.factor(demographic$status))
summary(as.factor(demographic.na$status))
prop.table(table(demographic$status))


#---- Understanding By Columns

colnames(demographic)

# 1)  app.id: not useful

# Three duplicate application id. 
# we will consider only one of it.

sum(duplicated(demographic$app.id))
demographic <- demographic[which(!duplicated(demographic$app.id)),]

# cross check duplicate exclusion
sum(duplicated(demographic$app.id))

# 2)  age:  Cleaning Needed (invalid values)

#     No missing values
#     low value outliers are there
#     20 rows having invalid values (age less than or equal to 0)
#     Age is normally distributed
#     invalid values will be converted to NA. 
#     woe transformation will take care of NA values.

summary(demographic$age)
sum(is.na(demographic$age))
dim(demographic[demographic$age<=0,])
min(demographic[demographic$age>0,]$age)

plotHistBox(demographic,demographic$age,"Age",5)

quantile(demographic$age,probs = seq(0,1,0.01))

# replace invalid age to NA
demographic[demographic$age<=0,]$age <- NA
quantile(demographic$age,probs = seq(0,1,0.01),na.rm = T)
summary(demographic$age)

# 3)  gender: Cleaning Needed (invalid values)

#     No missing values.
#     2 rows with invalid values.

# Male Applicant are three times higher than Female applicant
# Total Application: 69870
# Male Applicant: 53363 (76%)
# Female Applicant: 16506 (24%)


summary(demographic$gender)
sum(is.na(demographic$gender))
demographic[demographic$gender=="",]

plotBarChart (demographic,demographic$gender,colname="Gender")

# replace 2 invalid gender to Male (M). Since 76% applicant are Male.
demographic[demographic$gender=="",]$gender= "M"
demographic$gender <- as.factor(as.character(demographic$gender))
summary(as.factor(demographic$gender))
plotBarChart (demographic,demographic$gender,colname="Gender")


# 4)  marital.status: Cleaning Needed (invalid values)

#     No missing values
#     6 rows with invalid values

# Total Applicatione: 69870
# Large Married people apply for credit card compare to marrid, might be due to their expenses.
# Married: 59547 (85%)
# Single: 10317 (15%)


summary(demographic$marital.status)
sum(is.na(demographic$marital.status))
nrow(demographic[demographic$marital.status=="",])

plotBarChart(demographic,demographic$marital.status,"Marital Status")

# We can replace invalid values with Married Status. Because of 85% applicant are married. 
demographic[demographic$marital.status=="",]$marital.status = "Married"
demographic$marital.status = as.factor(as.character(demographic$marital.status))
summary(demographic$marital.status)

plotBarChart(demographic,demographic$marital.status,"Marital Status")

# 5)  dependents: Cleaning Needed (missing values)  

#     3 missing values.
#     No big difference in number of application in different group of dependents
#     But Applicant having 4 to 5 dependent are less compare to 1 to 3 dependents 

summary(demographic$dependents)
sum(is.na(demographic$dependents))
plotBarChart(demographic,demographic$dependents,colname = "Dependensts")



# 6)  income: Cleaning Needed (invalid values)  
#     Assumption: income is in thousand per month.

#     No missing values
#     107 rows with negative or 0 income that is invalid.
#     income is normally distributed and almost 80% among all applicant, earning between 10k to 50k.
#     No outlier in the data, just some invalid values.

summary(demographic$income)
sum(is.na(demographic$income))
nrow(demographic[demographic$income<=0,])
demographic[demographic$income<=0,]
min(demographic[demographic$income>0,]$income)

plotHistBox(demographic,demographic$income,"income",10)
quantile(demographic$income , probs =  seq(0,1,0.01))

# replace invalid salary value to NA. woe will take care of NA values.

demographic[demographic$income<=0,] $ income <-NA
plotHistBox(demographic,demographic$income,"income",10)

# 7)  education:  Cleaning Needed(invalid values)

#     No missing value
#     118 rows with blank.
#     47867 applicant are either Masters or Professional, which can be significant.
#     17302, applicant are Bachelor, which is bit less than Masters or professional.

summary(demographic$education)
sum(is.na(demographic$education))
nrow(demographic[demographic$education=="",])
plotBarChart(demographic,demographic$education,colname = "Education")
demographic[demographic$education=="",]$education <- NA
demographic$education <- as.factor(as.character(demographic$education))
summary(demographic$education)

# 8)  profession: Cleaning Needed (invalid values)

#     No missing values
#     Almost 57% applicant are in "SAL" profession.
#     13 rows with blank.

summary(demographic$profession)
sum(is.na(demographic$profession))
demographic[demographic$profession=="",]
plotBarChart(demographic,demographic$profession,"Profession")

demographic[demographic$profession=="",]$profession<-NA
demographic$profession <- as.factor(as.character(demographic$profession))
plotBarChart(demographic,demographic$profession,"Profession")
summary(demographic$profession)


# 9)  residence.type: Cleaning Needed (invalid values)

#     No missing values
#     8 rows with blank residence type, we can replace it with Rented.
#     Among all applicant 75% are rented. 

summary(demographic$residence.type)
sum(is.na(demographic$residence.type))
plotBarChart(demographic,demographic$residence.type,"Residence")


# replace blank with Rented. because 75% are rented.

demographic[demographic$residence.type == "",]$residence.type <- "Rented"
demographic$residence.type <- as.factor(as.character(demographic$residence.type))
summary(demographic$residence.type)
plotBarChart(demographic,demographic$residence.type,"Residence")

# 10) month.in.residence: No Cleaning Needed.

#     No missing values
#     No invalid values.
#     No outliers
#     As per histogram, data is right skewed. hence mean is higer than median.
#     Almost 50% applicant, spend between 6 to 10 months in current residence. Can be significant.
#     Because, most of them are rented. There are high changes that they will leave the residence frequently.
#     We can create bins for duration.

summary(demographic$months.in.residence)
quantile(demographic$months.in.residence,probs = seq(0,1,0.01))
plotHistBox(demographic,demographic$months.in.residence,"Months In residence",20)


# 11) month.in.company: No Cleaning Needed

#     No missing values
#     No invalid values
#     There are some outliers. woe will take care of outliers
#     data is normally distributed.
#     We can create bin for this.


summary(demographic$months.in.company)
quantile(demographic$months.in.company,probs = seq(0,1,0.01))
plotHistBox(demographic,demographic$months.in.company,"Months in Company",10)


#================================== Credit Bureau and Demographic Details ==================================

#=========================== Data Understanding and Cleaning ==========================

#=== Note:  We will merge credit bureau and Demographics data for further understanding and cleaning 
#           process of credit bureau data.


# In some cases, you will find that all the variables in the credit bureau data are zero and credit card utilisation is missing.
# These represent cases in which there is a no-hit in the credit bureau. 
# You will also find cases with credit card utilisation missing. 
# These are the cases in which the applicant does not have any other credit card.


#------ Read data

credit.bureau <- read.csv("Credit Bureau data.csv", header = T)
View(credit.bureau)


#------ Data gathering

# Credit bureau data is provided by authority. It has customer behaviour data.
# E.g. No of time 60 DPD since 6 or 12 months, No of home and car loans.
# Credit card unitilization and trade details etc.


#------ Data Description
# 1)  71295 observation same as demographic data, and 19 variable.
# 2)  Only numeric data is available including dependent variable. We will convert dependent to factor.
# 3)  No duplicate data is there.
# 4)  there are 3028 missing values, we will understand more in column level data understanding.
# 5)  Dependent variable has 1425 missing values, we will remove missing status data.

dim(credit.bureau)
str(credit.bureau)
sum(duplicated(credit.bureau))
sum(is.na(credit.bureau))


# Check for missing performance tag.
# performance tagging is same as demographic data. 
# remove all rows with missing performance tag from credit bureau data. Same as demographic
summary(as.factor(credit.bureau$Performance.Tag))
summary(as.factor(demographic$status))


# give meaningful and sort column name to dataset.

colnames(credit.bureau)  <- c("app.id","num.90.dpd.or.worse.6.months","num.60.dpd.or.worse.6.months","num.30.dpd.or.worse.6.months",
                              "num.90.dpd.or.worse.12.months","num.60.dpd.or.worse.12.months","num.30.dpd.or.worse.12.months",
                              "avg.cc.utilization.12.months","open.trades.in.6.months","open.trades.in.12.months","open.pl.trades.in.6.months",
                              "open.pl.trades.in.12.months","cc.inquiries.6.months","cc.inquiries.12.months","open.home.loan","outstanding.balanace",
                              "total.trades","open.auto.loan","status")

colnames(credit.bureau)

# observation under different performance tag is now same as demographic.
# remove rows with missing performance tag and store in separate data frame
credit.bureau.na <- credit.bureau[is.na(credit.bureau$status),]
credit.bureau <- credit.bureau[!is.na(credit.bureau$status),]
summary(as.factor(credit.bureau$status))
summary(as.factor(credit.bureau.na$status))


sum(credit.bureau[credit.bureau$status==1 & !is.na(credit.bureau$outstanding.balanace) , c("outstanding.balanace")])
#---- check for duplicate application id.

# 3 application id are duplicate.

sum(duplicated(credit.bureau$app.id))
credit.bureau <- credit.bureau[which(!duplicated(credit.bureau$app.id)),]

# check for perfomance tag distribution in demographics and credit bureau data.
# distribution is same, means correct duplicate rows are excluded.
summary(as.factor(demographic$status))
summary(as.factor(credit.bureau$status))

#------ Data merging with demographic
# merge both with NA and without NA data.
# demographic and credit bureau data will be merged and validated whether status are in sync or not for each application. 

dim(demographic)
dim(credit.bureau)


# merging do data set using application id and validating status column
demographic.credit <- merge(x=demographic,y=credit.bureau,by.x= "app.id",by.y = "app.id")
str(demographic.credit)

# merging data having NA in status for demographics as well as credit bureau.
demographic.credit.na <- merge(x=demographic.na,y=credit.bureau.na,by.x= "app.id",by.y = "app.id")
str(demographic.credit.na)

# check summary
summary(demographic.credit)
summary(demographic.credit.na)

# All status values are in sync between demographic and credit bureau data.
# we will take only one status column and remove others.
sum(demographic.credit$status.x!=demographic.credit$y)

# remove column status.x and rename status.y to status
colnames(demographic.credit)
demographic.credit$status <- demographic.credit$status.y
demographic.credit <- demographic.credit[,-c(12,30)]
colnames(demographic.credit)

# remove column status.x and rename status.y to status from demographic.credit.na
colnames(demographic.credit.na)
demographic.credit.na$status <- demographic.credit.na$status.y
demographic.credit.na <- demographic.credit.na[,-c(12,30)]
colnames(demographic.credit.na)

# check for final data dimension
# number of observation are matching with actual data. 
dim(demographic.credit)
dim(demographic.credit.na)

#-- Get the outstanding amount for Defaulters

sum(demographic.credit[demographic.credit$status==1,c("outstanding.balanace")],na.rm = T)
sum(demographic.credit[demographic.credit$status==0,c("outstanding.balanace")],na.rm = T)

#-- data is ready for data understanding for column by column in credit bureau. 

# 1)  90.dpd.or.worse.6.months: No cleaning needed

# no missing values.
# no invalid values.
# column should be converted categor and even we can create bins 
# mostly applicant having 0, 90 days dpd in last 6 monhts (78%).
# hence, 22% having at least 1, 90 days dpd in last 6 months.
# applicant having more or equal to 1, 90 days dpd past 6 monts, almost 87% having 1 dpd.

summary(demographic.credit$num.90.dpd.or.worse.6.months)
summary(as.factor(demographic.credit$num.90.dpd.or.worse.6.months))
plotBarChart(demographic.credit,demographic.credit$num.90.dpd.or.worse.6.months,"90 Days DPD Last 6 Months")
plotHistBox(demographic.credit,demographic.credit$num.90.dpd.or.worse.6.months,"90 Days DPD Last 6 Months",1)

group_by(demographic.credit,demographic.credit$num.90.dpd.or.worse.6.months) %>% summarise(nApplication = n())


# 2)  num.60.dpd.or.worse.6.months: No cleaning needed

# no missing values
# no invalid values
# column should be converted categor and even we can create bins
# mostly applicant having 0, 60 days dpd in last 6 monhts (74%).
# hence, 26% having at least 1, 60 days dpd in last 6 months.which is 4% higher than 90 days dpd in 6 months.   
# applicant having more or equal to 1, 60 days dpd past 6 monts, almost 62% having 1 time 60 days dpd and 28% having 2 time 60 days dpd.

summary(demographic.credit$num.60.dpd.or.worse.6.months)
summary(as.factor(demographic.credit$num.60.dpd.or.worse.6.months))

plotBarChart(demographic.credit,demographic.credit$num.60.dpd.or.worse.6.months,"60 Days DPD Last 6 Months")
plotHistBox(demographic.credit,demographic.credit$num.60.dpd.or.worse.6.months,"60 Days DPD Last 6 Months",1)

group_by(demographic.credit,demographic.credit$num.60.dpd.or.worse.6.months) %>% summarise(nApplication = n())


# 3)  num.30.dpd.or.worse.6.months:

# no missing values
# no invalid values
# column should be converted categor and even we can create bins
# mostly applicant having 0, 30 days dpd in last 6 monhts (72%).
# hence, 28% having at least 1, 30 days dpd in last 6 months.which is 2% higher than 90 days dpd in 6 months.   
# applicant having more or equal to 1, 30 days dpd past 6 monts, almost 78% having 1 or 2 time 60 days dpd.
summary(demographic.credit$num.30.dpd.or.worse.6.months)
summary(as.factor(demographic.credit$num.30.dpd.or.worse.6.months))

plotBarChart(demographic.credit,demographic.credit$num.30.dpd.or.worse.6.months,"30 Days DPD Last 6 Months")
plotHistBox(demographic.credit,demographic.credit$num.30.dpd.or.worse.6.months,"30 Days DPD Last 6 Months",1)

group_by(demographic.credit,demographic.credit$num.30.dpd.or.worse.6.months) %>% summarise(nApplication = n())


# 4)  num.90.dpd.or.worse.12.months: no data cleaning needed

# no missing values
# no invalid values
# column should be converted categor and even we can create bins
# mostly applicant having 0, 90 days dpd in last 12 monhts (72%).
# hence, 28% having at least 1, 90 days dpd in last 12 months.which is 6% higher than 90 days dpd in 6 months.   
# applicant having more or equal to 1, 90 days dpd past 12 months, almost 92% having 1 or 2 time 90 days dpd.

summary(demographic.credit$num.90.dpd.or.worse.12.months)
summary(as.factor(demographic.credit$num.90.dpd.or.worse.12.months))

plotBarChart(demographic.credit,demographic.credit$num.90.dpd.or.worse.12.months,"90 Days DPD Last 12 Months")
plotHistBox(demographic.credit,demographic.credit$num.90.dpd.or.worse.12.months,"90 Days DPD Last 12 Months",1)

group_by(demographic.credit,demographic.credit$num.90.dpd.or.worse.12.months) %>% summarise(nApplication = n())


# 5)  num.60.dpd.or.worse.12.months: no data cleaning needed 

# no missing values
# no invalid values
# column should be converted categor and even we can create bins
# 66% applicant having 0, 60 days dpd in last 12 monhts.
# hence, 34% having at least 1, 60 days dpd in last 12 months.
# applicant having more or equal to 1, 60 days dpd past 12 months, almost 80% having 1 or 2 time 90 days dpd.

summary(demographic.credit$num.60.dpd.or.worse.12.months)
summary(as.factor(demographic.credit$num.60.dpd.or.worse.12.months))

plotBarChart(demographic.credit,demographic.credit$num.60.dpd.or.worse.12.months,"60 Days DPD Last 12 Months")
plotHistBox(demographic.credit,demographic.credit$num.60.dpd.or.worse.12.months,"60 Days DPD Last 12 Months",1)

group_by(demographic.credit,demographic.credit$num.60.dpd.or.worse.12.months) %>% summarise(nApplication = n())


# 6)  num.30.dpd.or.worse.12.months: no data cleaning needed 

# no missing values
# no invalid values
# column should be converted categor and even we can create bins
# 64% applicant (44858) having 0, 30 days dpd in last 12 monhts.
# hence, 36% having at least 1, 30 days dpd in last 12 months.
# applicant having more or equal to 1, 30 days dpd past 12 months, almost 70% having 1 or 2 time 90 days dpd.

summary(demographic.credit$num.30.dpd.or.worse.12.months)
summary(as.factor(demographic.credit$num.30.dpd.or.worse.12.months))

plotBarChart(demographic.credit,demographic.credit$num.30.dpd.or.worse.12.months,"30 Days DPD Last 12 Months")
plotHistBox(demographic.credit,demographic.credit$num.30.dpd.or.worse.12.months,"30 Days DPD Last 12 Months",1)

group_by(demographic.credit,demographic.credit$num.30.dpd.or.worse.12.months) %>% summarise(nApplication = n())


# 7)  avg.cc.utilization.12.months: cleaning is needed.

# missing values
# there are outliers after 94 percentile.
# data is right skewed and hence median is lesser than mean.
# mostly applicant are utilizing card 0 to 30 times.
# mostly they are utilizing between 10 to 20.

summary(demographic.credit$avg.cc.utilization.12.months)

plotHistBox(demographic.credit,demographic.credit$avg.cc.utilization.12.months,"Avg CC Utilization in 12 Months",10)
quantile( demographic.credit$avg.cc.utilization.12.months,probs = seq(0,1,0.01),na.rm = TRUE)

# 8)  open.trades.in.6.months: Data cleaning needed

# only 1 missing value
# there are some outliers.
# 77% applicants have 3 or less open trades in last 6 months.

summary(demographic.credit$open.trades.in.6.months)
quantile(demographic.credit$open.trades.in.6.months,probs = seq(0,1,0.01), na.rm = T)
plotHistBox(demographic.credit,demographic.credit$open.trades.in.6.months,"Open trades in 6 month",2)

# 9)  open.trades.in.12.months: No data cleaning needed.

# no missing values
# outliers are there.

summary(demographic.credit$open.trades.in.12.months)
quantile(demographic.credit$open.trades.in.12.months,probs = seq(0,1,0.01), na.rm = T)
plotHistBox(demographic.credit,demographic.credit$open.trades.in.12.months, "Open trades in 12 months",2)

# 10)  open.pl.trades.in.6.months: No data cleaning needed.

# no missing values
# only 1 outlier.
# almost 44% applicant having 0 open pl trade in last 6 months.

summary(demographic.credit$open.pl.trades.in.6.months)
quantile(demographic.credit$open.pl.trades.in.6.months,probs = seq(0,1,0.01), na.rm = T)
plotHistBox(demographic.credit,demographic.credit$open.pl.trades.in.6.months, "Open PL trades in 6 months",2)

# 11) open.pl.trades.in.12.months: No data cleaning needed.

# no missing values
# only 1 outlier.
# almost 36% applicant having 0 open pl trade in last 12 months.
# means 8% more applicants have 1 or more, open pl trade compare to 6 months.

summary(demographic.credit$open.pl.trades.in.12.months)
quantile(demographic.credit$open.pl.trades.in.12.months,probs = seq(0,1,0.01), na.rm = T)
plotHistBox(demographic.credit,demographic.credit$open.pl.trades.in.12.months, "Open PL trades in 12 months",2)

# 12) cc.inquiries.6.months: No data cleaning needed.

# no missing values
# there are some outliers.
# almost 35% applicants did 0 times cc inquiries.
# 47% applicant did cc inquiries 1 to 3 times.

summary(demographic.credit$cc.inquiries.6.months)
quantile(demographic.credit$cc.inquiries.6.months,probs = seq(0,1,0.01), na.rm = T)
plotHistBox(demographic.credit,demographic.credit$cc.inquiries.6.months, "credit card inquiries 6 months",1)



# 13) cc.inquiries.12.months: No data cleaning needed.

# no missing values
# there are some outliers.
# almost 29% applicants did 0 times cc inquiries. which is less than credit card inquiries in 6 months.
# means over a peride of time, applicant do more inquiry about credit card.
# 57% applicant did cc inquiries 1 to 6 times.

summary(demographic.credit$cc.inquiries.12.months)
quantile(demographic.credit$cc.inquiries.12.months,probs = seq(0,1,0.01), na.rm = T)
plotHistBox(demographic.credit,demographic.credit$cc.inquiries.12.months, "credit card inquiries 12 months",1)

# 14) open.home.loan: Data cleaning needed.

# 272 missing values
# 26% applicant having home loan.
summary(factor(demographic.credit$open.home.loan))

# converted to factor.
demographic.credit$open.home.loan <- as.factor(demographic.credit$open.home.loan)
summary((demographic.credit$open.home.loan))


# 15) outstanding.balanace: Data cleaning needed.

# 272 missing values
# there are no outlier.
# data should be normalised.

summary((demographic.credit$outstanding.balanace))
quantile(demographic.credit$outstanding.balanace,probs = seq(0,1,0.01),na.rm = T)
plotHistBox(demographic.credit,demographic.credit$outstanding.balanace,"outstanding balance",120000)

# 16) total.trades: No Data clening needed.

# no missing values
# there are some outliers
# almost 88% applicant having total trades between 0 to 16.
summary((demographic.credit$total.trades))
quantile(demographic.credit$total.trades,probs = seq(0,1,0.01))
plotHistBox(demographic.credit,demographic.credit$total.trades,"total trades",4)

# 17) "open.auto.loan": No data cleaning needed.

# no missing values
# 8% applicant has auto loan.

demographic.credit$open.auto.loan <- as.factor(demographic.credit$open.auto.loan)
summary((demographic.credit$open.auto.loan))


#=================================== EDA ===================================
# We will do the EDA first then, do the final data preparation and feature transformation using woe and iv.


#-- create dataset for EDA

demographic.credit.eda <- demographic.credit

#==================== Uni Variate Analysis ======================


# 1)  age:  significant

# between age 30 to 60 we can see more defaulters. but applicants are high in that age range compare to otheres as well. 


summary(demographic.credit.eda$age)


pDefault <- plotHist(demographic.credit.eda[demographic.credit.eda$status==1,] ,
                        demographic.credit.eda[demographic.credit.eda$status==1,]$age,
                        "Age",5,"Defaulters By Age")
pNonDefault <- plotHist(demographic.credit.eda[demographic.credit.eda$status==0,] ,
                           demographic.credit.eda[demographic.credit.eda$status==0,]$age,
                           "Age",5,"Non Defaulters By Age")
pAll <- plotHist(demographic.credit.eda[demographic.credit.eda$status==0,] ,
                        demographic.credit.eda[demographic.credit.eda$status==0,]$age,
                        "Age",5,"All Applicant Distribution By Age")
grid.arrange(nrow =3, ncol =1, pAll, pDefault, pNonDefault )


# plot bar chart with default percentage in each age.
plot_bar (demographic.credit.eda,demographic.credit.eda$age,"age","Percentage of Defaulters by Age")

# creat bin for age group. 

# default rate for applicant between 30 to 50 is high compare to others. 
# default rate for applicant between 10 to 20 is very low compare to others.
demographic.credit.eda$age.group <-  as.factor(cut(demographic.credit.eda$age,breaks = seq(10,70,10)))
plot_bar(demographic.credit.eda,demographic.credit.eda$age.group,"Age Group","Percentage of Defaulters By Age Group")


# 2)  gender: Can be significant


# eventhough female applicant are low compare to males. default rate is bit higher than male.
# it might be due to financial stability for female is less than male. 
summary(demographic.credit.eda$gender)
table(demographic.credit.eda[demographic.credit.eda$gender=="F",c("status")])

plot_bar(data = demographic.credit.eda[demographic.credit.eda$status==1,],variable = demographic.credit.eda[demographic.credit.eda$status==1,]$gender,"gender","Percentage of Defaulters By Gender")
plot_bar(data = demographic.credit.eda[demographic.credit.eda$status==0,],variable = demographic.credit.eda[demographic.credit.eda$status==0,]$gender,"gender","Percentage of Defaulters By Gender")
plot_bar(data = demographic.credit.eda,variable = demographic.credit.eda$gender,"gender","Percentage of Defaulters By Gender")


# 3)  marital.status: significant.

# large population is married, but default rate for single in bit high by 0.11% compare to married.
# single applicant are more likely to be default, might be because married applicant are more financial stable 
# compare to signle, because of working spouses.
summary(demographic.credit.eda$marital.status)

plot_bar(data = demographic.credit.eda,variable = demographic.credit.eda$marital.status,"marital status","Percentage of Defaulters By Marital Status")


# 4)  dependents: might be significant
# applicant in each group are more or less same and default rate is also same.
# But for applicant having 2 dependents are less likely to be default.
summary(demographic.credit.eda$dependents)
plot_bar(data = demographic.credit.eda,variable = demographic.credit.eda$dependents,"dependents","Percentage of Defaulters By Number of Dependents")


# 5)  income: significant

summary(demographic.credit.eda$income)
p.def.income <- plotHist(demographic.credit.eda[demographic.credit.eda$status==1,],demographic.credit.eda[demographic.credit.eda$status==1,]$income,"income",5,"Income Histogram for Defaulters")
p.non.def.income <-  plotHist(demographic.credit.eda[demographic.credit.eda$status==0,],demographic.credit.eda[demographic.credit.eda$status==0,]$income,"income",5,"Income Histogram for Non Defaulters")
p.All <-  plotHist(demographic.credit.eda,demographic.credit.eda$income,"income",5,"Income Histogram for All Applicants")
grid.arrange(nrow =3, ncol =1 , p.All,p.def.income, p.non.def.income)

# bin the income
# applicant having income between 0 to 25, have high default rate almost 5%. 
# which is high compare to other income range.

demographic.credit.eda$income.bin<- as.factor(cut (demographic.credit.eda$income, seq(0,75,25)))

plot_bar(data = demographic.credit.eda, variable = demographic.credit.eda$income.bin,"income","Percentage of Defaulters By Income Group")

# 6)  education: significant

# for others 5.49% is the default rate. which is higher compare to others.
# Also, bachelor and masters having default rate bit higher than phd and professional.

summary(demographic.credit.eda$education)
plot_bar(data = demographic.credit.eda,variable = demographic.credit.eda$education,"education","Percentage of Defaulters By Education")

# 7)  profession: Significant 

# Even though SE (Selft Employed) population is low compare to others.
# Default rate is high 4.61%.
# 
summary(demographic.credit.eda$profession)

plot_bar(data = demographic.credit.eda, variable = demographic.credit.eda$profession,"Profession","Percentage of Defaulters By Profession")


# 8)  residence.type: significant

# applicant staying with parents or having company provided residence are most likely
# to be defaulted compare to others. 

summary(demographic.credit.eda$residence.type)
plot_bar(data = demographic.credit.eda, variable = demographic.credit.eda$residence.type,"Residence Type","Percentage of Defaulters By Residence Type")

# 9)  months.in.residence: significant.

summary(demographic.credit.eda$months.in.residence)
p.def.months.resi <- plotHist(demographic.credit.eda[demographic.credit.eda$status==1,],demographic.credit.eda[demographic.credit.eda$status==1,]$months.in.residence,"months in residence",10,"Number of months in Residence Histogram for defaulters")
p.non.def.months.resi <- plotHist(demographic.credit.eda[demographic.credit.eda$status==0,],demographic.credit.eda[demographic.credit.eda$status==0,]$months.in.residence,"months in residence",10,"Number of months in Residence Histogram for Non defaulters")

grid.arrange(nrow=2, ncol=1,p.def.months.resi,p.non.def.months.resi)

# bin data.
# applicant having 2 to 4 years of residence, are having high default rate.
# this is same for long term residence as well. 10 to 12 years.
demographic.credit.eda$months.in.residence.bin <- as.factor( cut(demographic.credit.eda$months.in.residence,seq(0,144,24)))
plot_bar(demographic.credit.eda, demographic.credit.eda$months.in.residence.bin,"months in resedence","Percentage of Defaulters By Number of Month is Residence")


# 10)  months.in.company: significant.

# number of applicants with less than 24 months with the company are less compare to others.
# but number of default are high with applicants, with less than 20 month in current company.

summary(demographic.credit.eda$months.in.company)
p.all <-  plotHist(demographic.credit.eda,demographic.credit.eda$months.in.company,"months in company",8,"Number of months in Company Histogram for All Applicant")
p.def.months.company <- plotHist(demographic.credit.eda[demographic.credit.eda$status==1,],demographic.credit.eda[demographic.credit.eda$status==1,]$months.in.company,"months in company",8,"Number of months in Company Histogram for Defaulters")
p.non.def.months.company <- plotHist(demographic.credit.eda[demographic.credit.eda$status==0,],demographic.credit.eda[demographic.credit.eda$status==0,]$months.in.company,"months in company",8,"Number of months in Company Histogram for Non Defaulters")

grid.arrange(nrow=3, ncol=1,p.all, p.non.def.months.company,p.def.months.company)

# bin data.

# for bin (0,12] and (12,24] default rate is almost 5%. Which is high compare to others.

demographic.credit.eda$months.in.company.bin <- as.factor( cut(demographic.credit.eda$months.in.company,seq(0,84,12)))
plot_bar(demographic.credit.eda, demographic.credit.eda$months.in.company.bin,"months in company","Percentage of Defaulters By Number of Months In Company")
  

# 11) num.90.dpd.or.worse.6.months: significant

# applicant having 1 90 days dpd in last 6 months having default rate of 7.34%.
# applicant didn't pay money in last 3 months back to back are most likely to default (11.06%).
# it's like once applicant fall into 90 days dpd with in last 6 month. 
# chances for them to become default will increase by 4.06% (7.34% - 3.28%)

summary(demographic.credit.eda$num.90.dpd.or.worse.6.months)
plot_bar(demographic.credit.eda,demographic.credit.eda$num.90.dpd.or.worse.6.months," 90 days dpd 6 months","Percentage of Defaulters By 90 days dpd 6 months")


# 11) num.60.dpd.or.worse.6.months: significant

# applicant having 1 60 days dpd in last 6 months having default rate of 7.04%.
# applicant having 3 60 days dpd in last 6 months having default rate of 10.07%.
# it's like once applicant fall into 90 days dpd with in last 6 month. 
# chances for them to become default will increase by 3.99% (7.04% - 3.05%)

summary(demographic.credit.eda$num.60.dpd.or.worse.6.months)
plot_bar(demographic.credit.eda,demographic.credit.eda$num.60.dpd.or.worse.6.months," 60 days dpd 6 months","Percentage of Defaulters By 60 days dpd 6 months")

# 11) num.30.dpd.or.worse.6.months: significant

# applicant having 1 30 days dpd in last 6 months having default rate of 6.54%.
# applicant having 4 30 days dpd in last 6 months having default rate of 10.24%.
# it's like once applicant fall into 90 days dpd with in last 6 month. 
# chances for them to become default will increase by 3.54% (6.54% - 2.9%)

summary(demographic.credit.eda$num.30.dpd.or.worse.6.months)
plot_bar(demographic.credit.eda,demographic.credit.eda$num.30.dpd.or.worse.6.months," 30 days dpd 6 months","Percentage of Defaulters By 30 days dpd in 6 months")



# 12) num.90.dpd.or.worse.12.months: significant

# applicant having 1 90 days dpd in last 12 months having default rate of 6.83%.
# applicant having 4 90 days dpd in last 12 months having default rate of 10.29%.
# it's like once applicant fall into 90 days dpd with in last 12 month. 
# chances for them to become default will increase by Approx 4% (6.83% - 2.9%)

summary(demographic.credit.eda$num.90.dpd.or.worse.12.months)
plot_bar(demographic.credit.eda,demographic.credit.eda$num.90.dpd.or.worse.12.months," 90 days dpd 12 months","Percentage of Defaulters By 90 days dpd in 12 months")

# 12) num.60.dpd.or.worse.12.months: significant

# as number of 60 dpd or worse in increases default percentage in increases as well. 

summary(demographic.credit.eda$num.60.dpd.or.worse.12.months)
plot_bar(demographic.credit.eda,demographic.credit.eda$num.60.dpd.or.worse.12.months," 60 days dpd 12 months","Percentage of Defaulters By 60 days dpd in 12 months")


# 13) num.30.dpd.or.worse.12.months: significant

# as number of 30 dpd or worse in increases default percentage in increases as well. 

summary(demographic.credit.eda$num.30.dpd.or.worse.12.months)
plot_bar(demographic.credit.eda,demographic.credit.eda$num.30.dpd.or.worse.12.months," 30 days dpd 12 months","Percentage of Defaulters By 30 days dpd in 12 months")
  
# 14) avg.cc.utilization.12.months: significant

# there are NA values, some of the data have not zero data in other columns like 90 days dpd or 60 days dpd.
# it's not like is credit card utilization is NA means there is no other credit card.

# applicant having cc unitlization between 25 to 70 are low compare to others.
# but, default rate is bit high with in this range, compare to others. 

summary(demographic.credit.eda$avg.cc.utilization.12.months)
p.all <- plotHist(demographic.credit.eda,demographic.credit.eda$avg.cc.utilization.12.months,"cc utilization in 12 months",5,"Average CC utilization Histogram for All applicant")
p.def.cc.12.month <- plotHist(demographic.credit.eda[demographic.credit.eda$status==1,] ,demographic.credit.eda[demographic.credit.eda$status==1,]$avg.cc.utilization.12.months,"cc utilization in 12 months",5,"Average CC utilization Histogram for Default applicant")
p.non.def.cc.12.month<- plotHist(demographic.credit.eda[demographic.credit.eda$status==0,] ,demographic.credit.eda[demographic.credit.eda$status==0,]$avg.cc.utilization.12.months,"cc utilization in 12 months",5,"Average CC utilization Histogram for Non Default applicant")

grid.arrange(nrow =3, ncol=1, p.all,p.def.cc.12.month,p.non.def.cc.12.month)

# bin the data
# we need to include 0 in the bin as well, so we are starting from -0.1.

# applicant having avg. cc utilization between 40 to 80. This is almost 7.5%. 
# this might be because those are having low salary and high utilization. 


demographic.credit.eda$avg.cc.utilization.12.months.bin <-  as.factor(cut(demographic.credit.eda$avg.cc.utilization.12.months,seq(-0.1,120,20)))
plot_bar(demographic.credit.eda,demographic.credit.eda$avg.cc.utilization.12.months.bin,"cc utilizatin by bins","Percentage of Defaulters By Avg CC utilization")


# 15)  open.trades.in.6.months: significant

# applicant having open trades between 1 to 5 are having high default rate compare to others.
summary(demographic.credit.eda$open.trades.in.6.months)
plot_bar(demographic.credit.eda,demographic.credit.eda$open.trades.in.6.months,"open trade 6 month","Percentage of Defaulters By Open Trade in 6 months")

# 16)  open.trades.in.12.months: significant

# applicant having open trades between 3 to 12 are having high default rate compare to others.
summary(demographic.credit.eda$open.trades.in.12.months)
plot_bar(demographic.credit.eda,demographic.credit.eda$open.trades.in.12.months,"open trade 12 month","Percentage of Defaulters By Open trades in 12 months")


# 17)  open.pl.trades.in.6.months: significant

# behavior is quite same as open trades in last 6 months.
# applicant having open pl trades between 3 to 12 are having high default rate compare to others.
summary(demographic.credit.eda$open.pl.trades.in.6.months)
plot_bar(demographic.credit.eda,demographic.credit.eda$open.pl.trades.in.6.months,"open pl trade 6 month","Percentage of Defaulters By open pl trade in 6 months")



# 17)  open.pl.trades.in.12.months: significant

# behavior is quite same as open trades in last 6 months.
# applicant having open pl trades between 3 to 9 are having high default rate compare to others.

summary(demographic.credit.eda$open.pl.trades.in.12.months)
plot_bar(demographic.credit.eda,demographic.credit.eda$open.pl.trades.in.12.months,"open pl trade 12 month","Percentage of Defaulters By open pl trade in 12 months")


# 18)  cc.inquiries.6.months: significant

# applicant normally doing 3 to 5 inquiries, having high default rate.

summary(demographic.credit.eda$cc.inquiries.6.months)
plot_bar(demographic.credit.eda,demographic.credit.eda$cc.inquiries.6.months,"cc inquiries 06 months","Percentage of Defaulters By CC inquiries in 6 months")



# 18)  cc.inquiries.12.months: significant

# behaviour is same. like applicant having inquiries with in middle range are more likely to default.
summary(demographic.credit.eda$cc.inquiries.12.months)
plot_bar(demographic.credit.eda,demographic.credit.eda$cc.inquiries.12.months,"cc inquiries 12 months","Percentage of Defaulters By CC inquiries in 12 months")


# 19)  open.home.loan: significant

# applicant having home loan are less likely to default. 
# might be because they have got home loan, and this kind of loan given to applicant
# with high stability in income. Hence, they are good customers.
summary(demographic.credit.eda$open.home.loan)
plot_bar(demographic.credit.eda,demographic.credit.eda$open.home.loan,"open home loan","Percentage of Defaulters By Open Home loan Status")

# 20)  outstanding.balanace: significant

# data is not normally distributed. we can see there are two range of outstanding balance. 

# in both the range applicant having outstanding in between, are more likely to default.

summary(demographic.credit.eda$outstanding.balanace)

p.all <- plotHist(demographic.credit.eda,demographic.credit.eda$outstanding.balanace,"outstanding balance",300000,"outstanding balance Histogram for All Applicant")
p.non.def.outstanding <- plotHist(demographic.credit.eda[demographic.credit.eda$status==0,],
                                  demographic.credit.eda[demographic.credit.eda$status==0,]$outstanding.balanace,"outstanding balance",300000,"Outstanding balance Histogram for Non Default Application")
p.def.outstanding <- plotHist(demographic.credit.eda[demographic.credit.eda$status==1,],
                              demographic.credit.eda[demographic.credit.eda$status==1,]$outstanding.balanace,"outstanding balance",300000,"outstanding balance Histogram for Default Applicant")


grid.arrange(nrow =3, ncol=1, p.all,p.def.outstanding,p.non.def.outstanding)

# bin outstanding amount

demographic.credit.eda$outstanding.balanace.bin <-  as.factor(cut(demographic.credit.eda$outstanding.balanace,seq(0,5400000,300000)))

plot_bar(demographic.credit.eda,demographic.credit.eda$outstanding.balanace.bin,"outstanding amount","Percentage of Defaulters By Outstanding Balance Bin")


# 21) total.trades: significant

summary(demographic.credit.eda$total.trades)
plot_bar(demographic.credit.eda,demographic.credit.eda$total.trades,"Total trades","Percentage of Defaulters By Total Trades" )


# bin the data
# applicant having 5 to 15, total trades they are more likely to default.
demographic.credit.eda$total.trades.bin <- as.factor(cut(demographic.credit.eda$total.trades,seq(-0.1,45,5)))
plot_bar(demographic.credit.eda,demographic.credit.eda$total.trades.bin,"total trades","Percentage of Defaulters By Total Trades(Bin)")


# 22) open.auto.loan: significant

# same behaviour as open home loan. applicant having open auto loan are less likely to default.
summary(demographic.credit.eda$open.auto.loan)
class(demographic.credit.eda$open.auto.loan)
plot_bar(demographic.credit.eda, demographic.credit.eda$open.auto.loan,"auto loan","Percentage of Defaulters By Open Auto Loan" )

#==== Conclusion Uni Variate analysis

# 1)  There are lot many variables, those are significant. 
# 2)  Most of them might have multi colinearity, mostly in credit bureau data.
# 3)  Variable's having missing values. but those will be treated using woe and information value.
#     We will transform most of the variables using woe and IV technique after bi variate analysis.
# 4)  In bi variate analysis, we will consider some specific variables to dig more into behaviour of specific variable.



#=========================== Bi Variate Analysis ==========================
demographic.credit.eda.default <- demographic.credit.eda[demographic.credit.eda$status==1,]
demographic.credit.eda.non.default <- demographic.credit.eda[demographic.credit.eda$status==0,]

# 1)  gender vs age
# in all the age group, we can see female
# there is not clear indicator between age and gender. 

p1 <-  ggplot(demographic.credit.eda.default[demographic.credit.eda.default$gender=="M",],aes(x = age.group,fill=as.factor(status)))+
  geom_bar( )
p2 <-  ggplot(demographic.credit.eda.default[demographic.credit.eda.default$gender=="F",],aes(x = age.group,fill=as.factor(status)))+
  geom_bar( )
grid.arrange(nrow =2 , ncol=1, p1,p2)

# 2)  gender vs marital.status
# no clear relationship. 

# female default rate is bit higher by 0.18% compare to male.
# but there is no correlation between marital.status. 
# Whether female is single or married, default rate is not significantly different.

p1 <-  ggplot(demographic.credit.eda[demographic.credit.eda$gender=="M",],aes(x = marital.status,fill=as.factor(status)))+
  geom_bar(position = "fill" )
p2 <-  ggplot(demographic.credit.eda[demographic.credit.eda$gender=="F",],aes(x = marital.status,fill=as.factor(status)))+
  geom_bar(position = "fill" )
grid.arrange(nrow =2 , ncol=1, p1,p2)

# 3) gender vs income

# no clear correlationship

p1 <-  ggplot(demographic.credit.eda[demographic.credit.eda$gender=="M",],aes(x = income.bin,fill=as.factor(status)))+
  geom_bar(position = "fill" )
p2 <-  ggplot(demographic.credit.eda[demographic.credit.eda$gender=="F",],aes(x = income.bin,fill=as.factor(status)))+
  geom_bar(position = "fill" )
grid.arrange(nrow =2 , ncol=1, p1,p2)

# 4) eduction vs profession

# number of good customers from different educations are same in each profession.
# but if you default graph, applicants those are from professional education
# are more likely to be default if they are SE_PROF in profession. 
# Bachelors who are SE are more likely to default.

p1 <-  ggplot(demographic.credit.eda.default,aes(x = profession,fill=as.factor(education)))+
  geom_bar(position = "fill" )
p2 <-  ggplot(demographic.credit.eda.non.default,aes(x = profession,fill=as.factor(education)))+
  geom_bar(position = "fill" )
grid.arrange(nrow =2 , ncol=1, p1,p2)

# 5) income vs education
# low earning masters and high earning professional applicant are bit more likely to default.
p1 <-  ggplot(demographic.credit.eda.default,aes(x = income.bin,fill=as.factor(education)))+
  geom_bar(position = "fill" )
p2 <-  ggplot(demographic.credit.eda.non.default,aes(x = income.bin,fill=as.factor(education)))+
  geom_bar(position = "fill" )

grid.arrange(nrow =2 , ncol=1, p1,p2)

# 6)  income vs profession

# as SE_PROF earning more they become less likely to default.
# as SAL profession class earhing more they are defaulting more.

p1 <-  ggplot(demographic.credit.eda.default,aes(x = income.bin,fill=as.factor(profession)))+
  geom_bar(position = "fill" )
p2 <-  ggplot(demographic.credit.eda.non.default,aes(x = income.bin,fill=as.factor(profession)))+
  geom_bar(position = "fill" )

grid.arrange(nrow =2 , ncol=1, p1,p2)


colnames(demographic.credit.eda)



#================================ Feature Transformation ============================


#============== feature transformation using WOE and IV. ==============

# Run Information value on demographics dataset
# woe is the ratio of bad to good. 
# create woe data set using origional dataset.
demographic.credit.woe.data <- demographic.credit
colnames(demographic.credit.woe.data)
str(demographic.credit.woe.data)

iv_value <- create_infotables(demographic.credit.woe.data,y = "status",bin =10)
iv_value$Summary


# save iv value for future use.

write.csv(iv_value$Summary,paste(wdpath,"Data\\iv_values.csv",sep=""),row.names = F)

# As per analysis, IV value less than 0.02 means predicator is not useful.
# between 0.02 and 0.1 is showing weak relationship.
# between 0.1 to 0.3 is medium power relationship
# between 0.3 to 0.5 means it's having very strong predictive power. 
# greater than 0.5 it is suspicious predictive power.

#------ go one by one all the variables and check woe and plot it.====
#-- based on significance we will build fina dataset.

# 1)  avg.cc.utilization.12.months: information value 0.3099
# means variable has strong relationship.
# woe is also monotonic, it's means as avg cc utilization is increasing number of defaulters are increasing.
# we can replace avg.cc.utilization.12.months.bin with this bin values.

df.iv.avg.cc.utilization.12.months <- iv_value$Tables$avg.cc.utilization.12.months
df.iv.avg.cc.utilization.12.months$seq <- c(1:nrow(df.iv.avg.cc.utilization.12.months))
df.iv.avg.cc.utilization.12.months
p1 <- plot_infotables(iv_value,"avg.cc.utilization.12.months")
p2 <- ggplot(df.iv.avg.cc.utilization.12.months, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)

# replace actual with woe data

woe_data <- sapply(demographic.credit.woe.data[,c("avg.cc.utilization.12.months")],getwoevalue,woedata=df.iv.avg.cc.utilization.12.months,"numeric")

demographic.credit.woe.data$avg.cc.utilization.12.months <- woe_data



# 2)  open.trades.in.12.months: information value 0.298 Approx 0.3 

# variable has strong relationship. 
# woe is also monotonic, same as avg cc unitilization. 
# as number of open trades in 12 months are high applicant are most likely to default.

df.iv.open.trades.in.12.months <- iv_value$Tables$open.trades.in.12.months
df.iv.open.trades.in.12.months$seq <- c(1:nrow(df.iv.open.trades.in.12.months))


p1 <- plot_infotables(iv_value,"open.trades.in.12.months")
p2 <- ggplot(df.iv.open.trades.in.12.months, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)


# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("open.trades.in.12.months")],getwoevalue,woedata=df.iv.open.trades.in.12.months,"numeric")

# check for NA values
sum(is.na(woe_data))

demographic.credit.woe.data$open.trades.in.12.months <- woe_data


# 3)  open.pl.trades.in.12.months: iv (0.296) approx 0.3 

# variable has strong relationship.
# as number of pl trades are increases, number of defaulters are increases.

df.iv.open.pl.trades.in.12.months <- iv_value$Tables$open.pl.trades.in.12.months
df.iv.open.pl.trades.in.12.months$seq <- c(1:nrow(df.iv.open.pl.trades.in.12.months))


p1 <- plot_infotables(iv_value,"open.pl.trades.in.12.months")
p2 <- ggplot(df.iv.open.pl.trades.in.12.months, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.open.pl.trades.in.12.months


# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("open.pl.trades.in.12.months")],getwoevalue,woedata=df.iv.open.pl.trades.in.12.months,"numeric")

demographic.credit.woe.data$open.pl.trades.in.12.months <- woe_data


# 4)  cc.inquiries.12.months: iv (0.295) approx  0.3

# variable has strong relationship with output.
# applicant inquiring more about credit care are more likely to default.

df.iv.cc.inquiries.12.months <- iv_value$Tables$cc.inquiries.12.months
df.iv.cc.inquiries.12.months$seq <- c(1:nrow(df.iv.cc.inquiries.12.months))

p1 <- plot_infotables(iv_value,"cc.inquiries.12.months")
p2 <- ggplot(df.iv.cc.inquiries.12.months, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.cc.inquiries.12.months

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("cc.inquiries.12.months")],getwoevalue,woedata=df.iv.cc.inquiries.12.months,"numeric")
demographic.credit.woe.data$cc.inquiries.12.months <- woe_data


# 5)  outstanding.balanace: iv (0.24)  

# variable has weak relationship with output.
df.iv.outstanding.balanace <- iv_value$Tables$outstanding.balanace
df.iv.outstanding.balanace$seq <- c(1:nrow(df.iv.outstanding.balanace))


p1 <- plot_infotables(iv_value,"outstanding.balanace")
p2 <- ggplot(df.iv.outstanding.balanace, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.outstanding.balanace

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("outstanding.balanace")],getwoevalue,woedata=df.iv.outstanding.balanace,"numeric")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$outstanding.balanace <- woe_data


# 6)  num.30.dpd.or.worse.6.months: iv (0.24)

# weak relatiohship on output.

df.iv.num.30.dpd.or.worse.6.months <- iv_value$Tables$num.30.dpd.or.worse.6.months
df.iv.num.30.dpd.or.worse.6.months$seq <- c(1:nrow(df.iv.num.30.dpd.or.worse.6.months))


p1 <- plot_infotables(iv_value,"num.30.dpd.or.worse.6.months")
p2 <- ggplot(df.iv.num.30.dpd.or.worse.6.months, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.num.30.dpd.or.worse.6.months

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("num.30.dpd.or.worse.6.months")],getwoevalue,woedata=df.iv.num.30.dpd.or.worse.6.months,"numeric")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$num.30.dpd.or.worse.6.months <- woe_data


# 7)  total.trades: iv (0.24)

# weak relatiohship on output.

df.iv.total.trades <- iv_value$Tables$total.trades
df.iv.total.trades$seq <- c(1:nrow(df.iv.total.trades))


p1 <- plot_infotables(iv_value,"total.trades")
p2 <- ggplot(df.iv.total.trades, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.total.trades

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("total.trades")],getwoevalue,woedata=df.iv.total.trades,"numeric")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$total.trades <- woe_data

# 8)  open.pl.trades.in.6.months: iv (0.22)

# weak relationship with output.

df.iv.open.pl.trades.in.6.months <- iv_value$Tables$open.pl.trades.in.6.months
df.iv.open.pl.trades.in.6.months$seq <- c(1:nrow(df.iv.open.pl.trades.in.6.months))


p1 <- plot_infotables(iv_value,"open.pl.trades.in.6.months")
p2 <- ggplot(df.iv.open.pl.trades.in.6.months, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.open.pl.trades.in.6.months

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("open.pl.trades.in.6.months")],getwoevalue,woedata=df.iv.open.pl.trades.in.6.months,"numeric")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$open.pl.trades.in.6.months <- woe_data

# 9)  num.90.dpd.or.worse.12.months: iv(0.21)
# weak relationship with output

df.iv.num.90.dpd.or.worse.12.months <- iv_value$Tables$num.90.dpd.or.worse.12.months
df.iv.num.90.dpd.or.worse.12.months$seq <- c(1:nrow(df.iv.num.90.dpd.or.worse.12.months))


p1 <- plot_infotables(iv_value,"num.90.dpd.or.worse.12.months")
p2 <- ggplot(df.iv.num.90.dpd.or.worse.12.months, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.num.90.dpd.or.worse.12.months

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("num.90.dpd.or.worse.12.months")],getwoevalue,woedata=df.iv.num.90.dpd.or.worse.12.months,"numeric")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$num.90.dpd.or.worse.12.months <- woe_data

# 10)  num.60.dpd.or.worse.6.months: iv(0.21)

df.iv.num.60.dpd.or.worse.6.months <- iv_value$Tables$num.60.dpd.or.worse.6.months
df.iv.num.60.dpd.or.worse.6.months$seq <- c(1:nrow(df.iv.num.60.dpd.or.worse.6.months))


p1 <- plot_infotables(iv_value,"num.60.dpd.or.worse.6.months")
p2 <- ggplot(df.iv.num.60.dpd.or.worse.6.months, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.num.60.dpd.or.worse.6.months

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("num.60.dpd.or.worse.6.months")],getwoevalue,woedata=df.iv.num.60.dpd.or.worse.6.months,"numeric")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$num.60.dpd.or.worse.6.months <- woe_data


# 11)  cc.inquiries.6.months: iv(0.21)

df.iv.cc.inquiries.6.months <- iv_value$Tables$cc.inquiries.6.months
df.iv.cc.inquiries.6.months$seq <- c(1:nrow(df.iv.cc.inquiries.6.months))


p1 <- plot_infotables(iv_value,"cc.inquiries.6.months")
p2 <- ggplot(df.iv.cc.inquiries.6.months, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.cc.inquiries.6.months

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("cc.inquiries.6.months")],getwoevalue,woedata=df.iv.cc.inquiries.6.months,"numeric")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$cc.inquiries.6.months <- woe_data

# 12)  num.30.dpd.or.worse.12.months: iv(0.20)

df.iv.num.30.dpd.or.worse.12.months <- iv_value$Tables$num.30.dpd.or.worse.12.months
df.iv.num.30.dpd.or.worse.12.months$seq <- c(1:nrow(df.iv.num.30.dpd.or.worse.12.months))


p1 <- plot_infotables(iv_value,"num.30.dpd.or.worse.12.months")
p2 <- ggplot(df.iv.num.30.dpd.or.worse.12.months, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.num.30.dpd.or.worse.12.months

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("num.30.dpd.or.worse.12.months")],getwoevalue,woedata=df.iv.num.30.dpd.or.worse.12.months,"numeric")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$num.30.dpd.or.worse.12.months <- woe_data

# 13)  open.trades.in.6.months: iv(0.2)

df.iv.open.trades.in.6.months <- iv_value$Tables$open.trades.in.6.months
df.iv.open.trades.in.6.months$seq <- c(1:nrow(df.iv.open.trades.in.6.months))


p1 <- plot_infotables(iv_value,"open.trades.in.6.months")
p2 <- ggplot(df.iv.open.trades.in.6.months, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.open.trades.in.6.months

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("open.trades.in.6.months")],getwoevalue,woedata=df.iv.open.trades.in.6.months,"numeric")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$open.trades.in.6.months <- woe_data

# 14)  num.60.dpd.or.worse.12.months: iv(0.2)

df.iv.num.60.dpd.or.worse.12.months <- iv_value$Tables$num.60.dpd.or.worse.12.months
df.iv.num.60.dpd.or.worse.12.months$seq <- c(1:nrow(df.iv.num.60.dpd.or.worse.12.months))


p1 <- plot_infotables(iv_value,"num.60.dpd.or.worse.12.months")
p2 <- ggplot(df.iv.num.60.dpd.or.worse.12.months, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.num.60.dpd.or.worse.12.months

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("num.60.dpd.or.worse.12.months")],getwoevalue,woedata=df.iv.num.60.dpd.or.worse.12.months,"numeric")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$num.60.dpd.or.worse.12.months <- woe_data

# 15)  num.90.dpd.or.worse.6.months: iv(0.16)


df.iv.num.90.dpd.or.worse.6.months <- iv_value$Tables$num.90.dpd.or.worse.6.months
df.iv.num.90.dpd.or.worse.6.months$seq <- c(1:nrow(df.iv.num.90.dpd.or.worse.6.months))


p1 <- plot_infotables(iv_value,"num.90.dpd.or.worse.6.months")
p2 <- ggplot(df.iv.num.90.dpd.or.worse.6.months, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.num.90.dpd.or.worse.6.months

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("num.90.dpd.or.worse.6.months")],getwoevalue,woedata=df.iv.num.90.dpd.or.worse.6.months,"numeric")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$num.90.dpd.or.worse.6.months <- woe_data

# 16)  months.in.residence: iv(0.07)

# weak predictive power.

df.iv.months.in.residence <- iv_value$Tables$months.in.residence
df.iv.months.in.residence$seq <- c(1:nrow(df.iv.months.in.residence))


p1 <- plot_infotables(iv_value,"months.in.residence")
p2 <- ggplot(df.iv.months.in.residence, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.months.in.residence

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("months.in.residence")],getwoevalue,woedata=df.iv.months.in.residence,"numeric")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$months.in.residence <- woe_data

# 17)  income: iv(0.04)
# weak predictive power


df.iv.income <- iv_value$Tables$income
df.iv.income$seq <- c(1:nrow(df.iv.income))


p1 <- plot_infotables(iv_value,"income")
p2 <- ggplot(df.iv.income, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.income

# replace actual with woe data
woe_data <- sapply(as.integer(demographic.credit.woe.data[,c("income")]),getwoevalue,woedata=df.iv.income,"numeric")

# check for NA values
sum(is.na(woe_data))

demographic.credit.woe.data$income <- woe_data

# 18)  months.in.company: iv(0.021)
# weak predictive power

df.iv.months.in.company <- iv_value$Tables$months.in.company
df.iv.months.in.company$seq <- c(1:nrow(df.iv.months.in.company))


p1 <- plot_infotables(iv_value,"months.in.company")
p2 <- ggplot(df.iv.months.in.company, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.months.in.company

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("months.in.company")],getwoevalue,woedata=df.iv.months.in.company,"numeric")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$months.in.company <- woe_data


# 19)  age: 

df.iv.age <- iv_value$Tables$age
df.iv.age$seq <- c(1:nrow(df.iv.age))


p1 <- plot_infotables(iv_value,"age")
p2 <- ggplot(df.iv.age, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.age

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("age")],getwoevalue,woedata=df.iv.age,"numeric")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$age <- woe_data


# 20)  dependents

df.iv.dependents <- iv_value$Tables$dependents
df.iv.dependents$seq <- c(1:nrow(df.iv.dependents))


p1 <- plot_infotables(iv_value,"dependents")
p2 <- ggplot(df.iv.dependents, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.dependents

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("dependents")],getwoevalue,woedata=df.iv.dependents,"numeric")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$dependents <- woe_data
str(demographic.credit.woe.data)

# 21)  open.home.loan


df.iv.open.home.loan <- iv_value$Tables$open.home.loan
df.iv.open.home.loan$seq <- c(1:nrow(df.iv.open.home.loan))

p1 <- plot_infotables(iv_value,"open.home.loan")
p2 <- ggplot(df.iv.open.home.loan, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.open.home.loan

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("open.home.loan")],getwoevalue,woedata=df.iv.open.home.loan,"factor")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$open.home.loan <- woe_data



# 22) profession 


df.iv.profession <- iv_value$Tables$profession
df.iv.profession$seq <- c(1:nrow(df.iv.profession))


p1 <- plot_infotables(iv_value,"profession")
p2 <- ggplot(df.iv.profession, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.profession

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("profession")],getwoevalue,woedata=df.iv.profession, "factor")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$profession <- woe_data

# 23)  open.auto.loan

df.iv.open.auto.loan <- iv_value$Tables$open.auto.loan
df.iv.open.auto.loan$seq <- c(1:nrow(df.iv.open.auto.loan))

p1 <- plot_infotables(iv_value,"open.auto.loan")
p2 <- ggplot(df.iv.open.auto.loan, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.open.auto.loan

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("open.auto.loan")],getwoevalue,woedata=df.iv.open.auto.loan,"factor")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$open.auto.loan <- woe_data
str(demographic.credit.woe.data)

# 24)  residence.type

df.iv.residence.type <- iv_value$Tables$residence.type
df.iv.residence.type$seq <- c(1:nrow(df.iv.residence.type))


p1 <- plot_infotables(iv_value,"residence.type")
p2 <- ggplot(df.iv.residence.type, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.residence.type

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("residence.type")],getwoevalue,woedata=df.iv.residence.type,"factor")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$residence.type <- woe_data



# 25)  education

df.iv.education <- iv_value$Tables$education
df.iv.education$seq <- c(1:nrow(df.iv.education))


p1 <- plot_infotables(iv_value,"education")
p2 <- ggplot(df.iv.education, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.education

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("education")],getwoevalue,woedata=df.iv.education,"factor")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$education <- woe_data

str(demographic.credit.woe.data)


# 26)  gender

df.iv.gender <- iv_value$Tables$gender
df.iv.gender$seq <- c(1:nrow(df.iv.gender))


p1 <- plot_infotables(iv_value,"gender")
p2 <- ggplot(df.iv.gender, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.gender

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("gender")],getwoevalue,woedata=df.iv.gender,"factor")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$gender <- woe_data

str(demographic.credit.woe.data)

# 27)  marital.status

df.iv.marital.status <- iv_value$Tables$marital.status
df.iv.marital.status$seq <- c(1:nrow(df.iv.marital.status))


p1 <- plot_infotables(iv_value,"marital.status")
p2 <- ggplot(df.iv.marital.status, aes(x= seq, y = WOE))+geom_line()
grid.arrange(nrow=2, ncol=1, p1,p2)
df.iv.marital.status

# replace actual with woe data
woe_data <- sapply(demographic.credit.woe.data[,c("marital.status")],getwoevalue,woedata=df.iv.marital.status,"factor")

# check for NA values
sum(is.na(woe_data))
demographic.credit.woe.data$marital.status <- woe_data



#====================== Generate the new Data files for further model building ===============

demographic.columns <- colnames(demographic)

write.csv(demographic,paste(wdpath,"data\\demographic.csv",sep = ""),row.names=F)
write.csv(demographic.credit,paste(wdpath,"data\\demographic_credit.csv",sep = ""),row.names=F)
write.csv(demographic.na, paste(wdpath,"data\\demographics_na.csv",sep = ""),row.names=F)
write.csv(demographic.credit.na, paste(wdpath,"data\\demographics_credit_na.csv",sep = ""),row.names=F)

demographic.woe.data <- demographic.credit.woe.data[,demographic.columns]
write.csv(demographic.woe.data,paste(wdpath,"data\\demographic_woe_data.csv",sep = ""),row.names=F)
write.csv(demographic.credit.woe.data ,paste(wdpath,"data\\demographic_credit_woe_data.csv",sep = ""),row.names=F)



