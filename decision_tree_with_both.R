wdpath <- "E:\\Arif\\Work\\Data Science Course\\Upgrad PGDDS\\Domain Elective\\BFSI\\Capstone Project\\"
setwd(wdpath)
library(rpart)
library(rpart.plot)
library(caTools)
library(rattle)
library(AUC)
library(caret)


#============================ Model Building Only demographics Data =========================

#======== Step: 1 =======
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
train.demographic.woe.data$status <- as.factor(train.demographic.woe.data$status)

str(train.demographic.woe.data)

# model.1

model.1 <-
  rpart(status~. , data= train.demographic.woe.data, control = 
                   rpart.control(minsplit = 10, cp =0.0002),parms = list(split="information" ))
model.1$cptable
prp(model.1)



#================== Build model using demographic and credit bureau data ====================

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


# our data set is biased. It might create problem in training model, since it will be biased. 
prop.table(table(train.demographic.credit.woe.data$status))

# model.1

model.1 <- rpart(status~., data = train.demographic.credit.woe.data,
                 control = rpart.control(minsplit = 28, cp = 0.01),method = "class")
model.1
model.1$cptable

# there is only one root in the tree. 
# try small cp value and minsplit value.

model.1 <- rpart(status~., data = train.demographic.credit.woe.data,
                 control = rpart.control(minsplit = 28, cp = 0.0001),method = "class")
model.1
model.1$cptable
printcp(model.1)

# as per cptable there is no improvement at cp = 0.00011, hence it will not cplit futher
# and when we prun the tree to reduce overfitting it will show only on root.




