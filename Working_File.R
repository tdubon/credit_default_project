cat("\n","------------------ import data --------------","\n")

# Install packages
install.packages('stargazer',dependencies=TRUE)
install.packages('corrplot', dependencies=TRUE)
install.packages('dplyr')
install.packages('rbin', dependencies = TRUE)
install.packages('OneR',dependencies=TRUE)
install.packages('woeBinning',dependencies=TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('car', dependencies=TRUE)

# Load packages
library(stargazer)
library(tidyverse)
library(moments)
library(corrplot)
library(car)
library('OneR')
library('woeBinning')
library('ggplot2')
library('dplyr')
library('rbin')

# Define path and file;
# You will need to change the path value to match the location on your own computer;
my.path <- '/Users/tdubon/Documents/Northwestern/MSDS_498/Project_Data/';
my.file <- paste(my.path,'credit_card_default.RData', sep='');

# Read the RData object using readRDS();
credit_card_default <- readRDS(my.file)

# Copy original dataframe
cc_default_df <- credit_card_default
 
table(cc_default_df$train)
table(cc_default_df$test)
table(cc_default_df$validate)




cat("\n","------------------ DATA QUALITY CHECK ON RAW VARIABLES --------------","\n")

# Show dataframe structure & preliminary summary stats;
str(cc_default_df)
summary(cc_default_df)
head(cc_default_df)

# categorical data to be treated as factors 
cc_default_df[c(3:5, 7:12, 25, 27:30)]<-lapply(cc_default_df[c(3:5, 7:12, 25, 27:30)], factor)

# missing values
sapply(cc_default_df, function(x) sum(is.na(x)))

# Rename PAY_0 TO PAY_1
names(cc_default_df)[7] <- "PAY_1"


# apply stargazer to study and summarize continuous variables for data quality check
# define output path
out.path <- '/Users/tdubon/Documents/Northwestern/MSDS_498/Project_Data';

# summary stat table
file.name <- 'Stat_Table.html';
stargazer(cc_default_df, type=c('html'),out=paste(out.path,file.name,sep=''),
          title=c('Table 1: Summary Statistics for Default of Credit Card Clients Data'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, median=TRUE)


# summary of factor variables
table(cc_default_df$SEX)
table(cc_default_df$EDUCATION)
table(cc_default_df$MARRIAGE)
table(cc_default_df$PAY_1)
table(cc_default_df$PAY_2)
table(cc_default_df$PAY_3)
table(cc_default_df$PAY_4)
table(cc_default_df$PAY_5)
table(cc_default_df$PAY_6)

cat("\n","------------------ TRAIN DATA SPLIT for WOE BIN --------------","\n")

train_df <- cc_default_df[cc_default_df$train==1, 2:25]
n.train <- dim(train_df)[1] # 15,180
count_if('1', cc_default_df$train) #confirms train_df matches original


cat("\n","------------------ FEATURE ENGINEERING --------------","\n")

# AGE WOE BIN 
woe.binning(df=train_df,target.var=c('DEFAULT'),pred.var=c('AGE')) #USED TRAINING DATA TO DETERMINE BINS
# AGE: 0-24, 25-32, 33-INF

# RECODE BINNED AGE  
cc_default_df$AGE_0_24 <- ifelse(cc_default_df$AGE<=24, '1','0')
cc_default_df$AGE_25_32 <- ifelse(cc_default_df$AGE>=25 & cc_default_df$AGE<=32, '1','0')
cc_default_df$AGE_33_80 <- ifelse(cc_default_df$AGE>=33, '1','0')

table(cc_default_df$AGE_0_24)
table(cc_default_df$AGE_25_32)
table(cc_default_df$AGE_33_80)

cc_default_df$AGE_0_24 <- as.factor(cc_default_df$AGE_0_24)
cc_default_df$AGE_25_32 <- as.factor(cc_default_df$AGE_25_32)
cc_default_df$AGE_33_80 <- as.factor(cc_default_df$AGE_33_80)

# recode EDUCATION: 0, 5 & 6 -> 4 "OTHERS"
table(cc_default_df$EDUCATION)
cc_default_df$EDUCATION <- as.factor(cc_default_df$EDUCATION)

cc_default_df$EDUCATION <- recode_factor(cc_default_df$EDUCATION, '0' = '4')
cc_default_df$EDUCATION <- recode_factor(cc_default_df$EDUCATION, '5' ='4')
cc_default_df$EDUCATION <- recode_factor(cc_default_df$EDUCATION, '6' ='4')

cc_default_df$EDUCATION <- factor(cc_default_df$EDUCATION, levels= c('1','2', '3', '4'))
table(cc_default_df$EDUCATION)


# recode MARRIAGE: 0 -> 3 "OTHERS"
table(cc_default_df$MARRIAGE)
cc_default_df$MARRIAGE <- recode_factor(cc_default_df$MARRIAGE, '0' ='3')
table(cc_default_df$MARRIAGE)
cc_default_df$MARRIAGE <- factor(cc_default_df$MARRIAGE, levels= c('1','2', '3'))


# recode PAY_1: -2 -> -1 "OTHERS"
# cc_default_df$PAY_1 <- credit_card_default$PAY_0

cc_default_df$PAY_1 <- as.integer(cc_default_df$PAY_1)
cc_default_df$PAY_1[cc_default_df$PAY_1 <= -2] <- -1
table(cc_default_df$PAY_1)


cc_default_df$PAY_2 <- as.integer(cc_default_df$PAY_2)
table(cc_default_df$PAY_2)
cc_default_df$PAY_2[cc_default_df$PAY_2 <= -2] <- -1
table(cc_default_df$PAY_2)

cc_default_df$PAY_3 <- credit_card_default$PAY_3
cc_default_df$PAY_3 <- as.integer(cc_default_df$PAY_3)
table(cc_default_df$PAY_3)
cc_default_df$PAY_3[cc_default_df$PAY_3 <= -2] <- -1
table(cc_default_df$PAY_3)

cc_default_df$PAY_4 <- credit_card_default$PAY_4
cc_default_df$PAY_4 <- as.integer(cc_default_df$PAY_4)
table(cc_default_df$PAY_4)
cc_default_df$PAY_4[cc_default_df$PAY_4 <= -2] <- -1
table(cc_default_df$PAY_4)

cc_default_df$PAY_5 <- credit_card_default$PAY_5
cc_default_df$PAY_5 <- as.integer(cc_default_df$PAY_5)
table(cc_default_df$PAY_5)
cc_default_df$PAY_5[cc_default_df$PAY_5 <= -2] <- -1
table(cc_default_df$PAY_5)

cc_default_df$PAY_6 <- credit_card_default$PAY_6
cc_default_df$PAY_6 <- as.integer(cc_default_df$PAY_6)
table(cc_default_df$PAY_6)
cc_default_df$PAY_6[cc_default_df$PAY_6 <= -2] <- -1
table(cc_default_df$PAY_6)

# calculate AVG_BILL_AMT
cc_default_df$Avg_Bill_Amt <- NA
cc_default_df$Avg_Bill_Amt <- apply(cc_default_df[13:18], 1, mean)

# calculate AVG_PMT_AMT
cc_default_df$Avg_Pmt_Amt <- NA
cc_default_df$Avg_Pmt_Amt <- apply(cc_default_df[19:24], 1, mean)
head(cc_default_df$Avg_Pmt_Amt)

# set type for updated variables
table(cc_default_df$PAY_1)
cc_default_df$PAY_1 <- as.factor(cc_default_df$PAY_1)
table(cc_default_df$PAY_1)

cc_default_df$PAY_2 <- as.factor(cc_default_df$PAY_2)
table(cc_default_df$PAY_2)
cc_default_df$PAY_3 <- as.factor(cc_default_df$PAY_3)
table(cc_default_df$PAY_3)
cc_default_df$PAY_4 <- as.factor(cc_default_df$PAY_4)
table(cc_default_df$PAY_4)
cc_default_df$PAY_5 <- as.factor(cc_default_df$PAY_5)
table(cc_default_df$PAY_5)
cc_default_df$PAY_6 <- as.factor(cc_default_df$PAY_6)
table(cc_default_df$PAY_6)



# create Pmt_Ratio function

ratio <- function(pay_var, bill_var, new_var){
  new_var <- NA
  new_var <- as.numeric(new_var)
  new_var <- pay_var/bill_var
  new_var[pay_var == 0 & bill_var == 0] <- 100
  new_var[pay_var == 0 & bill_var != 0] <- 0
  new_var[pay_var != 0 & bill_var == 0] <- 1
  return(new_var)
}

# Ratio_1
cc_default_df$Pmt_Ratio1 <- ratio(pay_var=cc_default_df$PAY_AMT1, bill_var=cc_default_df$BILL_AMT2, new_var=cc_default_df$Pmt_Ratio1)

# Ratio_2
cc_default_df$Pmt_Ratio2 <- ratio(pay_var=cc_default_df$PAY_AMT2, bill_var=cc_default_df$BILL_AMT3, new_var=cc_default_df$Pmt_Ratio2)

# Ratio_3
cc_default_df$Pmt_Ratio3 <- ratio(pay_var=cc_default_df$PAY_AMT3, bill_var=cc_default_df$BILL_AMT4, new_var=cc_default_df$Pmt_Ratio3)

# Ratio_4
cc_default_df$Pmt_Ratio4 <- ratio(pay_var=cc_default_df$PAY_AMT4, bill_var=cc_default_df$BILL_AMT5, new_var=cc_default_df$Pmt_Ratio4)

# Ratio_5
cc_default_df$Pmt_Ratio5 <- ratio(pay_var=cc_default_df$PAY_AMT5, bill_var=cc_default_df$BILL_AMT6, new_var=cc_default_df$Pmt_Ratio5)



# check variables
count_if('Inf', cc_default_df$Pmt_Ratio1)
count_if('NaN', cc_default_df$Pmt_Ratio1)
count_if('0', cc_default_df$Pmt_Ratio1)
summary(cc_default_df$Pmt_Ratio1)

count_if('Inf', cc_default_df$Pmt_Ratio2)
count_if('NaN', cc_default_df$Pmt_Ratio2)
count_if('0', cc_default_df$Pmt_Ratio2)
summary(cc_default_df$Pmt_Ratio2)

count_if('Inf', cc_default_df$Pmt_Ratio3)
count_if('NaN', cc_default_df$Pmt_Ratio3)
count_if('0', cc_default_df$Pmt_Ratio3)
summary(cc_default_df$Pmt_Ratio3)

count_if('Inf', cc_default_df$Pmt_Ratio4)
count_if('NaN', cc_default_df$Pmt_Ratio4)
count_if('0', cc_default_df$Pmt_Ratio4)
summary(cc_default_df$Pmt_Ratio4)

count_if('Inf', cc_default_df$Pmt_Ratio5)
count_if('NaN', cc_default_df$Pmt_Ratio5)
count_if('0', cc_default_df$Pmt_Ratio5)
summary(cc_default_df$Pmt_Ratio5)

count_if('Inf', cc_default_df$Pmt_Ratio6)
count_if('NaN', cc_default_df$Pmt_Ratio6)
count_if('0', cc_default_df$Pmt_Ratio6)
summary(cc_default_df$Pmt_Ratio6)


# calculate Avg_Pmt_Ratio
cc_default_df$Avg_Pmt_Ratio <- NA
cc_default_df$Avg_Pmt_Ratio <- apply(cc_default_df[36:40], 1, mean)
head(cc_default_df$Avg_Pmt_Ratio)


# Utilization
cc_default_df$Util1 <- NA
cc_default_df$Util2 <- NA
cc_default_df$Util3 <- NA
cc_default_df$Util4 <- NA
cc_default_df$Util5 <- NA
cc_default_df$Util6 <- NA

cc_default_df$Util1 <- ifelse(cc_default_df$BILL_AMT1==0, '0', cc_default_df$BILL_AMT1/cc_default_df$LIMIT_BAL)
cc_default_df$Util2 <- ifelse(cc_default_df$BILL_AMT2==0, '0', cc_default_df$BILL_AMT2/cc_default_df$LIMIT_BAL)
cc_default_df$Util3 <- ifelse(cc_default_df$BILL_AMT3==0, '0', cc_default_df$BILL_AMT3/cc_default_df$LIMIT_BAL)
cc_default_df$Util4 <- ifelse(cc_default_df$BILL_AMT4==0, '0', cc_default_df$BILL_AMT4/cc_default_df$LIMIT_BAL)
cc_default_df$Util5 <- ifelse(cc_default_df$BILL_AMT5==0, '0', cc_default_df$BILL_AMT5/cc_default_df$LIMIT_BAL)
cc_default_df$Util6 <- ifelse(cc_default_df$BILL_AMT6==0, '0', cc_default_df$BILL_AMT6/cc_default_df$LIMIT_BAL)

cc_default_df$Util1 <- as.numeric(cc_default_df$Util1)
cc_default_df$Util2 <- as.numeric(cc_default_df$Util2)
cc_default_df$Util3 <- as.numeric(cc_default_df$Util3)
cc_default_df$Util4 <- as.numeric(cc_default_df$Util4)
cc_default_df$Util5 <- as.numeric(cc_default_df$Util5)
cc_default_df$Util6 <- as.numeric(cc_default_df$Util6)

count_if('0', cc_default_df$Util1)

# calculate Avg_Util
cc_default_df$Avg_Util <- apply(cc_default_df[42:47], 1, mean)
head(cc_default_df$Avg_Util)

# Balance Growth Over 6 Months - calculate month to month difference 
cc_default_df$Bal_Chx1 <- NA
cc_default_df$Bal_Chx2 <- NA
cc_default_df$Bal_Chx3 <- NA
cc_default_df$Bal_Chx4 <- NA
cc_default_df$Bal_Chx5 <- NA

cc_default_df$Bal_Chx1 <- (cc_default_df$BILL_AMT2 - cc_default_df$BILL_AMT1) #Calc Bill2-Bill1
cc_default_df$Bal_Chx2 <- (cc_default_df$BILL_AMT3 - cc_default_df$BILL_AMT2) #Calc Bill3-Bill2
cc_default_df$Bal_Chx3 <- (cc_default_df$BILL_AMT4 - cc_default_df$BILL_AMT3) #Calc Bill4-Bill3
cc_default_df$Bal_Chx4 <- (cc_default_df$BILL_AMT5 - cc_default_df$BILL_AMT4) #Calc Bill5-Bill4
cc_default_df$Bal_Chx5 <- (cc_default_df$BILL_AMT6 - cc_default_df$BILL_AMT5) #Calc Bill6-Bill5

# Balance Growth Over 6 Months
cc_default_df$Bal_Growth_6mo <- NA
cc_default_df$Bal_Growth_6mo <- apply(cc_default_df[49:53], 1, sum)

plot(cc_default_df$Bal_Growth_6mo)


#Utilization Growth Over 6 Months
cc_default_df$Util_Chx1 <- NA
cc_default_df$Util_Chx2 <- NA
cc_default_df$Util_Chx3 <- NA
cc_default_df$Util_Chx4 <- NA
cc_default_df$Util_Chx5 <- NA

cc_default_df$Util_Chx1 <- (cc_default_df$Util2 - cc_default_df$Util1) #Calc Util2-Util1
cc_default_df$Util_Chx2 <- (cc_default_df$Util3 - cc_default_df$Util2) #Calc Util3-Util2
cc_default_df$Util_Chx3 <- (cc_default_df$Util4 - cc_default_df$Util3) #Calc Util4-Util3
cc_default_df$Util_Chx4 <- (cc_default_df$Util5 - cc_default_df$Util4) #Calc Util5-Util4
cc_default_df$Util_Chx5 <- (cc_default_df$Util6 - cc_default_df$Util5) #Calc Util6-Util5

# Utilization Growth Over 6 Months
cc_default_df$Util_Growth_6mo <- NA
cc_default_df$Util_Growth_6mo <- apply(cc_default_df[55:59], 1, sum)

plot(cc_default_df$Util_Growth_6mo)


# Max Bill Amount
cc_default_df$Max_Bill_Amt <- apply(cc_default_df[13:18], 1, max)

# Max Payment Amount
cc_default_df$Max_Pmt_Amt <- apply(cc_default_df[19:24], 1, max)

#Max Delinquency
cc_default_df$Max_DLQ <- NA
cc_default_df$PAY_1[cc_default_df$PAY_1 == '-1'] <- '0'
cc_default_df$PAY_2[cc_default_df$PAY_2 == '-1'] <- '0'
cc_default_df$PAY_3[cc_default_df$PAY_3 == '-1'] <- '0'
cc_default_df$PAY_4[cc_default_df$PAY_4 == '-1'] <- '0'
cc_default_df$PAY_5[cc_default_df$PAY_5 == '-1'] <- '0'
cc_default_df$PAY_6[cc_default_df$PAY_6 == '-1'] <- '0'

cc_default_df$Max_DLQ <- apply(cc_default_df[7:12], 1, max)
cc_default_df$Max_DLQ <- as.numeric(cc_default_df$Max_DLQ)


cat("\n","------------------ SPLIT DATA --------------","\n")

train_df <- cc_default_df[cc_default_df$train == 1, c(3:5, 25, 31:63)] #split keeping only wanted col
n.train <- dim(train_df)[1] # 15,180
count_if('1', cc_default_df$train) #confirms train_df matches original




cat("\n","------------------ EDA ON ENGINEERED VARIABLES --------------","\n")


# summary stat table
file.name <- 'Stat_Table.html';
stargazer(train_df, type=c('html'),out=paste(out.path,file.name,sep=''),
          title=c('Table 1: Summary Statistics for Default of Credit Card Clients Data'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, median=TRUE)


# summary of factor variables
table(train_df$SEX)
table(train_df$EDUCATION)
table(train_df$MARRIAGE)
table(train_df$AGE_0_24)
table(train_df$AGE_25_32)
table(train_df$AGE_33_80)

###### Continuous variables
# graphs of continuous variables

par(mfrow=c(1,1))
hist(train_df$Avg_Bill_Amt, xlab = '',  main="Histogram of Avg_Bill_Amt", col="blue")
boxplot(train_df$Avg_Bill_Amt ~ train_df$DEFAULT ,xlab = '',main="Boxplot of Avg_Bill_Amt",col="blue")
quantile(train_df$Avg_Bill_Amt, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 

round(skewness(train_df$Avg_Bill_Amt),2)
round(kurtosis(train_df$Avg_Bill_Amt),2)

#------------------------
hist(train_df$Avg_Pmt_Amt, xlab = '',  main="Histogram of Avg_Pmt_Amt", col="blue")
boxplot(train_df$Avg_Pmt_Amt,xlab = '',main="Boxplot of Avg_Pmt_Amt",col="blue")
quantile(train_df$Avg_Pmt_Amt, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 

round(skewness(train_df$Avg_Pmt_Amt),2)
round(kurtosis(train_df$Avg_Pmt_Amt),2)


#------------------------
hist(train_df$Avg_Pmt_Ratio, xlab = '',  main="Histogram of Avg_Pmt_Ratio", col="blue")
boxplot(train_df$Avg_Pmt_Ratio,xlab = '',main="Boxplot of Avg_Pmt_Ratio",col="blue")
quantile(train_df$Avg_Pmt_Ratio, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 

round(skewness(train_df$Avg_Pmt_Ratio),2)
round(kurtosis(train_df$Avg_Pmt_Ratio),2)


#------------------------
hist(train_df$Avg_Util, xlab = '',  main="Histogram of Avg_Util", col="blue")
boxplot(train_df$Avg_Util,xlab = '',main="Boxplot of Avg_Util",col="blue")
quantile(train_df$Avg_Util, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 

round(skewness(train_df$Avg_Util),2)
round(kurtosis(train_df$Avg_Util),2)


#------------------------
hist(train_df$Bal_Growth_6mo, xlab = '',  main="Histogram of Bal_Growth_6mo", col="blue")
boxplot(train_df$Bal_Growth_6mo,xlab = '',main="Boxplot of Bal_Growth_6mo",col="blue")
quantile(train_df$Bal_Growth_6mo, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 

round(skewness(train_df$Bal_Growth_6mo),2)
round(kurtosis(train_df$Bal_Growth_6mo),2)



#------------------------
hist(train_df$Util_Growth_6mo, xlab = '',  main="Histogram of Util_Growth_6mo", col="blue")
boxplot(train_df$Util_Growth_6mo,xlab = '',main="Boxplot of Util_Growth_6mo",col="blue")
quantile(train_df$Util_Growth_6mo, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 

round(skewness(train_df$Util_Growth_6mo),2)
round(kurtosis(train_df$Util_Growth_6mo),2)


#------------------------
hist(train_df$Max_Bill_Amt, xlab = '',  main="Histogram of Max_Bill_Amt", col="blue")
hist(train_df$Max_Pmt_Amt, xlab = '',  main="Histogram of Max_Pmt_Amt", col="blue")
hist(train_df$Max_DLQ, xlab = '',  main="Histogram of Max_DLQ", col="blue")




# #------------------------graphs of factor variables

ggplot(train_df, aes(SEX))+
  geom_bar(aes(fill=SEX), width = 0.5) + 
  theme(axis.text.x = element_text(angle=90, vjust=0)) +
  labs(title="BAR CHART OF SEX VARIABLE")   


ggplot(train_df, aes(EDUCATION))+
  geom_bar(aes(fill=EDUCATION), width = 0.5) + 
  theme(axis.text.x = element_text(angle=90, vjust=0)) +
  labs(title="BAR CHART OF EDUCATION VARIABLE")   

ggplot(train_df, aes(MARRIAGE))+
  geom_bar(aes(fill=MARRIAGE), width = 0.5) + 
  theme(axis.text.x = element_text(angle=90, vjust=0)) +
  labs(title="BAR CHART OF MARRIAGE VARIABLE")  

ggplot(train_df, aes(DEFAULT))+
  geom_bar(aes(fill=DEFAULT), width = 0.5) + 
  theme(axis.text.x = element_text(angle=90, vjust=0)) +
  labs(title="BAR CHART OF DEFAULT")  



cat("\n","------------------ Variable selection --------------","\n")


# Correlation Table
par(mfrow=c(1,1))
corr=cor(train_df[,c(8, 9, 15, 22, 28, 34, 35:37)])
corrplot(corr,method="color", outline=T, cl.pos="n", rect.col="black", 
         tl.col="indianred4", addCoef.col="black", number.digits=2, number.cex=0.60, 
         tl.cex=0.7, cl.cex=1, col=colorRampPalette(c("green4", "white", "red"))(100))

# produce list of correlations by highest value
corr[lower.tri(corr,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
corr=as.data.frame(as.table(corr))  #Turn into a 3-column table
corr=na.omit(corr)  #Get rid of the junk we flagged above
corr=corr[order(-abs(corr$Freq)),]    #Sort by highest correlation (whether +ve or -ve)
corr

table(cc_default_df$train)
table(cc_default_df$test)
table(cc_default_df$validate)

# ------------- OneR

# engineered features
model.1 <- OneR(DEFAULT ~ SEX + EDUCATION + MARRIAGE +
                  AGE_0_24 + AGE_25_32 + AGE_33_80 + 
                  Avg_Bill_Amt + Avg_Pmt_Amt + Avg_Pmt_Ratio + Avg_Util +
                  Bal_Growth_6mo + Util_Growth_6mo + Max_Bill_Amt + 
                  Max_Pmt_Amt + Max_DLQ, data=train_df, verbose=TRUE);
#only 78% accuracy - Max_DLQ is id as significant
summary(model.1)

# ------------- Random Forest
# most important variable using random forests for class variables

library('tree')
tree_train_df =tree(DEFAULT~.,train_df)
summary(tree_train_df)
par(mfrow=c(1,1))
plot(tree_train_df)
text(tree_train_df, pretty=0)
tree_train_df
#most impt variables: Max_DLQ, Max_Bill_Amt

table(train_df$DEFAULT)

