cat("\n","------------------ import data --------------","\n")

# Install packages
#install.packages('stargazer',dependencies=TRUE)
#install.packages('corrplot', dependencies=TRUE)
#install.packages('dplyr')
#install.packages('rbin', dependencies = TRUE)
#install.packages('OneR',dependencies=TRUE)
#install.packages('woeBinning',dependencies=TRUE)
#install.packages('ggplot2', dependencies = TRUE)
#install.packages('car', dependencies=TRUE)

# Load packages
library(stargazer)
library(tidyverse)
library(moments)
library(corrplot)
library(car)
library(OneR)
library(woeBinning)
library(ggplot2)
library(dplyr)
library(rbin)
library(expss)

library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)
library(caret)	
library(ROCR)

# Define path and file;
# You will need to change the path value to match the location on your own computer;
my.path <- '/Users/tdubon/Documents/Northwestern/MSDS_498/Project_Data/';
my.file <- paste(my.path,'credit_card_default.RData', sep='');

# Read the RData object using readRDS();
credit_card_default <- readRDS(my.file)

# Copy original dataframe
cc_default_df <- credit_card_default
 
head(cc_default_df)



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

#-------------------------------------------------LIMIT_BAL WOE BIN 
woe.binning(df=train_df,target.var=c('DEFAULT'),pred.var=c('LIMIT_BAL')) #USED TRAINING DATA TO DETERMINE BINS
# (-Inf,30,000] 
# (30,000,160,000] 
# (160,000, Inf] 

summary(cc_default_df$LIMIT_BAL)

# RECODE BINNED LIMIT_BAL 
cc_default_df$LIMIT_BAL_Neg_29999 <- ifelse(cc_default_df$LIMIT_BAL<30000, '1','0')
cc_default_df$LIMIT_BAL_30000_159999 <- ifelse(cc_default_df$LIMIT_BAL>=30000 & cc_default_df$LIMIT_BAL<160000, '1','0')
cc_default_df$LIMIT_BAL_160000_1000000 <- ifelse(cc_default_df$LIMIT_BAL>=160000 & cc_default_df$LIMIT_BAL<=1000000, '1','0')

table(cc_default_df$LIMIT_BAL_Neg_29999)
table(cc_default_df$LIMIT_BAL_30000_159999)
table(cc_default_df$LIMIT_BAL_160000_1000000) #binned limit_bal=1 totals from all bins add to 30k


cc_default_df$LIMIT_BAL_Neg_29999 <- as.factor(cc_default_df$LIMIT_BAL_Neg_29999)
cc_default_df$LIMIT_BAL_30000_159999 <- as.factor(cc_default_df$LIMIT_BAL_30000_159999)
cc_default_df$LIMIT_BAL_160000_1000000 <- as.factor(cc_default_df$LIMIT_BAL_160000_1000000)



#---------------------------------------------AGE WOE BIN 
woe.binning(df=train_df,target.var=c('DEFAULT'),pred.var=c('AGE')) #USED TRAINING DATA TO DETERMINE BINS
# AGE: 0-25, 25-33, 33-INF

#----------------------------------------------------------------RECODE BINNED AGE  
cc_default_df$AGE_0_24 <- ifelse(cc_default_df$AGE<25, '1','0')
cc_default_df$AGE_25_32 <- ifelse(cc_default_df$AGE>=25 & cc_default_df$AGE<33, '1','0')
cc_default_df$AGE_33_80 <- ifelse(cc_default_df$AGE>=33, '1','0')

table(cc_default_df$AGE_0_24)
table(cc_default_df$AGE_25_32)
table(cc_default_df$AGE_33_80) #binned AGE=1 totals from all bins add to 30k

cc_default_df$AGE_0_24 <- as.factor(cc_default_df$AGE_0_24)
cc_default_df$AGE_25_32 <- as.factor(cc_default_df$AGE_25_32)
cc_default_df$AGE_33_80 <- as.factor(cc_default_df$AGE_33_80)

#---------------------------------------------------------------recode EDUCATION: 0, 5 & 6 -> 4 "OTHERS"
table(cc_default_df$EDUCATION)
cc_default_df$EDUCATION <- as.factor(cc_default_df$EDUCATION)

cc_default_df$EDUCATION <- recode_factor(cc_default_df$EDUCATION, '0' = '4')
cc_default_df$EDUCATION <- recode_factor(cc_default_df$EDUCATION, '5' ='4')
cc_default_df$EDUCATION <- recode_factor(cc_default_df$EDUCATION, '6' ='4')

cc_default_df$EDUCATION <- factor(cc_default_df$EDUCATION, levels= c('1','2', '3', '4'))
table(cc_default_df$EDUCATION) #new total for 4: 468


#---------------------------------------------------------------recode MARRIAGE: 0 -> 3 "OTHERS"
table(cc_default_df$MARRIAGE)
cc_default_df$MARRIAGE <- recode_factor(cc_default_df$MARRIAGE, '0' ='3')
table(cc_default_df$MARRIAGE)
cc_default_df$MARRIAGE <- factor(cc_default_df$MARRIAGE, levels= c('1','2', '3'))


#---------------------------------------------------------------recode PAY variables: 0, -2 -> -1 "OTHERS"
#cc_default_df$PAY_1 <- credit_card_default$PAY_0
table(cc_default_df$PAY_1)
#cc_default_df$PAY_1 <- as.factor(cc_default_df$PAY_1)
cc_default_df$PAY_1 <- recode_factor(cc_default_df$PAY_1, '0' ='-1')
cc_default_df$PAY_1 <- recode_factor(cc_default_df$PAY_1, '-2' ='-1')
table(cc_default_df$PAY_1)


table(cc_default_df$PAY_2)
cc_default_df$PAY_2 <- recode_factor(cc_default_df$PAY_2, '0' ='-1')
cc_default_df$PAY_2 <- recode_factor(cc_default_df$PAY_2, '-2' ='-1')
table(cc_default_df$PAY_2)


table(cc_default_df$PAY_3)
cc_default_df$PAY_3 <- recode_factor(cc_default_df$PAY_3, '0' ='-1')
cc_default_df$PAY_3 <- recode_factor(cc_default_df$PAY_3, '-2' ='-1')
table(cc_default_df$PAY_3)


table(cc_default_df$PAY_4)
cc_default_df$PAY_4 <- recode_factor(cc_default_df$PAY_4, '0' ='-1')
cc_default_df$PAY_4 <- recode_factor(cc_default_df$PAY_4, '-2' ='-1')
table(cc_default_df$PAY_4)


table(cc_default_df$PAY_5)
cc_default_df$PAY_5 <- recode_factor(cc_default_df$PAY_5, '0' ='-1')
cc_default_df$PAY_5 <- recode_factor(cc_default_df$PAY_5, '-2' ='-1')
table(cc_default_df$PAY_5)


table(cc_default_df$PAY_6)
cc_default_df$PAY_6 <- recode_factor(cc_default_df$PAY_6, '0' ='-1')
cc_default_df$PAY_6 <- recode_factor(cc_default_df$PAY_6, '-2' ='-1')
table(cc_default_df$PAY_6)


#--------------------------------------------------------calculate AVG_BILL_AMT
str(cc_default_df)
cc_default_df$Avg_Bill_Amt <- NA
cc_default_df$Avg_Bill_Amt <- apply(cc_default_df[13:18], 1, mean)

summary(cc_default_df$Avg_Bill_Amt)


train_df <- cc_default_df[cc_default_df$train==1, c(2:25, 37)]
str(train_df)
# AVG_BILL_AMT WOE BIN 
woe.binning(df=train_df,target.var=c('DEFAULT'),pred.var=c('Avg_Bill_Amt')) #USED TRAINING DATA TO DETERMINE BINS
# (-Inf,189.9916667]        
# (189.9916667,2847.1]       
# (2847.1,7488.683333]       
# (7488.683333,31916.93333]  
# (31916.93333, Inf] 

# RECODE BINNED AVG_BILL_AMT  
cc_default_df$Avg_Bill_Amt_Neg_56050_188 <- ifelse(cc_default_df$Avg_Bill_Amt<189, '1','0')
cc_default_df$Avg_Bill_Amt_189_2846 <- ifelse(cc_default_df$Avg_Bill_Amt>=189 & cc_default_df$Avg_Bill_Amt<2847, '1','0')
cc_default_df$Avg_Bill_Amt_2847_7488 <- ifelse(cc_default_df$Avg_Bill_Amt>=2847 & cc_default_df$Avg_Bill_Amt<7489, '1','0')
cc_default_df$Avg_Bill_Amt_7489_31915 <- ifelse(cc_default_df$Avg_Bill_Amt>=7489 & cc_default_df$Avg_Bill_Amt<31916, '1','0')
cc_default_df$Avg_Bill_Amt_31916_900000 <- ifelse(cc_default_df$Avg_Bill_Amt>=31916 & cc_default_df$Avg_Bill_Amt<=900000, '1','0')

table(cc_default_df$Avg_Bill_Amt_Neg_56050_188)
table(cc_default_df$Avg_Bill_Amt_189_2846)
table(cc_default_df$Avg_Bill_Amt_2847_7488)
table(cc_default_df$Avg_Bill_Amt_7489_31915)
table(cc_default_df$Avg_Bill_Amt_31916_900000)

cc_default_df$Avg_Bill_Amt_Neg_56050_188 <- as.factor(cc_default_df$Avg_Bill_Amt_Neg_56050_188)
cc_default_df$Avg_Bill_Amt_189_2846 <- as.factor(cc_default_df$Avg_Bill_Amt_189_2846)
cc_default_df$Avg_Bill_Amt_2847_7488 <- as.factor(cc_default_df$Avg_Bill_Amt_2847_7488)
cc_default_df$Avg_Bill_Amt_7489_31915 <- as.factor(cc_default_df$Avg_Bill_Amt_7489_31915)
cc_default_df$Avg_Bill_Amt_31916_900000 <- as.factor(cc_default_df$Avg_Bill_Amt_31916_900000)



#--------------------------------------------------calculate AVG_PMT_AMT
cc_default_df$Avg_Pmt_Amt <- NA
cc_default_df$Avg_Pmt_Amt <- apply(cc_default_df[19:24], 1, mean)
head(cc_default_df$Avg_Pmt_Amt)
summary(cc_default_df$Avg_Pmt_Amt)


train_df <- cc_default_df[cc_default_df$train==1, c(2:25, 37, 43)]
str(train_df)

# Avg_Pmt_Amt WOE BIN 
woe.binning(df=train_df,target.var=c('DEFAULT'),pred.var=c('Avg_Pmt_Amt')) #USED TRAINING DATA TO DETERMINE BINS
#(-Inf,2832.65] 
#(2832.65,12092.5] 
#(12092.5, Inf] 

cc_default_df$Avg_Pmt_Amt_0_2832 <- ifelse(cc_default_df$Avg_Pmt_Amt<2833, '1','0')
cc_default_df$Avg_Pmt_Amt_2833_12092 <- ifelse(cc_default_df$Avg_Pmt_Amt>=2833 & cc_default_df$Avg_Pmt_Amt<12093, '1','0')
cc_default_df$Avg_Pmt_Amt_12093_627344 <- ifelse(cc_default_df$Avg_Pmt_Amt>=12093 & cc_default_df$Avg_Pmt_Amt<627345, '1','0')

table(cc_default_df$Avg_Pmt_Amt_0_2832)
table(cc_default_df$Avg_Pmt_Amt_2833_12092)
table(cc_default_df$Avg_Pmt_Amt_12093_627344)

cc_default_df$Avg_Pmt_Amt_0_2832 <- as.factor(cc_default_df$Avg_Pmt_Amt_0_2832)
cc_default_df$Avg_Pmt_Amt_2833_12092 <- as.factor(cc_default_df$Avg_Pmt_Amt_2833_12092)
cc_default_df$Avg_Pmt_Amt_12093_627344 <- as.factor(cc_default_df$Avg_Pmt_Amt_12093_627344)

str(cc_default_df)

#------------------------------------------calculate Avg_Pmt_Ratio

#-------------------calculate ratios
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



str(cc_default_df)
#----------------------------------------------calculate average ratio
cc_default_df$Avg_Pmt_Ratio <- NA
cc_default_df$Avg_Pmt_Ratio <- apply(cc_default_df[47:51], 1, mean)
summary(cc_default_df$Avg_Pmt_Ratio)

train_df <- cc_default_df[cc_default_df$train==1, c(2:25, 37, 43, 52)]
str(train_df)

#---------------------------------------------Bin Avg_Pmt_Ratio 
woe.binning(df=train_df,target.var=c('DEFAULT'),pred.var=c('Avg_Pmt_Ratio')) #USED TRAINING DATA TO DETERMINE BINS
#(-Inf,0.03406552305]  
#(0.03406552305,0.1583070591]
#(0.1583070591,1.000163684] 
#(1.000163684,1.17911605] 
#(1.17911605, Inf] 

cc_default_df$Avg_Pmt_Ratio_Neg16429_pt02 <- ifelse(cc_default_df$Avg_Pmt_Ratio<0.03406552305, '1','0')
cc_default_df$Avg_Pmt_Ratio_pt03_pt15 <- ifelse(cc_default_df$Avg_Pmt_Ratio>=0.03406552305 & cc_default_df$Avg_Pmt_Ratio<0.1583070591, '1','0')
cc_default_df$Avg_Pmt_Ratio_pt16_1 <- ifelse(cc_default_df$Avg_Pmt_Ratio>=0.1583070591 & cc_default_df$Avg_Pmt_Ratio<1.000163684, '1','0')
cc_default_df$Avg_Pmt_Ratio_1_1pt17 <- ifelse(cc_default_df$Avg_Pmt_Ratio>=1.000163684 & cc_default_df$Avg_Pmt_Ratio<1.17911605, '1','0')
cc_default_df$Avg_Pmt_Ratio_1pt18_2688 <- ifelse(cc_default_df$Avg_Pmt_Ratio>=1.17911605 & cc_default_df$Avg_Pmt_Ratio<2688, '1','0')

table(cc_default_df$Avg_Pmt_Ratio_Neg16429_pt02)
table(cc_default_df$Avg_Pmt_Ratio_pt03_pt15)
table(cc_default_df$Avg_Pmt_Ratio_pt16_1)
table(cc_default_df$Avg_Pmt_Ratio_1_1pt17)
table(cc_default_df$Avg_Pmt_Ratio_1pt18_2688)

cc_default_df$Avg_Pmt_Ratio_Neg16429_pt02 <- as.factor(cc_default_df$Avg_Pmt_Ratio_Neg16429_pt02)
cc_default_df$Avg_Pmt_Ratio_pt03_pt15 <- as.factor(cc_default_df$Avg_Pmt_Ratio_pt03_pt15)
cc_default_df$Avg_Pmt_Ratio_pt16_1 <- as.factor(cc_default_df$Avg_Pmt_Ratio_pt16_1)
cc_default_df$Avg_Pmt_Ratio_1_1pt17 <- as.factor(cc_default_df$Avg_Pmt_Ratio_1_1pt17)
cc_default_df$Avg_Pmt_Ratio_1pt18_2688 <- as.factor(cc_default_df$Avg_Pmt_Ratio_1pt18_2688)


str(cc_default_df)



#------------------------------------------------------------Calculate Avg_Util 
#----------------Calculate Utility
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

#--------------calculate average of utility
str(cc_default_df)
cc_default_df$Avg_Util <- apply(cc_default_df[58:63], 1, mean)
summary(cc_default_df$Avg_Util)

train_df <- cc_default_df[cc_default_df$train==1, c(2:25, 37, 43, 52, 64)]
str(train_df)

#-----------------Bin Avg_Util 
woe.binning(df=train_df,target.var=c('DEFAULT'),pred.var=c('Avg_Util')) #USED TRAINING DATA TO DETERMINE BINS
#(-Inf,0.0009562517806]           
#(0.0009562517806,0.009194422492]  
#(0.009194422492,0.3673418095]     
#(0.3673418095,0.8231566667]      
#(0.8231566667, Inf]    

cc_default_df$Avg_Util_negpt2326_pt00095 <- ifelse(cc_default_df$Avg_Util<0.0009562517806, '1','0')
cc_default_df$Avg_Util_pt00096_pt0091 <- ifelse(cc_default_df$Avg_Util>=0.0009562517806 & cc_default_df$Avg_Util<0.009194422492, '1','0')
cc_default_df$Avg_Util_pt0092_pt3673 <- ifelse(cc_default_df$Avg_Util>=0.009194422492 & cc_default_df$Avg_Util<0.3673418095, '1','0')
cc_default_df$Avg_Util_pt3674_pt8231 <- ifelse(cc_default_df$Avg_Util>=0.3673418095 & cc_default_df$Avg_Util<0.8231566667, '1','0')
cc_default_df$Avg_Util_pt8232_6 <- ifelse(cc_default_df$Avg_Util>=0.8231566667 & cc_default_df$Avg_Util<6, '1','0')

table(cc_default_df$Avg_Util_negpt2326_pt00095)
table(cc_default_df$Avg_Util_pt00096_pt0091)
table(cc_default_df$Avg_Util_pt0092_pt3673)
table(cc_default_df$Avg_Util_pt3674_pt8231)
table(cc_default_df$Avg_Util_pt8232_6)

cc_default_df$Avg_Util_negpt2326_pt00095 <- as.factor(cc_default_df$Avg_Util_negpt2326_pt00095)
cc_default_df$Avg_Util_pt00096_pt0091 <- as.factor(cc_default_df$Avg_Util_pt00096_pt0091)
cc_default_df$Avg_Util_pt0092_pt3673 <- as.factor(cc_default_df$Avg_Util_pt0092_pt3673)
cc_default_df$Avg_Util_pt3674_pt8231 <- as.factor(cc_default_df$Avg_Util_pt3674_pt8231)
cc_default_df$Avg_Util_pt8232_6 <- as.factor(cc_default_df$Avg_Util_pt8232_6)


str(cc_default_df)


#------------------------------------------------------------------Balance Growth Over 6 Months 
#------------------------calculate month to month difference 
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

#---------------------Balance Growth Over 6 Months
cc_default_df$Bal_Growth_6mo <- NA

cc_default_df$Bal_Growth_6mo <- apply(cc_default_df[70:74], 1, sum) #add the changes
summary(cc_default_df$Bal_Growth_6mo)
plot(cc_default_df$Bal_Growth_6mo)

str(cc_default_df)
train_df <- cc_default_df[cc_default_df$train==1, c(3:25, 37, 43, 52, 64, 75, 84)]
str(train_df)

#--------------------Bin Bal_Growth_6mo
woe.binning(df=train_df,target.var=c('DEFAULT'),pred.var=c('Bal_Growth_6mo')) #USED TRAINING DATA TO DETERMINE BINS
#(-Inf,-123]
#(-123,21389.95]
#(21389.95, Inf]    

summary(cc_default_df$Bal_Growth_6mo)

cc_default_df$Bal_Growth_6mo_neg708323_neg122 <- ifelse(cc_default_df$Bal_Growth_6mo < -123, '1','0')
cc_default_df$Bal_Growth_6mo_neg123_21389 <- ifelse(cc_default_df$Bal_Growth_6mo>= -123 & cc_default_df$Bal_Growth_6mo< 21389.95, '1','0')
cc_default_df$Bal_Growth_6mo_21390_428792 <- ifelse(cc_default_df$Bal_Growth_6mo>= 21389.95 & cc_default_df$Bal_Growth_6mo< 428792, '1','0')

table(cc_default_df$Bal_Growth_6mo_neg708323_neg122)
table(cc_default_df$Bal_Growth_6mo_neg123_21389)
table(cc_default_df$Bal_Growth_6mo_21390_428792)


cc_default_df$Bal_Growth_6mo_neg708323_neg122 <- as.factor(cc_default_df$Bal_Growth_6mo_neg708323_neg122)
cc_default_df$Bal_Growth_6mo_neg123_21389 <- as.factor(cc_default_df$Bal_Growth_6mo_neg123_21389)
cc_default_df$Bal_Growth_6mo_21390_428792 <- as.factor(cc_default_df$Bal_Growth_6mo_21390_428792)


str(cc_default_df)


#--------------------------------------------------Util_Growth_6mo
#------------------utility vectors
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

#-----------------Sum Utility changes
cc_default_df$Util_Growth_6mo <- NA
str(cc_default_df)
cc_default_df$Util_Growth_6mo <- apply(cc_default_df[79:83], 1, sum) #add the changes
summary(cc_default_df$Util_Growth_6mo)
plot(cc_default_df$Util_Growth_6mo)

#---------------bin Utilization Growth 
woe.binning(df=train_df,target.var=c('DEFAULT'),pred.var=c('Util_Growth_6mo')) #USED TRAINING DATA TO DETERMINE BINS
summary(train_df$Util_Growth_6mo)
#(-Inf,-0.72262]     
#(-0.72262,-0.00075] 
#(-0.00075,0] 
#(0,0.02930625]
#(0.02930625, Inf]

cc_default_df$Util_Growth_6mo_Neg5_NegPt7 <- ifelse(cc_default_df$Util_Growth_6mo < -0.72262, '1','0')
cc_default_df$Util_Growth_6mo_NegPt7_NegPt00075 <- ifelse(cc_default_df$Util_Growth_6mo >= -0.72262 & cc_default_df$Util_Growth_6mo < -0.00075, '1','0')
cc_default_df$Util_Growth_6mo_NegPt00075_0 <- ifelse(cc_default_df$Util_Growth_6mo >= -0.00075 & cc_default_df$Util_Growth_6mo < 0, '1','0')
cc_default_df$Util_Growth_6mo_0_Pt029 <- ifelse(cc_default_df$Util_Growth_6mo >= 0 & cc_default_df$Util_Growth_6mo < 0.029, '1','0')
cc_default_df$Util_Growth_6mo_Pt029_2 <- ifelse(cc_default_df$Util_Growth_6mo >= 0.029 & cc_default_df$Util_Growth_6mo <2, '1','0')


cc_default_df$Util_Growth_6mo_Neg5_NegPt7 <- as.factor(cc_default_df$Util_Growth_6mo_Neg5_NegPt7)
cc_default_df$Util_Growth_6mo_NegPt7_NegPt00075 <- as.factor(cc_default_df$Util_Growth_6mo_NegPt7_NegPt00075)
cc_default_df$Util_Growth_6mo_NegPt00075_0 <- as.factor(cc_default_df$Util_Growth_6mo_NegPt00075_0)
cc_default_df$Util_Growth_6mo_0_Pt029 <- as.factor(cc_default_df$Util_Growth_6mo_0_Pt029)
cc_default_df$Util_Growth_6mo_Pt029_2 <- as.factor(cc_default_df$Util_Growth_6mo_Pt029_2)




#---------------------------------------------------- Max Bill Amount
cc_default_df$Max_Bill_Amt <- apply(cc_default_df[13:18], 1, max)

#-----------------------------------------------------Max Payment Amount
cc_default_df$Max_Pmt_Amt <- apply(cc_default_df[19:24], 1, max)

#----------------------------------------------------Max Delinquency
cc_default_df$Max_DLQ <- NA
cc_default_df$Max_DLQ <- apply(cc_default_df[7:12], 1, max) #using PAY_1 - PAY_6
cc_default_df$Max_DLQ <- as.numeric(cc_default_df$Max_DLQ)

#-----------------------------------------------------Max Utilization 
cc_default_df$Max_Util <- NA
cc_default_df$Max_Util <- apply(cc_default_df[58:63], 1, max) #using Util Chx 1-5
cc_default_df$Max_Util <- as.numeric(cc_default_df$Max_Util)

str(cc_default_df)
table(cc_default_df$DEFAULT)




cat("\n","------------------ SPLIT DATA --------------","\n")
str(cc_default_df)


train_df <- cc_default_df[cc_default_df$train == 1, c(3:5, 7:12, 25, 31:36, 38:42, 44:46, 53:57, 65:69, 76:83, 85:93)] #split keeping only wanted col
n.train <- dim(train_df)[1] # 15,180
count_if('1', cc_default_df$train) #confirms train_df matches original

test_df <- cc_default_df[cc_default_df$test == 1, c(3:5, 7:12, 25, 31:36, 38:42, 44:46, 53:57, 65:69, 76:83, 85:93)] #split keeping only wanted col
n.test <- dim(test_df)[1] # 7323
count_if('1', cc_default_df$test) #confirms test_df matches original

validate_df <- cc_default_df[cc_default_df$validate == 1, c(3:5, 7:12, 25, 31:36, 38:42, 44:46, 53:57, 65:69, 76:83, 85:93)] #split keeping only wanted col
n.validate <- dim(validate_df)[1] # 7497
count_if('1', cc_default_df$validate) #confirms train_df matches original

str(train_df)

table(cc_default_df$DEFAULT)

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

table(train_df$LIMIT_BAL_Neg_29999)
table(train_df$LIMIT_BAL_30000_159999)
table(train_df$LIMIT_BAL_160000_1000000)

table(train_df$Avg_Bill_Amt_Neg_56050_188)
table(train_df$Avg_Bill_Amt_189_2846)
table(train_df$Avg_Bill_Amt_2847_7488)
table(train_df$Avg_Bill_Amt_7489_31915)
table(train_df$Avg_Bill_Amt_31916_900000)

table(train_df$Avg_Pmt_Amt_0_2832)
table(train_df$Avg_Pmt_Amt_2833_12092)
table(train_df$Avg_Pmt_Amt_12093_627344)

table(train_df$Avg_Pmt_Ratio_Neg16429_pt02)
table(train_df$Avg_Pmt_Ratio_pt03_pt15)
table(train_df$Avg_Pmt_Ratio_pt16_1)
table(train_df$Avg_Pmt_Ratio_1_1pt17)
table(train_df$Avg_Pmt_Ratio_1pt18_2688)

table(train_df$Avg_Util_negpt2326_pt00095)
table(train_df$Avg_Util_pt00096_pt0091)
table(train_df$Avg_Util_pt0092_pt3673)
table(train_df$Avg_Util_pt3674_pt8231)
table(train_df$Avg_Util_pt8232_6)

table(train_df$Bal_Growth_6mo_neg708323_neg1)
table(train_df$Bal_Growth_6mo_neg123_21389)
table(train_df$Bal_Growth_6mo_21390_428792)

summary(train_df$Util_Growth_6mo)
summary(train_df$Max_Bill_Amt)
summary(train_df$Max_Pmt_Amt)
summary(train_df$Max_DLQ)
summary(train_df$Med_Bill)

################################### Continuous variables
# graphs of continuous variables

#par(mfrow=c(1,1))

aba <- cc_default_df[,c(25, 27, 37)]
head(aba2)

aba2 <- aba[aba$train==1, 3]
#------------------------ Avg_Bill_Amt
hist(aba2, xlab = '',  main="Histogram of Avg_Bill_Amt", col="blue")
boxplot(aba2,xlab = '',main="Boxplot of Avg_Bill_Amt",col="blue")
quantile(aba2, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 

plot(train_df$Avg_Pmt_Ratio, na.rm = TRUE)

round(skewness(aba2),2)
round(kurtosis(aba2),2)


#------------------------ Avg_Pmt_Ratio
Avg_Pmt_Ratio_Range <- range(cc_default_df$Avg_Pmt_Ratio)
hist(cc_default_df$Avg_Pmt_Ratio, xlab = '',  main="Histogram of Avg_Pmt_Ratio", col="blue")
boxplot(cc_default_df$Avg_Pmt_Ratio,xlab = '',main="Boxplot of Avg_Pmt_Ratio",col="blue")
quantile(cc_default_df$Avg_Pmt_Ratio, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 

plot(cc_default_df$Avg_Pmt_Ratio, na.rm = TRUE)

#------------------------
hist(train_df$Util_Growth_6mo, xlab = '',  main="Histogram of Util_Growth_6mo", col="blue")
boxplot(train_df$Util_Growth_6mo,xlab = '',main="Boxplot of Util_Growth_6mo",col="blue")
quantile(train_df$Util_Growth_6mo, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 

round(skewness(train_df$Util_Growth_6mo),2)
round(kurtosis(train_df$Util_Growth_6mo),2)


#------------------------
hist(train_df$Max_Bill_Amt, xlab = '',  main="Histogram of Max_Bill_Amt", col="blue")
boxplot(train_df$Max_Bill_Amt,xlab = '',main="Boxplot of Max_Bill_Amt",col="blue")
quantile(train_df$Max_Bill_Amt, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 

round(skewness(train_df$Max_Bill_Amt),2)
round(kurtosis(train_df$Max_Bill_Amt),2)


#------------------------
hist(train_df$Max_Pmt_Amt, xlab = '',  main="Histogram of Max_Pmt_Amt", col="blue")
boxplot(train_df$Max_Pmt_Amt,xlab = '',main="Boxplot of Max_Pmt_Amt",col="blue")
quantile(train_df$Max_Pmt_Amt, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 

round(skewness(train_df$Max_Pmt_Amt),2)
round(kurtosis(train_df$Max_Pmt_Amt),2)





# #------------------------graphs of factor variables



ggplot(train_df, aes(SEX, DEFAULT))+
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

par(mfrow=c(1,1))

ggplot(train_df, aes(LIMIT_BAL_Neg_29999))+
  geom_bar(aes(fill=DEFAULT), width = 0.5) + 
  theme(axis.text.x = element_text(angle=90, vjust=0)) +
  labs(title="BAR CHART OF LIMIT_BAL_Neg_29999") 
  
ggplot(train_df, aes(LIMIT_BAL_30000_159999))+
  geom_bar(aes(fill=DEFAULT), width = 0.5) + 
  theme(axis.text.x = element_text(angle=90, vjust=0)) +
  labs(title="BAR CHART OF LIMIT_BAL_30000_159999")  
  
ggplot(train_df, aes(LIMIT_BAL_160000_1000000))+
  geom_bar(aes(fill=DEFAULT), width = 0.5) + 
  theme(axis.text.x = element_text(angle=90, vjust=0)) +
  labs(title="BAR CHART OF LIMIT_BAL_160000_1000000")   

x <- ifelse(train_df$LIMIT_BAL_Neg_29999==1 & train_df$DEFAULT==1, "1", "na") 
table(x) 
z <- ifelse(train_df$LIMIT_BAL_30000_159999==1 & train_df$DEFAULT==1, "1", "na") 
table(z)
v <- ifelse(train_df$LIMIT_BAL_160000_1000000==1 & train_df$DEFAULT==1, "1", "na") 
table(v)

ggplot(train_df, aes(Avg_Bill_Amt_Neg_56050_188))+
  geom_bar(aes(fill=Avg_Bill_Amt_Neg_56050_188), width = 0.5) + 
  theme(axis.text.x = element_text(angle=90, vjust=0)) +
  labs(title="BAR CHART OF Avg_Bill_Amt_Neg_56050_188") 
  
ggplot(train_df, aes(Avg_Bill_Amt_189_2846))+
  geom_bar(aes(fill=Avg_Bill_Amt_189_2846), width = 0.5) + 
  theme(axis.text.x = element_text(angle=90, vjust=0)) +
  labs(title="BAR CHART OF Avg_Bill_Amt_189_2846")  
  
ggplot(train_df, aes(Avg_Bill_Amt_2847_7488))+
  geom_bar(aes(fill=Avg_Bill_Amt_2847_7488), width = 0.5) + 
  theme(axis.text.x = element_text(angle=90, vjust=0)) +
  labs(title="BAR CHART OF Avg_Bill_Amt_2847_7488") 
  
ggplot(train_df, aes(Avg_Bill_Amt_7489_31915))+
  geom_bar(aes(fill=Avg_Bill_Amt_7489_31915), width = 0.5) + 
  theme(axis.text.x = element_text(angle=90, vjust=0)) +
  labs(title="BAR CHART OF Avg_Bill_Amt_7489_31915") 
  
ggplot(train_df, aes(Avg_Bill_Amt_31916_900000))+
  geom_bar(aes(fill=Avg_Bill_Amt_31916_900000), width = 0.5) + 
  theme(axis.text.x = element_text(angle=90, vjust=0)) +
  labs(title="BAR CHART OF Avg_Bill_Amt_31916_900000")   

a <- ifelse(train_df$Avg_Bill_Amt_Neg_56050_188==1 & train_df$DEFAULT==1, "1", "na") 
table(a) 
b <- ifelse(train_df$Avg_Bill_Amt_189_2846==1 & train_df$DEFAULT==1, "1", "na") 
table(b)
c <- ifelse(train_df$Avg_Bill_Amt_2847_7488==1 & train_df$DEFAULT==1, "1", "na") 
table(c)
d <- ifelse(train_df$Avg_Bill_Amt_7489_31915==1 & train_df$DEFAULT==1, "1", "na") 
table(d)
e <- ifelse(train_df$Avg_Bill_Amt_31916_900000==1 & train_df$DEFAULT==1, "1", "na") 
table(e)

print(table(a, b, c, d, e))

f  <- subset(train_df, AGE_33_80 ==1 & train_df$DEFAULT==1, select="DEFAULT")
table(f) #1947
g  <- subset(train_df, AGE_0_24 ==1 & train_df$DEFAULT==1, select="DEFAULT")
table(g) #379
h  <- subset(train_df, AGE_25_32 ==1 & train_df$DEFAULT==1, select="DEFAULT")
table(h) #1097

graphics.off()

cat("\n","------------------ Correlations  --------------","\n")

library(corrplot)
# Correlation Table
str(train_df)
par(mfrow=c(1,1))
corr=cor(cc_default_df[,c(2,6, 37, 43, 52, 64, 75, 84, 90:93)])
corrplot(corr,method="color", outline=T, cl.pos="n", rect.col="black", 
         tl.col="indianred4", addCoef.col="black", number.digits=2, number.cex=0.60, 
         tl.cex=0.7, cl.cex=1, col=colorRampPalette(c("green4", "white", "red"))(100))

# produce list of correlations by highest value
corr[lower.tri(corr,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
corr=as.data.frame(as.table(corr))  #Turn into a 3-column table
corr=na.omit(corr)  #Get rid of the junk we flagged above
corr=corr[order(-abs(corr$Freq)),]    #Sort by highest correlation (whether +ve or -ve)
corr

graphics.off()

cat("\n","------------------ Model Based EDA  --------------","\n")

#------------------------------------------------------------------ Naive Bayes MODEL

library(naivebayes)

nb <- naive_bayes(DEFAULT~., train_df_encod)
nb
summary(nb)
names(nb)
nb$levels
nb$tables
nb$prior
plot(nb)

predicted_class <- predict(nb, type=c('prob'))
pct.acc <- mean(predicted_class==train_df_encod$DEFAULT)


#------------------------------------------------------------rpart Tree & formatting 
#https://blog.revolutionanalytics.com/2013/06/plotting-classification-and-regression-trees-with-plotrpart.html

library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)	

#--------------------Define Model Formula Inputs
form <- as.formula(DEFAULT ~ .)

form1a <- as.formula(DEFAULT ~ (LIMIT_BAL*Max_Util*Avg_Util) + SEX + EDUCATION + MARRIAGE + 
(Avg_Bill_Amt*Max_Bill_Amt) + (Avg_Pmt_Amt*Max_Pmt_Amt) + (PAY_1*PAY_2*PAY_3*PAY_4*PAY_5*PAY_6*Max_DLQ) + (Bal_Growth_6mo*Util_Growth_6mo))


#without PAY variables
form2 <- as.formula(DEFAULT ~ SEX + EDUCATION + MARRIAGE + LIMIT_BAL_Neg_29999 + LIMIT_BAL_30000_159999 + 
    LIMIT_BAL_160000_1000000 + AGE_0_24 + AGE_25_32 + AGE_33_80 + 
    Avg_Bill_Amt_Neg_56050_188 + Avg_Bill_Amt_189_2846 + Avg_Bill_Amt_2847_7488 + 
    Avg_Bill_Amt_7489_31915 + Avg_Bill_Amt_31916_900000 + Avg_Pmt_Amt_0_2832 + 
    Avg_Pmt_Amt_2833_12092 + Avg_Pmt_Amt_12093_627344 + Avg_Pmt_Ratio_Neg16429_pt02 + 
    Avg_Pmt_Ratio_pt03_pt15 + Avg_Pmt_Ratio_pt16_1 + Avg_Pmt_Ratio_1_1pt17 + 
    Avg_Pmt_Ratio_1pt18_2688 + Avg_Util_negpt2326_pt00095 + Avg_Util_pt00096_pt0091 + 
    Avg_Util_pt0092_pt3673 + Avg_Util_pt3674_pt8231 + Avg_Util_pt8232_6 + 
    Bal_Growth_6mo_neg708323_neg122 + Bal_Growth_6mo_neg123_21389 + 
    Bal_Growth_6mo_21390_428792 + Util_Growth_6mo_Neg5_NegPt7 + 
    Util_Growth_6mo_NegPt7_NegPt00075 + Util_Growth_6mo_NegPt00075_0 + Util_Growth_6mo_0_Pt029 + 
    Util_Growth_6mo_Pt029_2 + Max_Bill_Amt + Max_Pmt_Amt + Max_DLQ + Util_Chx1 + Util_Chx2 + Util_Chx3 + Util_Chx4 + Util_Chx5)

#variables id as important by simple logistic regression
form2a <- as.formula(DEFAULT ~ SEX + EDUCATION + 
                        MARRIAGE + PAY_1 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + 
                        LIMIT_BAL_Neg_29999 + LIMIT_BAL_30000_159999 + 
                        Avg_Bill_Amt_Neg_56050_188 + 
                        Avg_Bill_Amt_189_2846 + Avg_Bill_Amt_2847_7488 + 
                        Avg_Bill_Amt_7489_31915 + Avg_Pmt_Amt_0_2832 + 
                        Avg_Pmt_Amt_2833_12092 + Max_Bill_Amt + Max_DLQ)

#with pay variables and only variables used to build the tree
form3 <- as.formula(DEFAULT ~ PAY_1 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + Avg_Pmt_Ratio_Neg16429_pt02 + Max_DLQ)

#removed Max_DLQ
form4 <- as.formula(DEFAULT ~ PAY_1 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + Avg_Pmt_Ratio_Neg16429_pt02) 

#addef top 6 var from RF full run
form5 <- as.formula(DEFAULT ~ PAY_1 + Max_Util + Max_Bill_Amt + Max_Pmt_Amt + Util_Chx1 + Util_Chx3 + Util_Chx2 + Util_Chx5 + Util_Chx4 + Max_DLQ)
    
#--------------------------------------------------------tree model 1---------------------------

#trained only with var id as important by simple log reg
tree <- rpart(form2a, data = train_df, method = 'class', minsplit=30)
summary(tree)
par(mfrow=c(1,1))


#-------------------------perf measures on training data
library(pROC)
tree_probs_train = predict(tree, train_df, type="class")
confusionMatrix(tree_probs_train, train_df$DEFAULT)

roc_tree <- roc(train_df$DEFAULT, type.convert(tree_probs_train))
auc(roc_tree)

#-----------------------perf measusres on test data
tree_probs = predict(tree, test_df, type="class")
confusionMatrix(tree_probs, test_df$DEFAULT)

roc_tree <- roc(test_df$DEFAULT, type.convert(tree_probs))
auc(roc_tree)



#------------------------visualize
plot(tree)
text(tree)
prp(tree) #plot the tree
prp(tree, varlen=30) #shorten variables

#new.tree <- prp(tree,snip=TRUE)$obj # interactively trim the tree
#prp(new.tree.1) # display the new tree
fancyRpartPlot(tree)


#-----------------------------------------------------tree model 2---------------------------

#trained with all minus pay
tree <- rpart(form2, data = train_df, method = 'class', minsplit=30)
summary(tree)
par(mfrow=c(1,1))


#-------------------------perf measures on training data
tree_probs_train = predict(tree, train_df, type="class")
confusionMatrix(tree_probs_train, train_df$DEFAULT)

roc_tree <- roc(train_df$DEFAULT, type.convert(tree_probs_train))
auc(roc_tree)

#-----------------------perf measusres on test data
tree_probs = predict(tree, test_df, type="class")
confusionMatrix(tree_probs, test_df$DEFAULT)

roc_tree <- roc(test_df$DEFAULT, type.convert(tree_probs))
auc(roc_tree)



#------------------------visualize
plot(tree)
text(tree)
prp(tree) #plot the tree
prp(tree, varlen=30) #shorten variables

#new.tree <- prp(tree,snip=TRUE)$obj # interactively trim the tree
#prp(new.tree.1) # display the new tree
fancyRpartPlot(tree)



#-----------------------------------------------------tree model 3---------------------------

#trained with all + pay
tree <- rpart(form, data = train_df, method = 'class', minsplit=30)
summary(tree)
par(mfrow=c(1,1))


#-------------------------perf measures on training data
tree_probs_train = predict(tree, train_df, type="class")
confusionMatrix(tree_probs_train, train_df$DEFAULT)

roc_tree <- roc(train_df$DEFAULT, type.convert(tree_probs_train))
auc(roc_tree)

#-----------------------perf measusres on test data
tree_probs = predict(tree, test_df, type="class")
confusionMatrix(tree_probs, test_df$DEFAULT)

roc_tree <- roc(test_df$DEFAULT, type.convert(tree_probs))
auc(roc_tree)



#------------------------visualize
plot(tree)
text(tree)
prp(tree) #plot the tree
prp(tree, varlen=30) #shorten variables

#new.tree <- prp(tree,snip=TRUE)$obj # interactively trim the tree
#prp(new.tree.1) # display the new tree
fancyRpartPlot(tree)






# --------------------------------------------------------- OneR on select features-----------------------
#variables id as important by simple logistic regression
model.1 <- OneR(form2a, data=train_df, verbose=TRUE);

summary(model.1)
plot(model.1)


#-------------------------perf measures on training data
oneR_train = predict(model.1, train_df, type="class")
confusionMatrix(oneR_train, train_df$DEFAULT)

roc_tree <- roc(train_df$DEFAULT, type.convert(oneR_train))
auc(roc_tree)

table(train_df$PAY_2, train_df$DEFAULT)

#-----------------------perf measusres on test data
oneR_test = predict(model.1, test_df, type="class")
confusionMatrix(oneR_test , test_df$DEFAULT)

roc_tree <- roc(test_df$DEFAULT, type.convert(oneR_test))
auc(roc_tree)




# -------------------------------------------- OneR 2 all plus pay-----------------------
model.2 <- OneR(DEFAULT~., data=train_df, verbose=TRUE);

summary(model.2)


#-------------------------perf measures on training data
oneR_train = predict(model.2, train_df, type="class")
confusionMatrix(oneR_train, train_df$DEFAULT)

roc_tree <- roc(train_df$DEFAULT, type.convert(oneR_train))
auc(roc_tree)

#-----------------------perf measusres on test data
oneR_test = predict(model.2, test_df, type="class")
confusionMatrix(oneR_test , test_df$DEFAULT)

roc_tree <- roc(test_df$DEFAULT, type.convert(oneR_test))
auc(roc_tree)



# ------------------------------------------ OneR 3 all minus pay-----------------------
model.3 <- OneR(form2, data=train_df, verbose=TRUE);

summary(model.2)


#-------------------------perf measures on training data
oneR_train = predict(model.3, train_df, type="class")
confusionMatrix(oneR_train, train_df$DEFAULT)

roc_tree <- roc(train_df$DEFAULT, type.convert(oneR_train))
auc(roc_tree)

#-----------------------perf measusres on test data
oneR_test = predict(model.3, test_df, type="class")
confusionMatrix(oneR_test , test_df$DEFAULT)

roc_tree <- roc(test_df$DEFAULT, type.convert(oneR_test))
auc(roc_tree)



#-----------------------------------------------------------------SIMPLE Logistic Regression 
library(pROC)

#---------------------------------------------------------------------all variables
log_reg <- glm(form, data=train_df, family=binomial)
summary(log_reg) #AIC 13227

str(train_df)

#-------------------------perf measures on training data
log_probs = predict(log_reg, train_df, type="response")
summary(log_probs)
str(log_probs)

glm.pred = rep("0", 15180)
glm.pred[log_probs >.5] = "1"

confusionMatrix(as.factor(glm.pred), train_df$DEFAULT)



#-----------------------perf measures on test data
log_probs = predict(log_reg, test_df, type="response")

glm.pred = rep("0", 7323)
glm.pred[log_probs >.5] = "1"

confusionMatrix(as.factor(glm.pred), test_df$DEFAULT)





#mean(glm.pred==test_df$DEFAULT) #0.8223406 have been correctly predicted

#roc_log_reg <- roc(test_df$DEFAULT, log_probs)
#auc(roc_log_reg)  #AUC .7616

#plot(roc_log_reg)

#-----------------------------------------------------------------------reduced model
log_reg_impvar <- glm(form2a, data=train_df, family=binomial)
summary(log_reg_impvar) #AIC: 13249


#-------------------------perf measures on training data
log_probs_impvar = predict(log_reg_impvar, train_df, type="response")
glm.pred = rep("0", 15180)
glm.pred[log_probs_impvar >.5] = "1"


confusionMatrix(as.factor(glm.pred), train_df$DEFAULT)



#-----------------------perf measures on test data

log_probs_impvar = predict(log_reg_impvar, test_df, type="response")
glm.pred = rep("0", 7323)
glm.pred[log_probs_impvar >.5] = "1"


confusionMatrix(as.factor(glm.pred), test_df$DEFAULT)



#table(glm.pred, test_df$DEFAULT)
#(5477+545)/(5477+545+289+1012) #0.8223406 correctly classified

#roc_log_reg <- roc(test_df$DEFAULT, log_probs_impvar)
#auc(roc_log_reg)  #AUC .7758

#plot(roc_log_reg)



cat("\n","------------------ Predictive Models  --------------","\n")

# ------------- Random Forest

str(train_df)

head(train_df)
library(randomForest)
library(pROC)


#_--------------------------------------------------------------------model on form, all var
rf_mod = randomForest(form, data=train_df, mtry=15, importance=TRUE)
str(rf_mod)

#------------------results
par(mfrow=c(1,1))
plot(rf_mod)
varImpPlot(rf_mod)
importance(rf_mod)


graphics.off()



#-------------------------perf measures on training data
library(expss)
rf_train = predict(rf_mod, train_df, type="class")

rf_train <- factor(rf_train, levels=c(0,1))

#confusionMatrix(rf_train, train_df$DEFAULT)
head(rf_train)
cro(train_df$DEFAULT, rf_train)
roc_tree <- roc(train_df$DEFAULT, type.convert(rf_train))
auc(roc_tree)

#-----------------------perf measures on test data
rf_test = predict(rf_mod, test_df, type="class")

rf_test <- factor(rf_test, levels=c(0,1))

#confusionMatrix(rf_test, test_df$DEFAULT)
cro(test_df$DEFAULT, rf_test)

roc_tree <- roc(test_df$DEFAULT, type.convert(rf_test))
auc(roc_tree)



#_-------------------------------------------------------model on form5, variables id as important in RF

#rf_mod = randomForest(DEFAULT ~ PAY_1 + Max_Pmt_Amt + Util_Growth_6mo_Neg5_NegPt7+ #Util_Growth_6mo_NegPt7_NegPt00075 + Util_Growth_6mo_NegPt00075_0 + Util_Growth_6mo_0_Pt029 + #Util_Growth_6mo_Pt029_2 + Max_DLQ, data=train_df, mtry=5, importance=TRUE)

rf_mod = randomForest(form5, data=train_df, mtry=5, importance=TRUE)

#------------------results
par(mfrow=c(1,1))
plot(rf_mod)
varImpPlot(rf_mod)
importance(rf_mod)


graphics.off()




#-------------------------perf measures on training data
rf_train = predict(rf_mod, train_df, type="class")

rf_train <- factor(rf_train, levels=c(0,1))

#confusionMatrix(rf_train, train_df$DEFAULT)
head(rf_train)
cro(train_df$DEFAULT, rf_train)
roc_tree <- roc(train_df$DEFAULT, type.convert(rf_train))
auc(roc_tree)

#-----------------------perf measures on test data
rf_test = predict(rf_mod, test_df, type="class")

rf_test <- factor(rf_test, levels=c(0,1))

#confusionMatrix(rf_test, test_df$DEFAULT)
cro(test_df$DEFAULT, rf_test)

roc_tree <- roc(test_df$DEFAULT, type.convert(rf_test))
auc(roc_tree)


# ----------------------------------------------------------------- Gradient Boosting
head(test_df)

#install.packages("xgboost", dependencies=TRUE)

library(xgboost)
library(ROCR)
library(utils)
library(graphics)

default <- type.convert(train_df$DEFAULT)
table(default)
class(default)

default_test <- type.convert(test_df$DEFAULT)
table(default_test)
class(default_test)

x <- model.matrix(DEFAULT~., train_df)[,-1] #setting input variables
x_test <- model.matrix(DEFAULT~., test_df)[,-1]
colnames(x)

dtrain <- xgb.DMatrix(x, label=default)
dtest <- xgb.DMatrix(x_test, label=default_test)
watchlist <- list(train= dtrain, eval = dtest)
nrow(dtrain)
nrow(dtest)

param <- list(eta= .1, gamma = 3, max_depth=15, subsample=0.5, seed=1, eval_metric = "error", nthread = 2, objective="binary:logistic", booster = "gblinear")
my_etas <- list(eta=c(.01, .05, 0.1, 0.5, 0.9))

#---------------------------------------------------------------------------------------------------full model
bst <- xgb.train(param, dtrain, nrounds=5, watchlist, callbacks=list(cb.reset.parameters(my_etas)))
summary(bst)
names(bst)

names <- bst$feature_names

importance_matrix <- xgb.importance(colnames(dtrain), model=bst)
xgb.plot.importance(importance_matrix, rel_to_first=TRUE, xlab="Relative Importance")
gg <- xgb.ggplot.importance(importance_matrix[1:20], rel_to_first = TRUE, measure = NULL, n_clusters = 3)
gg + ggplot2::ylab("Frequency")

#feature importance information
#model <- xgb.dump(bst, with.stats = T)
#head(model)
#model[1:10] #This statement prints top 10 nodes of the model
#str(train_df)
#names <- dimnames(train_df[, -10])[[2]]
#importance_matrix <- xgb.importance(names, model=bst)
#xgb.plot.importance(importance_matrix[1:10,])
#plot(importance_matrix$Feature, importance_matrix$Weight)
#-----------------------------prediction train
str(train_df)

pred_boost <- predict(bst, dtrain)
bst$feature_names
dimnames(dtrain)

gbm_pred = rep("0", 15180)
gbm_pred[pred_boost >.5] = 1
str(gbm_pred)

gbm_pred <- type.convert(gbm_pred)
gbm_pred <- factor(gbm_pred, levels=c(0,1))

#confusion Matrix
#confusionMatrix(gbm_pred, train_df$DEFAULT)


#ROC
xgb.pred <- prediction(pred_boost, train_df$DEFAULT)
xgb_perf <- performance(xgb.pred, "tpr", "fpr")

auc_temp <- performance(xgb.pred, "auc")
gbm_auc_testing <- as.numeric(auc_temp@y.values)
gbm_auc_testing


#----------------------------prediction test

pred_boost <- predict(bst, dtest)
bst$feature_names
dimnames(dtest)

gbm_pred = rep("0", 7323)
gbm_pred[pred_boost >.5] = 1
str(gbm_pred)

gbm_pred <- type.convert(gbm_pred)
gbm_pred <- factor(gbm_pred, levels=c(0,1))

#confusion Matrix
#confusionMatrix(gbm_pred, test_df$DEFAULT)


#ROC
xgb.pred <- prediction(pred_boost, test_df$DEFAULT)
xgb_perf <- performance(xgb.pred, "tpr", "fpr")

#AUC
auc_temp <- performance(xgb.pred, "auc")
gbm_auc_testing <- as.numeric(auc_temp@y.values)
gbm_auc_testing

#plot
plot(xgb_perf, avg="threshold", colorize=TRUE, lwd=1, main="ROC Curve w/Thresholds", print.cuttofs.at=seq(0,1,by=.05), text.adj=c(-0.5, 0.5), text.cex=0.5)
grid(col="lightgray")
axis(1, at=seq(0, 1, by=0.1))
axis(2, at=seq(0, 1, by=0.1))
abline(v=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
abline(h=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
lines(x=c(0, 1), y=c(0, 1), col="black", lty="dotted")











#---------------------------------------------------------------------------------------------------reduced model

default <- type.convert(train_df$DEFAULT)
table(default)
class(default)

default_test <- type.convert(test_df$DEFAULT)
table(default_test)
class(default_test)

x <- model.matrix(form5, train_df)[,-1] #setting input variables
x_test <- model.matrix(form5, test_df)[,-1]
colnames(x)

dtrain <- xgb.DMatrix(x, label=default)
dtest <- xgb.DMatrix(x_test, label=default_test)
watchlist <- list(train= dtrain, eval = dtest)
nrow(dtrain)
nrow(dtest)

param <- list(eta= .1, gamma = 3, max_depth=15, subsample=0.5, seed=1, eval_metric = "error", nthread = 2, objective="binary:logistic", booster = "gblinear")
my_etas <- list(eta=c(.01, .05, 0.1, 0.5, 0.9))


#------------------------

bst <- xgb.train(param, dtrain, nrounds=5, watchlist, callbacks=list(cb.reset.parameters(my_etas)))
summary(bst)
names <- bst$feature_names

importance_matrix <- xgb.importance(names, model=bst)
xgb.plot.importance(importance_matrix, rel_to_first=TRUE, xlab="Relative Importance")
gg <- xgb.ggplot.importance(importance_matrix[1:10], rel_to_first = TRUE, measure = NULL, n_clusters = 3)
gg + ggplot2::ylab("Frequency")


#-----------------------------prediction train
str(train_df)

pred_boost <- predict(bst, dtrain)
bst$feature_names
dimnames(dtrain)

gbm_pred = rep("0", 15180)
gbm_pred[pred_boost >.5] = 1
str(gbm_pred)

gbm_pred <- type.convert(gbm_pred)
gbm_pred <- factor(gbm_pred, levels=c(0,1))
table(gbm_pred)
#confusion Matrix
#confusionMatrix(gbm_pred, train_df$DEFAULT)


#ROC
xgb.pred <- prediction(pred_boost, train_df$DEFAULT)
xgb_perf <- performance(xgb.pred, "tpr", "fpr")

#AUC
auc_temp <- performance(xgb.pred, "auc")
gbm_auc_testing <- as.numeric(auc_temp@y.values)
gbm_auc_testing




roc_test <- roc(train_df$DEFAULT, pred_boost, algorithm=2)
plot(roc_test)
auc(roc_test)
#----------------------------prediction test

pred_boost <- predict(bst, dtest)
bst$feature_names
dimnames(dtest)

gbm_pred = rep("0", 7323)
gbm_pred[pred_boost >.5] = 1
str(gbm_pred)

gbm_pred <- type.convert(gbm_pred)

#confusion Matrix
#confusionMatrix(gbm_pred, test_df$DEFAULT)
dim(gbm_pred)
test_frame <- as.data.frame(test_df$DEFAULT)

table(gbm_pred, test_df$DEFAULT)

#plot

#ROC
xgb.pred <- prediction(gbm_pred, test_df$DEFAULT)
xgb_perf <- performance(xgb.pred, "tpr", "fpr")

plot(xgb_perf, avg="threshold", colorize=TRUE, lwd=1, main="ROC Curve w/Thresholds", print.cuttofs.at=seq(0,1,by=.05), text.adj=c(-0.5, 0.5), text.cex=0.5)
grid(col="lightgray")
axis(1, at=seq(0, 1, by=0.1))
axis(2, at=seq(0, 1, by=0.1))
abline(v=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
abline(h=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
lines(x=c(0, 1), y=c(0, 1), col="black", lty="dotted")


#AUC
auc_temp <- performance(xgb.pred, "auc")
gbm_auc_testing <- as.numeric(auc_temp@y.values)
gbm_auc_testing #.7567


#---------------------------------------------------------------------------------------------------all minus pay
x <- model.matrix(form2, train_df)[,-1]
x_test <- model.matrix(form2, test_df)[,-1]
colnames(x)

dtrain <- xgb.DMatrix(x, label=default)
dtest <- xgb.DMatrix(x_test, label=default_test)
watchlist <- list(train= dtrain, eval = dtest)
nrow(dtrain)
nrow(dtest)

param <- list(eta= .1, gamma = 3, max_depth=15, subsample=0.5, seed=1, eval_metric = "error", nthread = 2, objective="binary:logistic", booster = "gblinear")
my_etas <- list(eta=c(.01, .05, 0.1, 0.5, 0.9))

#-------------------------------model
bst <- xgb.train(param, dtrain, nrounds=5, watchlist, callbacks=list(cb.reset.parameters(my_etas)))
summary(bst)
names(bst)

names <- bst$feature_names

importance_matrix <- xgb.importance(names, model=bst)
xgb.plot.importance(importance_matrix, rel_to_first=TRUE, xlab="Relative Importance")
gg <- xgb.ggplot.importance(importance_matrix[1:20], rel_to_first = TRUE, measure = NULL, n_clusters = 3)
gg + ggplot2::ylab("Frequency")


#-----------------------------prediction train
str(train_df)

pred_boost <- predict(bst, dtrain)
bst$feature_names
dimnames(dtrain)

gbm_pred = rep("0", 15180)
gbm_pred[pred_boost >.5] = 1
str(gbm_pred)

gbm_pred <- type.convert(gbm_pred)
gbm_pred <- factor(gbm_pred, levels=c(0,1))

#confusion Matrix
confusionMatrix(gbm_pred, train_df$DEFAULT)


#----------------------------prediction test

pred_boost <- predict(bst, dtest)
bst$feature_names
dimnames(dtest)

gbm_pred = rep("0", 7323)
gbm_pred[pred_boost >.5] = 1
str(gbm_pred)

gbm_pred <- type.convert(gbm_pred)
gbm_pred <- factor(gbm_pred, levels=c(0,1))

#confusion Matrix
confusionMatrix(gbm_pred, test_df$DEFAULT)

#--------------------------------------------------


#ROC
xgb.pred <- prediction(pred_boost, test_df$DEFAULT)
xgb_perf <- performance(xgb.pred, "tpr", "fpr")

plot(xgb_perf, avg="threshold", colorize=TRUE, lwd=1, main="ROC Curve w/Thresholds", print.cuttofs.at=seq(0,1,by=.05), text.adj=c(-0.5, 0.5), text.cex=0.5)
grid(col="lightgray")
axis(1, at=seq(0, 1, by=0.1))
axis(2, at=seq(0, 1, by=0.1))
abline(v=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
abline(h=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
lines(x=c(0, 1), y=c(0, 1), col="black", lty="dotted")


#AUC
auc_temp <- performance(xgb.pred, "auc")
gbm_auc_testing <- as.numeric(auc_temp@y.values)
gbm_auc_testing





#-------------------------Exploring data using tables
print(credit_card_default[c(8,10, 24, 34, 35,46, 53, 66, 2671, 2688), c(7:12, 14, 19)])
print(credit_card_default[3:9, c(7:12, 14, 19)])

print(credit_card_default[(credit_card_default$PAY_1=='-2'), c(7:12, 14, 19)])
table(train_df$PAY_1)
table(credit_card_default$PAY_0)




 # ---------------------------------------------------- Logistic Regression with Variable Selection
 
library(tidyverse)
library(MASS)
library(leaps)

#####################regsubsets

regfit.full = regsubsets(form, data = train_df, 
    nvmax = 11, method=c("backward", "forward", "step"))  #Best Subset Selection for ALL variables
reg.summary = summary(regfit.full)

names(reg.summary)
reg.summary$outmat
reg.summary$rsq
reg.summary$adjr2
which.min(reg.summary$bic)
which.min(reg.summary$cp)
coef(regfit.full, 1:3)
vcov(regfit.full, 3)
plot(regfit.full, main="Regsubsets Results", scale= c("bic"), col=gray(seq(0, 0.9, length=10)))

coef(regfit.full, 11)


#---------------------------------------------------------------------log reg selected variables
log_reg <- glm(DEFAULT~ PAY_1 + PAY_3 + PAY_5 + AGE_25_32 + Avg_Bill_Amt_Neg_56050_188 + Avg_Pmt_Amt_2833_12092 + Avg_Pmt_Ratio_Neg16429_pt02 + Avg_Util_negpt2326_pt00095 + Avg_Pmt_Ratio_1pt18_2688 + Bal_Growth_6mo_21390_428792, data=train_df, family=binomial)
summary(log_reg) #AIC 13227

str(train_df)

#-------------------------perf measures on training data
log_probs = predict(log_reg, train_df, type="response")
summary(log_probs)
str(log_probs)

glm.pred = rep("0", 15180)
glm.pred[log_probs >.5] = "1"

#confusionMatrix(as.factor(glm.pred), train_df$DEFAULT)

#confusionMatrix(rf_test, test_df$DEFAULT)
cro(test_df$DEFAULT, rf_test)

roc_tree <- roc(train_df$DEFAULT, type.convert(glm.pred))
auc(roc_tree)



#-----------------------perf measures on test data
log_probs = predict(log_reg, test_df, type="response")

glm.pred = rep("0", 7323)
glm.pred[log_probs >.5] = "1"

confusionMatrix(as.factor(glm.pred), test_df$DEFAULT)



roc_tree <- roc(test_df$DEFAULT, type.convert(glm.pred))
auc(roc_tree)


#---------------------------------------------------------------------RF selected variables
log_reg <- glm(form5, data=train_df, family=binomial)
summary(log_reg) 

install.packages("jtools", dependencies=TRUE)
library('jtools')
library("pROC")

summ(log_reg)
#export_summs(log_reg)
?tab_model
#-------------------------perf measures on training data
log_probs = predict(log_reg, train_df, type="response")
summary(log_probs)
str(log_probs)

glm.pred = rep("0", 15180)
glm.pred[log_probs >.5] = "1"

confusionMatrix(as.factor(glm.pred), train_df$DEFAULT)

effect_plot(log_reg, pred=PAY_1, interval=TRUE, plot.points=FALSE)


#-----------------------perf measures on test data
log_probs = predict(log_reg, test_df, type="response")

glm.pred = rep("0", 7323)
glm.pred[log_probs >.5] = "1"

confusionMatrix(as.factor(glm.pred), test_df$DEFAULT)



#mean(glm.pred==test_df$DEFAULT) #0.8223406 have been correctly predicted

#roc_log_reg <- roc(test_df$DEFAULT, log_probs)
#auc(roc_log_reg)  #AUC .7616

#plot(roc_log_reg)


#--------------------------------------------------------------------- MODEL SELECTED FOR MONITORING PLAN

log_reg <- glm(DEFAULT ~ PAY_1 + Max_Util + Max_Pmt_Amt + Max_DLQ + Bal_Growth_6mo_21390_428792 + Avg_Pmt_Amt_2833_12092 +AGE_25_32, data=train_df, family=binomial)

summ(log_reg)

#--------------------------------------perf measures on training data
log_probs_train = predict(log_reg, train_df, type="response")
summary(log_probs_train)
str(log_probs_train)

glm.pred.train = rep("0", 15180)
glm.pred.train[log_probs_train >.5] = "1"

confusionMatrix(as.factor(glm.pred.train), train_df$DEFAULT)

#effect_plot(log_reg, pred=PAY_1, interval=TRUE, plot.points=FALSE)

roc_tree <- roc(train_df$DEFAULT, type.convert(glm.pred.train))
auc(roc_tree)

#----------------------------------------Roc Curve

ROCpred <- prediction(log_probs_train, train_df$DEFAULT)
ROCperf <- performance(ROCpred, 'tpr', 'fpr')
plot(ROCperf, avg="threshold", colorize=TRUE, text.adj=c(-0.2, 1), main = "Training Data: ROC Curve with Thresholds")
grid(col="lightgray")
axis(1, at=seq(0, 1, by=0.1))
axis(2, at=seq(0, 1, by=0.1))
abline(v=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
abline(h=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
lines(x=c(0, 1), y=c(0, 1), col="black", lty="dotted")



#----------------------------------------KS Table - training data

model.score <- log_probs_train
response <- type.convert(train_df$DEFAULT)

my.df <- as.data.frame(cbind(model.score, response))
summary(my.df)

table(train_df$DEFAULT)



#decile model scores
#decile.pts <- quantile(my.df$model.score,
			probs=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9));
#print(decile.pts)

#semi-decile model scores
semi.decile.pts <- quantile(my.df$model.score,
			probs=c(0.05, 0.10,  0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95));
print(semi.decile.pts)

#assign model scores to deciles
my.df$model.semi.decile <- cut(my.df$model.score,breaks=c(0,semi.decile.pts,1),
			labels=rev(c('01','02','03','04','05','06','07','08','09','10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20'))
			);

head(my.df)

# Check the min score in each model decile;
aggregate(my.df$model.score,by=list(Decile=my.df$model.semi.decile),FUN=min);

table(my.df$model.semi.decile)
table(my.df$model.semi.decile,my.df$response)


#create ks table
ks.table <- as.data.frame(list(Y0=table(my.df$model.semi.decile,my.df$response)[,1],
		Y1=table(my.df$model.semi.decile,my.df$response)[,2],
		Decile=rev(c('01','02','03','04','05','06','07','08','09','10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20'))
		));


# Sort the data frame by decile;
ks.table[order(ks.table$Decile),]


# Now plug these values into your KS spreadsheet and compute the KS statistic;





#-------------------------------------------perf measures on test data
log_probs_test = predict(log_reg, test_df, type="response")

glm.pred = rep("0", 7323)
glm.pred[log_probs >.5] = "1"

confusionMatrix(as.factor(glm.pred), test_df$DEFAULT)



#mean(glm.pred==test_df$DEFAULT) #0.8223406 have been correctly predicted

#----------------------------------------Roc Curve
library(ROCR)
ROCpred <- prediction(log_probs, test_df$DEFAULT)
ROCperf <- performance(ROCpred, 'tpr', 'fpr')
plot(ROCperf, avg="threshold", colorize=TRUE, text.adj=c(-0.2, 1), main = "Testing Data: ROC Curve with Thresholds" )
grid(col="lightgray")
axis(1, at=seq(0, 1, by=0.1))
axis(2, at=seq(0, 1, by=0.1))
abline(v=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
abline(h=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
lines(x=c(0, 1), y=c(0, 1), col="black", lty="dotted")



#roc_tree <- roc(test_df$DEFAULT, type.convert(glm.pred))
#auc(roc_tree)


#plot(roc_tree)
table(cc_default_df$DEFAULT, cc_default_df$PAY_1)



#----------------------------------------KS Table - testing data

model.score <- log_probs_test
response <- type.convert(test_df$DEFAULT)

my.df <- as.data.frame(cbind(model.score, response))
summary(my.df)

table(test_df$DEFAULT)
table(glm.pred)


#decile model scores
#decile.pts <- quantile(my.df$model.score,
			probs=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9));
#print(decile.pts)

#semi-decile model scores
semi.decile.pts <- quantile(my.df$model.score,
			probs=c(0.05, 0.10,  0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95));
print(semi.decile.pts)

#assign model scores to deciles
my.df$model.semi.decile <- cut(my.df$model.score,breaks=c(0,semi.decile.pts,1),
			labels=rev(c('01','02','03','04','05','06','07','08','09','10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20'))
			);

head(my.df)

# Check the min score in each model decile;
aggregate(my.df$model.score,by=list(Decile=my.df$model.semi.decile),FUN=min);

table(my.df$model.semi.decile)
table(my.df$model.semi.decile,my.df$response)


#create ks table
ks.table <- as.data.frame(list(Y0=table(my.df$model.semi.decile,my.df$response)[,1],
		Y1=table(my.df$model.semi.decile,my.df$response)[,2],
		Decile=rev(c('01','02','03','04','05','06','07','08','09','10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20'))
		));


# Sort the data frame by decile;
ks.table[order(ks.table$Decile),]


# Now plug these values into your KS spreadsheet and compute the KS statistic;



























cat("\n","------------------ Factor Variable Updates - TRAINING DATA  --------------","\n")


train_df_encod <- train_df
str(train_df_encod)

levels(train_df_encod$SEX) <- c("Male", "Female")
table(train_df_encod$SEX)
table(train_df$SEX)

levels(train_df_encod$EDUCATION) <- c("Graduate",	"University",	"High School",	"Others")
table(train_df_encod$EDUCATION)
table(train_df$EDUCATION)

levels(train_df_encod$MARRIAGE) <- c("Married",	"Single",	"Others")
table(train_df_encod$MARRIAGE)
table(train_df$MARRIAGE)


levels(train_df_encod$PAY_1) <- c("Pay_Duly", "Delay1_Mnth", "Delay2_Mnth", 
                                  "Delay3_Mnth", "Delay4_Mnth", "Delay5_Mnth", "Delay6_Mnth",
                                  "Delay7_Mnth", "Delay8_Mnth")

table(train_df_encod$PAY_1)


levels(train_df_encod$PAY_2) <- c("Pay_Duly", "Delay1_Mnth", "Delay2_Mnth", 
                                  "Delay3_Mnth", "Delay4_Mnth", "Delay5_Mnth", "Delay6_Mnth",
                                  "Delay7_Mnth", "Delay8_Mnth")
table(train_df$PAY_2)
table(train_df_encod$PAY_2) 


levels(train_df_encod$PAY_3) <- c("Pay_Duly", "Delay1_Mnth", "Delay2_Mnth", 
                                  "Delay3_Mnth", "Delay4_Mnth", "Delay5_Mnth", "Delay6_Mnth",
                                  "Delay7_Mnth", "Delay8_Mnth")
table(train_df$PAY_3)
table(train_df_encod$PAY_3)


levels(train_df_encod$PAY_4) <- c("Pay_Duly", "Delay1_Mnth", "Delay2_Mnth", 
                                  "Delay3_Mnth", "Delay4_Mnth", "Delay5_Mnth", "Delay6_Mnth",
                                  "Delay7_Mnth", "Delay8_Mnth")
table(train_df$PAY_4)
table(train_df_encod$PAY_4) 

levels(train_df_encod$PAY_5) <- c("Pay_Duly", "Delay2_Mnth", 
                                  "Delay3_Mnth", "Delay4_Mnth", "Delay5_Mnth", "Delay6_Mnth",
                                  "Delay7_Mnth", "Delay8_Mnth")
table(train_df$PAY_5)
table(train_df_encod$PAY_5)

levels(train_df_encod$PAY_6) <- c("Pay_Duly", "Delay2_Mnth", 
                                  "Delay3_Mnth", "Delay4_Mnth", "Delay5_Mnth", "Delay6_Mnth",
                                  "Delay7_Mnth", "Delay8_Mnth")
table(train_df$PAY_6)
table(train_df_encod$PAY_6)

levels(train_df_encod$DEFAULT) <- c("NON_DEFAULT", "DEFAULT")
table(train_df_encod$DEFAULT)
table(train_df$DEFAULT)



#make.names(train_df_encod$PAY_1, unique = FALSE)
#table(train_df_encod$PAY_1)

#make.names(train_df_encod$DEFAULT, unique = FALSE)
#table(train_df_encod$DEFAULT)

str(train_df_encod)

cat("\n","------------------ Factor Variable Updates - TEST DATA  --------------","\n")


test_df_encod <- test_df

levels(test_df_encod$SEX) <- c("Male", "Female")
table(test_df_encod$SEX)
table(test_df$SEX)

levels(test_df_encod$EDUCATION) <- c("Graduate",	"University",	"High School",	"Others")
table(test_df_encod$EDUCATION)
table(test_df$EDUCATION)

levels(test_df_encod$MARRIAGE) <- c("Married",	"Single",	"Others")
table(test_df_encod$MARRIAGE)
table(test_df$MARRIAGE)


levels(test_df_encod$PAY_1) <- c("Pay_Duly", "Delay1_Mnth", "Delay2_Mnth", 
                                  "Delay3_Mnth", "Delay4_Mnth", "Delay5_Mnth", "Delay6_Mnth",
                                  "Delay7_Mnth", "Delay8_Mnth")

table(test_df_encod$PAY_1)


levels(test_df_encod$PAY_2) <- c("Pay_Duly", "Delay1_Mnth", "Delay2_Mnth", 
                                  "Delay3_Mnth", "Delay4_Mnth", "Delay5_Mnth", "Delay6_Mnth",
                                  "Delay7_Mnth", "Delay8_Mnth")
table(test_df$PAY_2)
table(test_df_encod$PAY_2) 


levels(test_df_encod$PAY_3) <- c("Pay_Duly", "Delay1_Mnth", "Delay2_Mnth", 
                                  "Delay3_Mnth", "Delay4_Mnth", "Delay5_Mnth", "Delay6_Mnth",
                                  "Delay7_Mnth", "Delay8_Mnth")
table(test_df$PAY_3)
table(test_df_encod$PAY_3)


levels(test_df_encod$PAY_4) <- c("Pay_Duly", "Delay1_Mnth", "Delay2_Mnth", 
                                  "Delay3_Mnth", "Delay4_Mnth", "Delay5_Mnth", "Delay6_Mnth",
                                  "Delay7_Mnth", "Delay8_Mnth")
table(test_df$PAY_4)
table(test_df_encod$PAY_4) 

levels(test_df_encod$PAY_5) <- c("Pay_Duly", "Delay2_Mnth", 
                                  "Delay3_Mnth", "Delay4_Mnth", "Delay5_Mnth", "Delay6_Mnth",
                                  "Delay7_Mnth", "Delay8_Mnth")
table(test_df$PAY_5)
table(test_df_encod$PAY_5)

levels(test_df_encod$PAY_6) <- c("Pay_Duly", "Delay2_Mnth", 
                                  "Delay3_Mnth", "Delay4_Mnth", "Delay5_Mnth", "Delay6_Mnth",
                                  "Delay7_Mnth", "Delay8_Mnth")
table(test_df$PAY_6)
table(test_df_encod$PAY_6)

levels(test_df_encod$DEFAULT) <- c("NON_DEFAULT", "DEFAULT")
table(test_df_encod$DEFAULT)
table(test_df$DEFAULT)

#make.names(test_df_encod$PAY_1, unique = FALSE)
#table(test_df_encod$PAY_1)

#make.names(test_df_encod$DEFAULT, unique = FALSE)
#table(test_df_encod$DEFAULT)

#make.names(test_df_encod[11:37])
#table(test_df_encod$LIMIT_BAL_Neg_29999)
#table(test_df$LIMIT_BAL_Neg_29999)



cat("\n","------------------ SVM  --------------","\n")

library(caret)
#setup cross validation
trctrl <- trainControl(method="repeatedcv", number=5, summaryFunction=twoClassSummary, classProbs=TRUE, repeats=3) #n is number of folds in K-cross validation

str(train_df_encod)
set.seed(123)
 

#------------------------------------ SVM model 
library(mltools)
library(kernlab)

svm <- train(form, train_df_encod, method="svmRadial", trControl=trctrl, tuneLength=10, preProc = c("center", "scale"), metric="ROC")

#trained SVM model
names(svm)
svm$coefnames
svm$results
svm$bestTune


kernlab::plot(svm$finalModel, train_df$DEFAULT)
svm$finalModel
svm$contrasts
svm$terms
#-------------------------perf measures on training data
train_pred <- predict(svm, newdata= train_df_encod)
head(train_pred)


levels(train_pred) <- c(0,1)
table(train_pred)

train_var <- train_df_encod$DEFAULT
#table(train_pred, train_var) #table(predicted, actual values)

confusionMatrix(train_pred,train_df$DEFAULT)

#ROC
train_pred <- type.convert(train_pred)
test_roc <- roc(train_df$DEFAULT, train_pred)

#AUC
auc(test_roc)
plot(test_roc)

#-------------------------perf measures on test data
test_pred <- predict(svm, newdata= test_df_encod)
head(test_pred)

levels(test_pred) <- c(0,1)
table(test_pred)

test_var <- test_df_encod$DEFAULT
table(test_pred, test_var) #table(predicted, actual values)

confusionMatrix(test_pred, test_df$DEFAULT)



#ROC
test_pred <- type.convert(test_pred)
test_roc <- roc(test_df$DEFAULT, test_pred)

#AUC
auc(test_roc)
plot(test_roc)




#------------------------------------  Second SVM model  -Explore best tune results
#The final values used for the model were sigma = 0.01172714 and C = 0.25

library(mltools)
library(kernlab)

set.seed(123)

grid <- expand.grid(sigma = c(0.001, 0.01, .015, .02), C = c(0.1, 0.2, 0.25, 0.26, 0.27))

svm_tuned <- train(form, train_df_encod, method="svmRadial", trControl=trctrl, tuneGrid=grid, preProc = c("center", "scale"), metric="ROC")

svm_tuned
names(svm_tuned)
svm_tuned$coefnames
svm_tuned$results
svm_tuned$bestTune





#-------------------------perf measures on training data
train_pred_tune <- predict(svm_tuned, newdata= train_df_encod)
head(train_pred_tune)


levels(train_pred_tune) <- c(0,1)
table(train_pred_tune)

train_var <- train_df_encod$DEFAULT
#table(train_pred, train_var) #table(predicted, actual values)

confusionMatrix(train_pred_tune,train_df$DEFAULT)

#-------------------------perf measures on test data
test_pred_tune <- predict(svm_tuned, newdata= test_df_encod)
head(test_pred_tune)

levels(test_pred_tune) <- c(0,1)
table(test_pred_tune)

test_var <- test_df_encod$DEFAULT
table(test_pred_tune, test_var) #table(predicted, actual values)

confusionMatrix(test_pred_tune, test_df$DEFAULT)
kernlab::plot(test_pred_tune, test_var)


#ROC
test_pred <- type.convert(test_pred)
test_roc <- roc(test_df$DEFAULT, test_pred)

#AUC
auc(test_roc)
plot(test_roc)




#------------------------------------  Third SVM model  -select variables

str(train_df_encod)
library(mltools)
library(kernlab)

set.seed(123)

grid <- expand.grid(sigma = c(0.001, 0.01, .015, .02), C = c(0.1, 0.2, 0.25, 0.26, 0.27))

svm_2 <- train(DEFAULT ~ SEX + EDUCATION + MARRIAGE + PAY_1 + PAY_2 + PAY_3 + PAY_4 + PAY_5 +  LIMIT_BAL_Neg_29999 + LIMIT_BAL_30000_159999 + AGE_25_32 + Avg_Bill_Amt_Neg_56050_188 + Avg_Bill_Amt_189_2846 + Avg_Bill_Amt_2847_7488 + Avg_Bill_Amt_7489_31915 + Avg_Pmt_Amt_0_2832 + Avg_Pmt_Amt_2833_12092 + Avg_Pmt_Ratio_1_1pt17 + Avg_Util_pt0092_pt3673 + Util_Growth_6mo_Neg5_NegPt7 + Util_Growth_6mo_NegPt7_NegPt00075 + Util_Growth_6mo_NegPt00075_0 + Util_Growth_6mo_0_Pt029 + Util_Growth_6mo_Pt029_2 + Max_Bill_Amt + Max_DLQ + Bal_Growth_6mo_neg708323_neg122, train_df_encod, method="svmRadial", trControl=trctrl, tuneGrid=grid, preProc = c("center", "scale"), metric="ROC")

head(svm_2)
svm_tuned
names(svm_tuned)
svm_tuned$coefnames
svm_tuned$results
svm_tuned$bestTune





#-------------------------perf measures on training data
train_pred_2 <- predict(svm_2, newdata= train_df_encod)
head(train_pred_2)


levels(train_pred_2) <- c(0,1)
table(train_pred_2)

train_var2 <- train_df_encod$DEFAULT
#table(train_pred, train_var) #table(predicted, actual values)

confusionMatrix(train_pred_2,train_df$DEFAULT)

#ROC
train_pred <- type.convert(test_pred)
test_roc <- roc(test_df$DEFAULT, test_pred)

#AUC
auc(test_roc)
plot(test_roc)

#-------------------------perf measures on test data
test_pred_2 <- predict(svm_2, newdata= test_df_encod)
head(test_pred_2)

levels(test_pred_2) <- c(0,1)
table(test_pred_2)

test_var2 <- test_df_encod$DEFAULT
table(test_pred_2, test_var2) #table(predicted, actual values)

confusionMatrix(test_pred_2, test_df$DEFAULT)
kernlab::plot(test_pred_2, test_var2)


#ROC
test_pred <- type.convert(test_pred_2)
test_roc <- roc(test_df$DEFAULT, test_pred_2)

#AUC
auc(test_roc)
plot(test_roc)


#------------------------------------  Fourth SVM model  -select variables

str(train_df_encod)
library(mltools)
library(kernlab)

set.seed(123)

grid <- expand.grid(sigma = c(0.001, 0.01, .015, .02), C = c(0.1, 0.2, 0.25, 0.26, 0.27))

svm_4 <- train(form5, train_df_encod, method="svmRadial", trControl=trctrl, tuneGrid=grid, preProc = c("center", "scale"), metric="ROC")

head(svm_4)

svm_tuned$coefnames
svm_tuned$results
svm_tuned$bestTune





#-------------------------perf measures on training data
train_pred_4 <- predict(svm_4, newdata= train_df_encod)
head(train_pred_4)

table(train_pred_4)
levels(train_pred_4) <- c(0,1)
table(train_pred_4)

#train_var2 <- train_df_encod$DEFAULT
#table(train_pred, train_var) #table(predicted, actual values)

confusionMatrix(train_pred_4,train_df$DEFAULT)

#ROC
train_pred <- type.convert(train_pred_4)
test_roc <- roc(train_df$DEFAULT, train_pred_4)

#AUC
auc(test_roc)
plot(test_roc)

#-------------------------perf measures on test data
test_pred_4 <- predict(svm_4, newdata= test_df_encod)
head(test_pred_4)

levels(test_pred_4) <- c(0,1)
table(test_pred_4)

test_var <- test_df_encod$DEFAULT
#table(test_pred_4, test_var2) #table(predicted, actual values)

confusionMatrix(test_pred_4, test_df$DEFAULT)
kernlab::plot(test_pred_4, test_var2)


#ROC
test_pred <- type.convert(test_pred_4)
test_roc <- roc(test_df$DEFAULT, test_pred_4)

#AUC
auc(test_roc)
plot(test_roc)

#-------------visualizations
library(dplyr)
head(test_df_encod)
#plotting tuning behavior
caret:: getModelInfo("svmRadial")$svmRadial$parameters
ggplot(svm_4) + theme_light()

plot(svm_4, train_df_encod)
print(svm_4)






# ------------------------------------------------------ Principal component analysis
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")

sub_cont_var <- cc_default_df[cc_default_df$train==1, c(2, 6, 13:25, 37, 43, 52, 64, 75, 84, 90:93)]

sapply(sub_cont_var, class)

str(sub_cont_var)
cols <- (c(1:25))
sub_cont_var[cols] <- sapply(sub_cont_var[cols], as.numeric)
sapply(sub_cont_var, class)
str(sub_cont_var)

library(pls)
pca <- PCA(sub_cont_var, scale.unit= TRUE, ncp=5, graph=TRUE)
print(pca)
get_pca_var(pca)

#get eigenvalues
eig.val <- get_eigenvalue(pca)
eig.val

#scree plot
fviz_eig(pca, addlabels = TRUE, ylim=c(0,50))
#plot cos2 of variables
var <- get_pca_var(pca) #exract results for variables - list of matrices of all results
fviz_cos2(pca, choice = "var", axes=1:2, top=10)

validationplot(pca, val.type="MSEP")
corrplot(var$contrib, is.corr=FALSE)
#10 components

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(as.factor(pca))

graphics.off()






#---------------------------------------------------------------------Final Log Regression Model
str(train_df)


#final <- as.formula(DEFAULT ~ EDUCATION + Max_DLQ + Avg_Pmt_Amt_0_2832 + Avg_Pmt_Amt_2833_12092 + Avg_Pmt_Amt_12093_627344 + #LIMIT_BAL_Neg_29999 + LIMIT_BAL_30000_159999 + LIMIT_BAL_160000_1000000 + Bal_Growth_6mo_neg708323_neg122 + #Bal_Growth_6mo_neg123_21389 + Bal_Growth_6mo_21390_428792 + Max_Util + Max_Bill_Amt + Avg_Pmt_Ratio_Neg16429_pt02 + #Avg_Pmt_Ratio_pt03_pt15 + Avg_Pmt_Ratio_pt16_1 + Avg_Pmt_Ratio_1_1pt17 + Avg_Pmt_Ratio_1pt18_2688) 

final <- as.formula(DEFAULT ~ LIMIT_BAL + Max_Pmt_Amt + Max_Util + Max_Bill_Amt + Avg_Pmt_Ratio + Max_DLQ)


log_final <- glm(form5, data=train_df, family=binomial)
summary(log_final) 



#-------------------------perf measures on training data
log_probs_final = predict(log_final, train_df, type="response")
summary(log_probs_final)
str(log_probs_final)

glm.pred_final = rep("0", 15180)
glm.pred_final[log_probs_final >.5] = "1"

table(glm.pred_final)
confusionMatrix(as.factor(glm.pred_final), train_df$DEFAULT)



#-----------------------perf measures on test data

#sub_cont_test <- cc_default_df[cc_default_df$test==1, c(2, 6, 13:25, 37, 43, 52, 64, 75, 84, 90:93)]
log_probs_test = predict(log_final, test_df, type="response")

glm.pred_test = rep("0", 7323)
glm.pred_test[log_probs_test >.5] = "1"

confusionMatrix(as.factor(glm.pred_test), test_df$DEFAULT)





