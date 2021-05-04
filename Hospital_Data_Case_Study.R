data1<-read.csv("C:/Users/Dell/Trim III/SM-R/R_Code/Data/Mission Hospital.csv", stringsAsFactors = TRUE)
str(data1)
dim(data1)
head(data1)
tail(data1)
fix(data1)
View(data1)

#Exploration
summary(data1)
library(DataExplorer)
create_report(data1)
library(pastecs)
stat.desc(data1)

# identify a package which can be used to find skewness and kutosis for all the scale variables
library(PerformanceAnalytics)
skew_data<-skewness(data1)

cor.test(data1$AGE, data1$BODY.WEIGHT)

#check the relations
library(psych)
describe(data1)

# For Scaled Variables

# 1 AGE AND BP HIGH
# age - skewness --- 0.43
# bp high skewness-----------0.83
cor.test(data1$AGE, data1$BP..HIGH)

# r = 0.60, p = 0.0000000
# null hypothesis r = 0, as per p value, reject Ho, conclude - There is + relation between age and bp high.

# Visualize the above using ggplot

library(ggplot2)
ggplot(data1,aes(AGE, BP..HIGH)) + geom_point()
ggplot(data1,aes(AGE, BP..HIGH)) + geom_smooth()


# 2 AGE AND WEIGHT
cor.test(data1$AGE, data1$BODY.WEIGHT)
ggplot(data1,aes(AGE, BODY.WEIGHT)) + geom_point()
ggplot(data1,aes(AGE, BODY.WEIGHT)) + geom_smooth()

#curvi-linear relationship


# 3 Amount paid and stay
cor.test(data1$ACTUAL.RECEIVABLE.AMOUNT, data1$LENGTH.OF.STAY..WARD)
ggplot(data = data1, aes(ACTUAL.RECEIVABLE.AMOUNT, LENGTH.OF.STAY..WARD)) + geom_point()
ggplot(data = data1, aes(ACTUAL.RECEIVABLE.AMOUNT, LENGTH.OF.STAY..WARD)) + geom_smooth()


#For Categorical Variables
View(data1)

tab1<-table(data1$GENDER, data1$MARITAL.STATUS)
tab1
prop.table(tab1)
prop.table(tab1,1)
prop.table(tab1,2)

# What is chi square
# Compares two variables in contingency table, categorical var
# test of association   Ho there is no association between gender and marital status or 
# Ho Gender and marital status are independent of each other
chisq.test(tab1)

# p = 0.01217

# graphically

ggplot(data1, aes(GENDER, fill = MARITAL.STATUS)) + geom_bar()


#-------------------------#
tab2<-table(data1$GENDER, data1$TYPE.OF.ADMSN)
tab2
prop.table(tab2)
prop.table(tab2,1)
prop.table(tab2,2)

chisq.test(tab2)
ggplot(data1, aes(GENDER, fill = TYPE.OF.ADMSN)) + geom_bar()
# There is no relation between gender and type of admission

# 1 gender and marital status layer type of admission
# 2 gender and type of admission layer marital status
# 3 marital status and type of admission layer of gender

# 1
library(dplyr)

data_male<-filter(data1, GENDER=="M")
ggplot(data_male, aes(data_male$MARITAL.STATUS, fill= data_male$TYPE.OF.ADMSN))+geom_bar()
tab1<-table(data_male$MARITAL.STATUS, data_male$TYPE.OF.ADMSN)
tab1
prop.table(tab1)
prop.table(tab1,1)
prop.table(tab1,2)
chisq.test(tab1)

data_male<-filter(data1, GENDER=="F")
ggplot(data_male, aes(data_male$MARITAL.STATUS, fill= data_male$TYPE.OF.ADMSN))+geom_bar()
tab1<-table(data_male$MARITAL.STATUS, data_male$TYPE.OF.ADMSN)
tab1
prop.table(tab1)
prop.table(tab1,1)
prop.table(tab1,2)
chisq.test(tab1)

#-------------------
library(mosaic)
library(vcd)
mosaicplot(data1$GENDER)
