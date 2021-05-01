data1  = read.csv("D:\\Akhil MBA\\MBA 3 Trimester\\Statistical Modeling using R\\Classroom_final_sheet.csv", stringsAsFactors = TRUE)
str(data1)

library(DataExplorer)
create_report(data1)

summary(data1)

# through summary() we got to know that there are outliers in Over.all.percentage.in.MBA.till.now and we need to clean it

# getting the relation between previous degree and specialization
tab1 = table(data1$Previous_Degree, data1$Specialization)
tab1
#ho == no relation b/w specilization and pervious degree
chisq.test(tab1)# from this accepting null hypothesis

# single scale variable-- Histogram
library(ggplot2)
ggplot(data1, aes(data1$Percentage_in_10_Class))+geom_histogram()
ggplot(data1, aes(data1$Percentage_in_12_Class))+geom_histogram()
ggplot(data1, aes(data1$Percentage_in_Under_Graduate))+geom_histogram()
ggplot(data1, aes(data1$Over.all.percentage.in.MBA.till.now.))+geom_histogram()

#CTC Affecting academic performance
# (ctc, specilization, work experience, post score, previous degree, % of all the previous scores)

# CTC, academic performance as 10%, 12%, ug%, mba%
# logically percentage of 10%,12%,ug%,mba% are independent variable and ctc is a dependent variable
# relation to tha above logic is true and assuming the relation to be linear
# corelation between independent and dependent variable
# null hypothesis r = 0
# alternate hypothesis r != 0
# testing using t test
# null hypothesis-- there is no relation between two 
# alternate hypothesis--- there is a relaitonship

cor.test(data1$CTC.Offeredin.lakhs.PA, data1$Percentage_in_10_Class)#accepting the null hypothesis saying that there is no relaiton
cor.test(data1$CTC.Offeredin.lakhs.PA, data1$Percentage_in_12_Class)
cor.test(data1$CTC.Offeredin.lakhs.PA, data1$Percentage_in_Under_Graduate)
cor.test(data1$CTC.Offeredin.lakhs.PA, data1$Over.all.percentage.in.MBA.till.now.)

# graphically
ggplot(data1, aes(data1$CTC.Offeredin.lakhs.PA, data1$Percentage_in_10_Class))+geom_point()
ggplot(data1, aes(data1$CTC.Offeredin.lakhs.PA, data1$Percentage_in_10_Class))+geom_smooth()

ggplot(data1, aes(data1$CTC.Offeredin.lakhs.PA, data1$Percentage_in_12_Class))+geom_point()
ggplot(data1, aes(data1$CTC.Offeredin.lakhs.PA, data1$Percentage_in_12_Class))+geom_smooth()

ggplot(data1, aes(data1$CTC.Offeredin.lakhs.PA, data1$Percentage_in_Under_Graduate))+geom_point()
ggplot(data1, aes(data1$CTC.Offeredin.lakhs.PA, data1$Percentage_in_Under_Graduate))+geom_smooth()

ggplot(data1, aes(data1$CTC.Offeredin.lakhs.PA, data1$Over.all.percentage.in.MBA.till.now.))+geom_point()
ggplot(data1, aes(data1$CTC.Offeredin.lakhs.PA, data1$Over.all.percentage.in.MBA.till.now.))+geom_smooth()

# checking whether CTC is normally distributed

library(psych)
library(pastecs)

describe(data1$CTC.Offeredin.lakhs.PA)
stat.desc(data1$CTC.Offeredin.lakhs.PA)
library(ggplot2)  
ggplot(data1, aes(data1$CTC.Offeredin.lakhs.PA))+geom_histogram()

#removing missing values
data1 = na.omit(data1) # in this missing values are removed and stores the rest data in data1
ggplot(data1, aes(data1$CTC.Offeredin.lakhs.PA))+geom_histogram()

# by using normality we can find the outliers

library(fBasics)
library(moments)

qqnorm(data1$CTC.Offeredin.lakhs.PA)
qqline(data1$CTC.Offeredin.lakhs.PA)

boxplot(data1$CTC.Offeredin.lakhs.PA)

shapiro.test(data1$CTC.Offeredin.lakhs.PA) # from this we reject null hypothesis of data is normally distributed

#-------------------------------------------------------------------

describe(data1$Over.all.percentage.in.MBA.till.now.)
stat.desc(data1$Over.all.percentage.in.MBA.till.now.)
ggplot(data1, aes(data1$Over.all.percentage.in.MBA.till.now.))+geom_histogram()

qqnorm(data1$Over.all.percentage.in.MBA.till.now.)
qqline(data1$Over.all.percentage.in.MBA.till.now.)# through this we can say that it is normally distributed and there is only one point which lies outside the line and here we accept the null hypothesis

boxplot(data1$Over.all.percentage.in.MBA.till.now.)

shapiro.test(data1$Over.all.percentage.in.MBA.till.now.)# this shows not normally distributed as there is one point lying outside

#linear regression
# y = bo + b1x1 + b2x2 + b3x3 + b4x4

#y~x

data1 = na.omit(data1)# this line removes the missing observations

reg1 = lm(data1$CTC.Offeredin.lakhs.PA~
            data1$Percentage_in_10_Class+
            data1$Percentage_in_12_Class+
            data1$Percentage_in_Under_Graduate+
            data1$Over.all.percentage.in.MBA.till.now., data = data1)
summary(reg1)# as P>0.05 accepting null hypothesis as no effect of academic performance on ctc offered, as adjisted R^2 is -ve this means that functional formation is wrong
# 

plot(reg1)# This gives outlier numbers, after running this we have go to console and click enter we will get outliers as 46,72,89,61
View(data1)# through this we got to know that error is in row 61 now we will change the percentage of 61st row overall mba % to 65 in the excel file and run the reg1 again
fix(data1)# R^2 coefficient of determination
reg1 = lm(data1$CTC.Offeredin.lakhs.PA~
            data1$Percentage_in_10_Class+
            data1$Percentage_in_12_Class+
            data1$Percentage_in_Under_Graduate+
            data1$Over.all.percentage.in.MBA.till.now., data = data1)
summary(reg1)
plot(reg1)

# removing rows of 46,72,120
data2 = data1[-c(46,72,120),]

data2 = na.omit(data2)
reg1 = lm(CTC.Offeredin.lakhs.PA~
            Percentage_in_10_Class+
            Percentage_in_12_Class+
            Percentage_in_Under_Graduate+
            Over.all.percentage.in.MBA.till.now., data = data2)
summary(reg1)
plot(reg1)
View(data2)
View(data1)


library(car)
outlierTest(reg1)
influenceIndexPlot(reg1)

reg2 = lm(CTC.Offeredin.lakhs.PA~Over.all.percentage.in.MBA.till.now., data = data2)
summary(reg2)

