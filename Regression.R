data1<-read.csv("C:/Users/Dell/MBA/Trim III/SM-R/R_Code/Data/final sheet 1.csv", stringsAsFactors = TRUE)
str(data1)


# Linear regression
# y = b0+b1x1+b2x2+b3x3+b4x4

# y `x

reg1<-lm(data1$CTC.Offeredin.lakhs.PA~
           data1$Percentage_in_10_Class+
           data1$Percentage_in_12_Class+
           data1$Percentage_in_Under_Graduate+
           data1$Over.all.percentage.in.MBA.till.now.)

# OR

#Ignoring the missing data
data1<-na.omit(data1)

reg1<-lm(CTC.Offeredin.lakhs.PA~
           Percentage_in_10_Class+
           Percentage_in_12_Class+
           Percentage_in_Under_Graduate+
           Over.all.percentage.in.MBA.till.now., data = data1)
summary(reg1)


# built with the model

#f-statistics 
#Anova
# H0: B1=B2=b3=B4=0
# H1: at least one is not zero

# Ho - model is not good fit
# H1 model is fit

# p is greater than 0.5 so accept null

# Discard the model as the mul R squared is only 2 %


plot(reg1)

# The outliers points from the plot 46 72 61 89

fix(data1)
# 46 - salary 12 lacs
# 72 - salary 12 lacs
# 61 - overall percentage in mba was 6.5 chnaged it to 65
# 89 - salary 10 lacs

# Change the values of the outliers
# CHnaged the vale from 6.5 to 65 of overall mba percentage


#R 2 = is coefficient of determination
# y = a+bx
# R2 = 80%
# Y and x is 
# x is creating 80% variation y
# Y and x are depected by 80%

# Estimate columsn are intercept and p value is the last column


#After changing the value from the table it was seen that 61 outliers was removed from the graph
plot(reg1)

#Now remove the remaining outliers
data2<-data1[-c(46,89,72),]
fix(data2)
reg2<-lm(CTC.Offeredin.lakhs.PA~
           Percentage_in_10_Class+
           Percentage_in_12_Class+
           Percentage_in_Under_Graduate+
           Over.all.percentage.in.MBA.till.now., data = data2)
summary(reg2)
plot(reg2)

library(car)
outlierTest(reg2)
influenceIndexPlot(reg2)

# As we already know that mba marks are affecting the model so removing the other columns

data3<-data2[-c(101),]
fix(data3)
reg3<-lm(CTC.Offeredin.lakhs.PA~
           Over.all.percentage.in.MBA.till.now., data = data3)
summary(reg3)
plot(reg3)
outlierTest(reg3)
influenceIndexPlot(reg2)

fix(data3)


#residual analysis
