data1<-read.csv("C:/Users/Dell/MBA/Trim III/SM-R/R_Code/Data/final sheet 1.csv", stringsAsFactors = TRUE)
str(data1)
# Removing missing values
data2<-na.omit(data1)

reg1<-lm(CTC.Offeredin.lakhs.PA~
           Over.all.percentage.in.MBA.till.now., data = data2)
summary(reg1)

# Residual Analysis
# randomly distributed -- no patern
# normally distributed --

plot(reg1)

reg1$coefficients

#Residuals - error term
reg1$residuals
# actual values of y and predicted values of y == error = residual

#Check whether the data is normal or not , normality test for residual
qqnorm(reg1$residuals)
qqline(reg1$residuals)

#Test for normality
shapiro.test(reg1$residuals)

#Plot(x,y)
#randomness in the residuals
plot(reg1$residuals, c(1:length(reg1$residuals)))

library(car)
outlierTest(reg1)
influenceIndexPlot(reg1)


#hetroscadisticity - Same variance - if we plot between fitted value on y axis and residual on x axis thenif the shape formed in graph is cone then it hetro
#homoscadisticity - same variation across the plot - No Pattern

#hetro
plot(reg1$residuals,reg1$fitted.values)
# in graph there is cone in the right side with one value then there is a problem


library(lmtest)
library(fBasics)
library(moments)

?bptest()
bptest(reg1)


#Multicollinearity - when there r 2 or more independent variable and thehere is high corelation
# common varaition between all teh variables nad beta won't be estimtated properly

# IF the variables are having the realtion among them then that is muliti collinearity
# VIF is used to check the same

library(car)
vif(reg1) # reg1 had only x variable (IV)

#If vif is 4 then the problem of multi collineraity is there


