data1<-read.csv("C:/Users/Dell/MBA/Trim III/SM-R/R_Code/Data/cleaned_var1.csv", stringsAsFactors = TRUE)
str(data1)
summary(data1)

# Co-realtion
# DV -- e(g/km)
# IV -- Mass, Wheel base, axle at1, engine capacity, electric enrgy consumption, ER, engine power at2
# Objective - To create a model which can predict the emission or to find the impact of different variables on emission

library(DataExplorer)
create_report(data1)

dim(data1)

library(caTools)
set.seed(100)

#dividing the data in to parts
split1<-sample.split(data1$id, SplitRatio = 0.70)

datatest<-subset(data1, split1 == FALSE)
datatrain<-subset(data1, split1 == TRUE)

dim(datatest)
dim(datatrain)

#regression - data is linearly related

reg1<-lm(e..g.km.~
           m..kg.+
           w..mm.+
           at1..mm.+
           at2..mm.+
           ec..cm3.+
           ep..KW.,data = datatrain)
summary(reg1)


# ANOVA
# H0: model is not good fitted()
# H0: B1=B2=B3
# H1: atleast one is different


# How much is the effect


#Multi collinearity
library(car)
vif(reg1)

# IF VIF is 2.5 good model
# 4 Avg model
# >10 not good mode

cor.test(datatrain$at1..mm.,datatrain$at2..mm.)
# what should i do
# remove 1 var as both var has vif as almost saem 57 around
# so divide into 2 different models

reg2<-lm(e..g.km.~
           m..kg.+
           w..mm.+
           at1..mm.+
           ec..cm3.+
           ep..KW.,data = datatrain)
summary(reg2)

reg3<-lm(e..g.km.~
           m..kg.+
           w..mm.+
           at2..mm.+
           ec..cm3.+
           ep..KW.,data = datatrain)
summary(reg3)

plot(reg2)

#Checking the outliers
library(car)
out1<-Boxplot(reg2$residuals)
out1

dim(datatrain)
#remove the data points
datatrain<-datatrain[-out1,]
dim(datatrain)

#run the reg2 model and check the model
# with this we have to re run the code again and again to remove the outliers

# So use other library
# using the concept of normal distribution

library(outliers)
#converting the data into standard normal

datatrain$e..g.km.

#into standard normal ----

datatrain$e..g.km.<-scale(datatrain$e..g.km., center = TRUE, scale = TRUE)
datatrain$e..g.km.

# if it is less than -1.96 and more than 1.96 then those values are outliers
which(datatrain$e..g.km.>= 1.96)
which(datatrain$e..g.km.<= -1.96)

Boxplot(datatrain$e..g.km.)

outlier(datatrain$e..g.km.)
#interquartile range - 1.5IQR will remove all the outliers above and below it

IQR(datatrain$e..g.km.,na.rm =TRUE)

quantile()

# Median - 1/2 half part
# Quantile - 1/10 
# Percentile - 1/100


# under outliers fun different ways to remove outliers
# mahalobnis distance, cook distance, standard normal, residual


# Way to  find the models which can be built
# Step wise
# Backward - models based on the var one by one high to low
# Forward - models based on the var one by one low to high

# Always try to follow Step wise as the forward and backward done; the var is added can't be removed

library(MASS)

# STEPAIC - step wise model finding

regmodel<-stepAIC(reg1, direction = "backward", trace = 1)
summary(regmodel)
regmodel$anova

regmodel<-stepAIC(reg1, direction = "forward", trace = 1)
summary(regmodel)
regmodel$anova

regmodel<-stepAIC(reg1, direction = "both", trace = 1)
summary(regmodel)
regmodel$anova

#final model ----


# Other way to chose hte model
library(leaps)
regmodel1<-regsubsets(e..g.km.~
                        m..kg.+
                        w..mm.+
                        at1..mm.+
                        at2..mm.+
                        ec..cm3.+
                        ep..KW.,data = datatrain, nbest = 3)

plot(regmodel1, scale = "r2")
plot(regmodel1, scale = "adjr2") # more no  of IV var
plot(regmodel1, scale = "bic")

# Check for below points
# heteroscadisticity
library(lmtest)


# auto correlation
library(lmtest)


# assume re2, reg3 are the best model - why to divide the data to train and test

# how to generalize the model
# accuracy what is accuracy of model
# sensitivity, specivity

# two tests in general covid
# RTPCR, antigen and true nod

# output is scale -- rmsr
# output is categorical -- matrix

