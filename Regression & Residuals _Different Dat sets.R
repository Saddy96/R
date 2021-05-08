data1<-read.csv("C:/Users/Dell/MBA/Trim III/SM-R/R_Code/Data/Mission Hospital.csv", stringsAsFactors = TRUE)
str(data1)
View(data1)
#DV - Total cost to the hospital, IV - age, hb, urea, creatline, ....scale variables

# training and testing data sets
# before applying the model clean the data, missing, outliers, should be removed

# Check missing values
summary(data1) # check the colmuns having NA

#removing missing values
data2<-na.omit(data1)
summary(data2)

# training and testing data sets
# Divide the data to 70 and 30
library(caTools)
#set data randomly to have a same randomness across all the system- same random divided data 
set.seed(100)

# divide hte data based on var two parts and ratio is 70 and 30
split1<-sample.split(data2$SL., SplitRatio = 0.70)

#Will store all the 70 and 30 % data
split1

#training the dataset

#split = TRUE - Training(70%)
#split = FALSE - Testing(30%)
data2train<-subset(data2, split1 == TRUE) #70%
data2test<-subset(data2, split1 == FALSE) #30%

dim(data2train)
dim(data2test)

reg1<-lm(TOTAL.COST.TO.HOSPITAL~
           AGE+
           HR.PULSE+
           BP..HIGH+
           HB+
           UREA+
           CREATININE+
           TOTAL.LENGTH.OF.STAY, data = data2train)
summary(reg1)

# THere is 61.3% variaiton to the DV total cost to teh haospital from the IV all var
# Null - there is relation between variables

# if p is less than 0.05 then reject null

#For HR.PULSE
# H0 : B3 =0 no relation
# H1: B1 != 0 impact of HR.pulse on total cost

# P is greater than 0.05 so accept null

# DO the same for all the variables
# e.g for other variables like Total l of stay increase i.e 1 day increase increases the cost by 15270.76


# multicollinearlity
library(car)
vif(reg1)

# There is no problem of multi collinearity as the values are less
# Different values cut off - 2.5,4,6,8,10

# Hence no multi colinearity

plot(reg1)
# THere was a problem in hetroscadicity as per the plot as there was a cone shape in teh data

# Auto corelation--- if x var has some vlaues then if the values of x itself has some corelationbetween its value or hte previous value of x then it auto corelation
#error term shouldn't have auto corealtion
# DW Rule for auto corealtion - if less than 2 then -ve
# if it 2 then no corealtion
# If >2 then +ve auto corelation
library(lmtest)
dwtest(reg1)
# there is no auto corelation as the value is 1.68

#hit and trial method as we r checking the p value and then remvinvg the var
reg1<-lm(TOTAL.COST.TO.HOSPITAL~
           AGE+
           BP..HIGH+
           TOTAL.LENGTH.OF.STAY, data = data2train)
summary(reg1)

reg1<-lm(TOTAL.COST.TO.HOSPITAL~
           AGE+
           TOTAL.LENGTH.OF.STAY, data = data2train)
summary(reg1)

# to fix above issue we use step wise regression
# step wise regression----------
library(leaps)
reg2<-regsubsets(TOTAL.COST.TO.HOSPITAL~
                   AGE+
                   HR.PULSE+
                   BP..HIGH+
                   HB+
                   UREA+
                   CREATININE+
                   TOTAL.LENGTH.OF.STAY, data = data2train, nbest = 3)
#nbset = 3 (7c3 combination)
reg2
summary(reg2)
plot(reg2, scale ="r2")
plot(reg2, scale ="adjr2")
plot(reg2, scale = "bic")

# If the 
# 2IV - R2 = 81
# 3IV - R2 = 62
# If there is single IV then use R2
# IF there are more then use adjusted R2

# The bset model is the top values



