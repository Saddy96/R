data1<-read.csv("C:/Users/Dell/MBA/Trim III/SM-R/R_Code/Data/final sheet 1.csv", stringsAsFactors = TRUE)
str(data1)

library(DataExplorer)

DataExplorer::create_report(data1)
summary(data1)

# outliers, influencial observtion , missing values
# clean 80%

tab1<-table(data1$Specialization, data1$Previous_Degree)
tab1
chisq.test(tab1)


# single variate - histogram

library(ggplot2)
ggplot(data1, aes(data1$Percentage_in_10_Class))+geom_histogram()
ggplot(data1, aes(data1$Percentage_in_12_Class))+geom_histogram()
ggplot(data1, aes(data1$Percentage_in_Under_Graduate))+geom_histogram()
ggplot(data1, aes(data1$Over.all.percentage.in.MBA.till.now.))+geom_histogram()

# CTC AFFECTING ACADEMIC PERFORMANCE

# (ctc, specilization, work experience, post scores, previous degree, % of all p)

#ctc, aacademic performance as 10%, 12%, UG, and MBA)
# identify the IV and DV

#relations --- yes, which type of relation assumeing hte relation to be liner
# corerelaion between IV and DV

cor.test(data1$CTC.Offeredin.lakhs.PA, data1$Percentage_in_10_Class)
cor.test(data1$CTC.Offeredin.lakhs.PA, data1$Percentage_in_12_Class)
cor.test(data1$CTC.Offeredin.lakhs.PA, data1$Percentage_in_Under_Graduate)
cor.test(data1$CTC.Offeredin.lakhs.PA, data1$Over.all.percentage.in.MBA.till.now.)

#Before applying regression or corelation
# Statistical
# dependent variable - CTC, Scales
# Independent Variable - 10, 12, UG and PG % : Scale. Scale +categoricals >> Dummy variable

# Assumptions Linearity
# IV is related to DV
# y = f(x1,x2,x3,...)
# y = B0+b1x1+b2x2+......
# B0 regression co efficient


# y ~ a1+b11x1 linear related

# y = B0+b1x1+b2x2+...... - not linear related

library(ggplot2)

# graphically - 

ggplot(data = data1, aes(data1$CTC.Offeredin.lakhs.PA, data1$Percentage_in_10_Class)) + geom_point()
ggplot(data = data1, aes(data1$CTC.Offeredin.lakhs.PA, data1$Percentage_in_10_Class)) + geom_smooth()

ggplot(data = data1, aes(data1$CTC.Offeredin.lakhs.PA, data1$Percentage_in_12_Class)) + geom_point()
ggplot(data = data1, aes(data1$CTC.Offeredin.lakhs.PA, data1$Percentage_in_12_Class)) + geom_smooth()

ggplot(data = data1, aes(data1$CTC.Offeredin.lakhs.PA, data1$Percentage_in_Under_Graduate))+geom_point()
ggplot(data = data1, aes(data1$CTC.Offeredin.lakhs.PA, data1$Percentage_in_Under_Graduate))+geom_smooth()
ggplot(data = data1, aes(data1$CTC.Offeredin.lakhs.PA, data1$Over.all.percentage.in.MBA.till.now.))+geom_point()
ggplot(data = data1, aes(data1$CTC.Offeredin.lakhs.PA, data1$Over.all.percentage.in.MBA.till.now.))+geom_smooth()



#Normality:
# Mean = Median = Mode (Nearly)
# Plot histogram (bell shaped kind)
# skewness - symmetry
# kurtosis - mesokatic

# Range value -1 to 1
# PP lot and QQplot 
# Normality test


# Residual R^2 (coefficient of Determination)

# Normality check
library(psych)
library(pastecs)

# Missing Values - 
# ignore - na.omit if the data missing is less 10%
# Imputation - if the missing values are more then 10 replace by mean, median, mode 

#Ignoring the missing data
data1<-na.omit(data1)
ggplot(data1, aes(data1$CTC.Offeredin.lakhs.PA))+geom_histogram()
describe(data1$CTC.Offeredin.lakhs.PA)
stat.desc(data1$CTC.Offeredin.lakhs.PA)

# normality - can we identify the outliers - yes

library(fBasics)
library(moments)

qqnorm(data1$CTC.Offeredin.lakhs.PA)
qqline(data1$CTC.Offeredin.lakhs.PA)
# if all the points lie on the line then its means data is normally distributred

boxplot(data1$CTC.Offeredin.lakhs.PA)
# circilar points values are outliers
# Which has to be removed or remagnituted to make it to range

# TO check if the data is normally distributed
shapiro.test(data1$CTC.Offeredin.lakhs.PA)
# Null - data is normally distributed
# Al - Data is not normally distributed
# As per the value p is very small so it reject null so the data are not normally distributed

#--------------------
ggplot(data1, aes(data1$Over.all.percentage.in.MBA.till.now.))+geom_histogram()

qqnorm(data1$Over.all.percentage.in.MBA.till.now.)
qqline(data1$Over.all.percentage.in.MBA.till.now.)

# Seems to be norammly distribute - one or two outliers r there which can be removed

shapiro.test(data1$Over.all.percentage.in.MBA.till.now.)

