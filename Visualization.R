data1<-read.csv("C:/Users/Dell/Trim III/R_Code/Data/final sheet 1.csv")

#Visualization and Exploration
library(graphics)
library(ggplot2)

?plot()

plot(data1$Percentage_in_10_Class,data1$Percentage_in_12_Class,
     main = "Scatter plot for 10th% and 12th%",
     sub = "inferences about the grpahs, higher percentage are scored by 10 and 12",
     xlab = "10th Percentage", ylab = "12th Percentage")

plot(data1$Percentage_in_10_Class,data1$Percentage_in_12_Class, ylim = c(0,400))

#Point type graphs
plot(data1$Percentage_in_10_Class,data1$Percentage_in_12_Class, type = "p")

#Line type graphs
plot(data1$Percentage_in_10_Class,data1$Percentage_in_12_Class, type = "l")

#Histogram type graphs
plot(data1$Percentage_in_10_Class,data1$Percentage_in_12_Class, type = "h")

#Categorical Data
View(data1)
str(data1)

#first convert the data to factor
data1$Gender<-as.factor(data1$Gender)
plot(data1$Gender, type = "h")

data1$Previous_Degree<-as.factor(data1$Previous_Degree)
plot(data1$Previous_Degree)

#ggplot 2
library(ggplot2)

?ggplot2()

ggplot(data = data1, mapping = aes(data1$Percentage_in_10_Class,data1$Percentage_in_12_Class))+geom_point()
ggplot(data = data1, mapping = aes(Percentage_in_10_Class,Percentage_in_Under_Graduate))+geom_point()

#If students are scoring same values, so use geom_jitter - adds randomness to the data
ggplot(data = data1, mapping = aes(Percentage_in_10_Class,Percentage_in_Under_Graduate))+geom_jitter()

ggplot(data = data1, mapping = aes(sort(Percentage_in_10_Class),Percentage_in_Under_Graduate))+geom_line()

ggplot(data = data1, mapping = aes(Percentage_in_10_Class,Percentage_in_Under_Graduate))+geom_smooth()

#Single Scale variables
ggplot(data = data1, mapping = aes(Percentage_in_10_Class))+geom_dotplot()
ggplot(data = data1, mapping = aes(Percentage_in_10_Class))+geom_histogram()
ggplot(data = data1, mapping = aes(Percentage_in_10_Class))+geom_density()
ggplot(data = data1, mapping = aes(Percentage_in_10_Class))+geom_freqpoly()

ggplot(data = data1, mapping = aes(Percentage_in_12_Class))+geom_dotplot()
ggplot(data = data1, mapping = aes(Percentage_in_12_Class))+geom_histogram()

#Categorical variables
ggplot(data = data1, mapping = aes(Gender))+geom_bar()
ggplot(data = data1, mapping = aes(Previous_Degree))+geom_bar()
ggplot(data = data1, mapping = aes(Marital_status))+geom_bar()

#two variables
ggplot(data = data1, mapping = aes(Marital_status, fill = Previous_Degree))+geom_bar()
ggplot(data = data1, mapping = aes(Gender, fill = Previous_Degree))+geom_bar()
