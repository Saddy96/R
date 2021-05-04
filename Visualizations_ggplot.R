#Convert character var to string

data1<-read.csv("C:/Users/Dell/Trim III/SM-R/R_Code/Data/final sheet 1.csv", stringsAsFactors = TRUE)

str(data1)
library(DataExplorer)
library(ggplot2)

#Explore the data grapically, Summar, desc, stats desc

DataExplorer::create_report()
create_report(data1)


#psych - Library for explore scaled data - histrogram ,scatter plot
library(psych)
psych::describe(data1)

#grouping based on gender
psych::describeBy(data1,group = data1$Gender)

describeBy(data1$Percentage_in_10_Class, group = data1$Gender)

#Categorical data- frequency distribution table, prop, table,1,2, cross tab, visualization - Bar or multiple bar
#ggplot(data1, mapping = aes(x,y)) + geom object
#two scale variables 0%, 12%,, scatter plot

#Categorical
ggplot(data1, aes(Previous_Degree, fill = Gender))+
    geom_bar()

ggplot(data1, aes(Previous_Degree, fill = Gender))+
    geom_bar(position =  "dodge")
ggplot(data1, aes(Previous_Degree, fill = Gender))+
  geom_bar(position =  "stack")
ggplot(data1, aes(Previous_Degree, fill = Gender))+
  geom_bar(position =  "fill")

# 3 categorical
ggplot(data1, aes(Previous_Degree, fill = Gender))+
  geom_bar() + facet_grid(~Place_you_belong_to)

# 4 variables
ggplot(data1, aes(Previous_Degree, fill = Gender))+
  geom_bar() + facet_grid(~Place_you_belong_to + STATE)

#~ is formula or relationship
ggplot(data1, aes(Previous_Degree, fill = Gender))+
  geom_bar() + facet_grid(Place_you_belong_to~STATE)

ggplot(data1, aes(Previous_Degree, fill = Gender))+
  geom_bar() + facet_grid(Place_you_belong_to~STATE+Work_Experience)

ggplot(data1, aes(Previous_Degree, fill = Gender))+
  geom_bar() + facet_grid(Specialization+Place_you_belong_to~STATE+Work_Experience)


ggplot(data1, aes(Specialization, fill = Gender)) + geom_bar() + facet_wrap(~Place_you_belong_to+Marital_status)

#one scale and one categorical ## always use categorical var first 
ggplot(data1, aes(Gender, Percentage_in_10_Class)) + geom_boxplot()
#... can be outliers, box size variations
ggplot(data1, aes(Gender, Percentage_in_10_Class)) + geom_count()
ggplot(data1, aes(Gender, Percentage_in_10_Class)) + geom_violin()

# 2 var - scaled data
ggplot(data1, aes(Percentage_in_10_Class, Percentage_in_12_Class))+geom_point()
ggplot(data1, aes(Percentage_in_10_Class, Percentage_in_12_Class))+geom_point(aes(color=Gender))
ggplot(data1, aes(Percentage_in_10_Class, Percentage_in_12_Class))+geom_point()+facet_grid(data1$Gender)
ggplot(data1, aes(Percentage_in_10_Class, Percentage_in_12_Class))+geom_point(aes(color=Gender,shape = Specialization))

ggplot(data1, aes(Percentage_in_10_Class, Percentage_in_12_Class))+geom_point(aes(shape=Gender))

ggplot(data1, aes(Percentage_in_10_Class))+geom_histogram()
ggplot(data1, aes(Percentage_in_10_Class))+geom_freqpoly()
ggplot(data1, aes(Percentage_in_10_Class))+geom_histogram()+geom_freqpoly()

ggplot(data1, aes(Percentage_in_10_Class, Percentage_in_12_Class))+geom_bin2d()+geom_point()
ggplot(data1, aes(Percentage_in_10_Class, Percentage_in_12_Class))+geom_point()+geom_bin2d()


#Using plot()
plot()

#mosaic,, mosaicplot()