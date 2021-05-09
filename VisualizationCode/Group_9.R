# Use Case --------------------------
  #Air Quality Index report has to be provided to Environmentalist for the analysis of various pollutants 
  # resulting to the pollution for different cities across India. Using the data provided we are analyzing
  # the different variables and its effect on the target variables that is Air Quality Index and formulating
  # the strategies required to improve the pollution levels.

# Business Problem ---------------------
  # To get an estimate of the air quality index across multiple cities by measuring the quantity of 
  #  particulate matter and other harmful pollutants in the air. 

# OBJECTIVES ----------------------------
# 1. To identify the cities that are most and least polluted.
# 2. To assess the air quality index of the cities
# 3. To determine the extent to which the harmful pollutants are present in the air.
# 4. To check the air quality across a span of years in the mentioned cities.
# 5. To analyze the effect of independent variables on the target variable.

# Libraries-----------------------------------
  
#Libraries required for the data to be imported

#Importing the necessary packages

#To read the excel data
library(readxl)

#Library for data exploration
library(DataExplorer)

#Library for Data Visualization
library(ggplot2)

#Library for uni-variate, bi-variate and multi-variate analysis
library(psych) 
library(pastecs)
library(descr)
library(skimr)

#Library for Co-relation
library(corrplot)  


# Data Import--------------------------------------
  
#Importing the Data
pollutionData<-read.csv("C:/Users/Dell/Trim III/SM-R/R_Code/CIA II/Data/Pollution_Data.csv", stringsAsFactors = TRUE)

# Data Exploration --------------------------------

#DESCRIPTION OF VARIABLES:
#1. When the temperature is high, nitrogen and oxygen combine together and form various
  # different oxides. Nitrogen monoxide and nitrogen dioxide are the most abundant.
  # NOx is a combination of Nitrogen monoxide and Nitrogen dioxide.

#2. Nitric oxide is not hazardous in a typical ambient environment. But if 
  # there is excess Nitric oxide in the air, it may cause respiratory ailments,
  # hematologic side effects, metabolic disorders, low blood pressure, nausea, vomiting and diarrhea.

#3. NOx can be formed due to different reactions. Nitrogen oxides are formed mainly 
  # during lightning strikes. Nitrogen oxides are also produced from fuel combustion
  # in automobiles. Stationary emission from coal-fired power plants and electric power
  # boilers also produces Nitrogen oxides. Its from smog and acid rain. The impact of
  # NOx on human health include damage to lung tissue, breathing and respiratory problems.

#4. The atmosphere is often polluted because of particles. These particles are often 
  # floating dust or particulate matters. Particulate matters are seen in different 
  # sizes based on their diameters: the bigger particles, the easier it to wash away.
  # The finer particles stayed for a long time in the atmosphere and transported over a
  # long distance. PM10 and PM2.5 are two types of particulate matter which are
  # increasing day by day, causing air pollution. When you breathe, these particles
  # penetrate into the lungs quickly and causes cough and asthma, high blood pressure,
  # heart attack, strokes etc. which can lead to premature death. High PM2.5 can also
  #increase air mist and fog, which reduces visibility.

#5. Ozone pollutant is not emitted but created by chemical reactions between
  # nitrogen oxide and a volatile organic compound. This reaction happens when
  # pollutants are emitted by cars, power plants, industries, etc. chemically
  # react in sunlight. It can be transported long distances by wind. People most at 
  # risk due to the ozone in the atmosphere include people with asthma, children, older
  # adults and people who do a lot of outdoor activity.

#Structure of the Data
str(pollutionData)

#Gives the shape of the data
dim(pollutionData)

#Finding the no of columns
length(pollutionData)
ncol(pollutionData)

#Finding the no of rows
length(pollutionData$City)
nrow(pollutionData)

# Displaying the data in tabular format
View(pollutionData)

#Data Exploration using the Data Explorer Function
create_report(pollutionData)
#Interpretations of the Report:
  # 1. The data profiling Report describes that there is no missing data in the
    # dataset, the total observation is 99,776.
  
  # 2. we have 19% of discrete columns, 81% of continuous columns and we don't
    # have missing columns or observations in the data, Data structure derives
    # 6236 observation of 16 variables.

  # 3. The uni-variate Distribution histogram for all the pollutants is right
    # skewed. we have two categorical variables which is represented in bar
    # chart it says that hyderabad has noted highest responses followed by delhi.

  # 4. AQI categories observations is noted in order Moderate has the highest
    # followed by Satisfactory, poor, good, very poor, Severe.

  # 5. Q-Q PLOT(Quantile - Quantile) tells about the normal distribution of
    # pollutants PM10 & AQI has highest observations.

  # 6. The correlation analysis for entire data tells that it is a positive right
    # skewed data, C6H6 & O3 have very less correlation & AQI-(PM2.5 & PM 10) have
    # the highest correlation.

  # 7. Principal component analysis (PCA) is a technique used for identification
    # of a smaller number of uncorrelated variables states we have 11 principal
    # components that PC1 is 26% , PC2 is 34.1% and PC11 is 77.7%.

#Overall stats of the entire Data - Descriptive statistics

# It will give the statistical calculation or summary for the data passed
summary(pollutionData)
stat.desc(pollutionData)
describe(pollutionData)
skim(pollutionData)

#Interpretations:
# The top four pollutants responsible for pollution are nitric oxide, 
  # sulphur dioxide, particulate matter 2.5 and particulate matter 10.
  # Their average presence in air is 32.45, 11.514,61.33 and 123.42, respectively.
  # The majority of the observations recorded on a specific day fall into the 
  # Moderate (2521) or Satisfactory (2079) category. However, there are 124
  # observations and 410 observations recorded for the Severe and Very poor
  # Air Quality Index.

#Frequency distribution of categorical variables:
# Uni-variate Analysis
ftable(pollutionData$City)
ftable(pollutionData$Year)
ftable(pollutionData$AQI_Cat)

# Bi-variate Analysis
ftable(pollutionData$City, pollutionData$AQI_Cat)
#Interpretations: 
# From the table, we see that Delhi has the most number of days with a
  # Severe and Very poor quality index.

ftable(pollutionData$City, pollutionData$Year)
ftable(pollutionData$Year, pollutionData$AQI_Cat)


# Multi-variate Analysis
ftable(pollutionData$City, pollutionData$AQI_Cat, pollutionData$Year)
#Interpretations:
  # As per the data available, in 2015, Delhi has the most recorded
    # observations in the category severe and very poor, followed by Hyderabad.
    # In 2016, Hyderabad had a poor air quality index. In 2017, 2018, 2019 and
    # 2020, Delhi again had peak pollution with the maximum number of observations
    # under the category. Here, we can infer Delhi has been the state with consistent
    # pollution.

#Descriptive stats for Individual variables
describeBy(pollutionData, group = pollutionData$City)
describeBy(pollutionData, group = pollutionData$Year)
describeBy(pollutionData, group = pollutionData$AQI_Cat)

# DATA VISUALIZATION -----------------------

#Distribution for the variables responsible for poor AQI

#For "NO" Variable
ggplot(pollutionData, aes(NO)) + geom_dotplot()
ggplot(pollutionData, aes(NO)) + geom_histogram()
ggplot(pollutionData, aes(NO)) + geom_density()

#For "NOX" Variable
ggplot(pollutionData, aes(NOx)) + geom_dotplot()
ggplot(pollutionData, aes(NOx)) + geom_histogram()
ggplot(pollutionData, aes(NOx)) + geom_density()

#For "PM2.5" Variable
ggplot(pollutionData, aes(PM2.5)) + geom_dotplot()
ggplot(pollutionData, aes(PM2.5)) + geom_histogram()
ggplot(pollutionData, aes(PM2.5)) + geom_density()

#For "PM10" Variable
ggplot(pollutionData, aes(PM10)) + geom_dotplot()
ggplot(pollutionData, aes(PM10)) + geom_histogram()
ggplot(pollutionData, aes(PM10)) + geom_density()


#Categorical Variables
ggplot(pollutionData, aes(AQI_Cat))+geom_bar()
ggplot(pollutionData, aes(City))+geom_bar()
ggplot(pollutionData, aes(Year))+geom_bar()

ggplot(pollutionData, aes(City, fill = AQI_Cat))+geom_bar()
ggplot(pollutionData, aes(City, fill = AQI_Cat))+geom_bar(position = "dodge")

# Interpretation
# From the bar graph, it can be observed that Severe Pollution is detected in Delhi.
  # Therefore, it can be said that the Air Quality Index in Delhi is very poor.

ggplot(pollutionData, aes(AQI_Cat, fill = City)) + geom_bar(position =  "dodge")

ggplot(pollutionData, aes(AQI_Cat, fill = City))+geom_bar(position = "stack")+ facet_grid(~pollutionData$Year)

ggplot(pollutionData, aes(Date, AQI)) + geom_point()
ggplot(pollutionData, aes(Date, AQI)) + geom_line()
pollutionData$Date<-as.Date(pollutionData$Date, "%m/%d/%Y")

ggplot(pollutionData, aes(Date, AQI)) + geom_smooth()
#Interpretations:
  # From the graph, it can be observed that in 2015 the AQI is high.
    # However, there is a decrease in the subsequent years. The lowest
    # AQI has been recorded in 2020 because of the lockdown.

#Scatter Plot
ggplot(pollutionData, aes(PM2.5, AQI)) + geom_point()
ggplot(pollutionData, aes(PM2.5, AQI)) + geom_point(aes(color = City))
#Interpretation:
  # The point graph when compared with Air quality index and Particulate 
  # Matter 2.5-micrometer in ug / m3 with funnel distribution of cities, 
  # Delhi and Hyderabad have the maximum points when compared to other cities.

ggplot(pollutionData, aes(PM2.5, PM10)) + geom_point()
ggplot(pollutionData, aes(PM2.5, PM10)) + geom_point(aes(color = City))
#Interpretation:
  # The point graph when compared with Particulate Matter 2.5-micrometer in ug / m3
    # and Particulate Matter 10-micrometer in ug / m3 with funnel distribution
    # of cities, Delhi and Hyderabad have the maximum points when compared to
    # other cities. Its the comparatively same as the previous plot graph.


ggplot(pollutionData, aes(Year, AQI)) + geom_density_2d()

#Uni-variate analysis - Right skewed distribution(pollutants)
#Maximum number of cities are in the moderate AQI category 


#Mapping the pollutants data leading to poor Air Quality Index
pollutants <- pollutionData[, c(4:16)]
str(pollutants)

#Co-relation for the pollutants
corrpollutants <- cor(pollutants)
corrpollutants
corrplot(corrpollutants, method = "color")
corrplot.mixed(corrpollutants, lower.col = "black", number.cex = .7)
heatmap(corrpollutants)

# Interpretations: 
#1. From the map, it is observed that a high positive correlation exists
  # between PM 2.5, PM10, NOx with the air quality index. PM 2.5 has the
  # highest positive correlation with 92%, followed by PM10 with 91% and NOx
  # with 65%. A little correlation exists between Xylene and the air quality
  # index, and it is around 7%.

#2. From the data, we can say that Delhi and Hyderabad are the most affected cities.

# Data Segmentation 
delhiData <- subset(pollutionData, City == "Delhi")
hyderabadData <- subset(pollutionData, City == "Hyderabad")

delhiData$Date<-as.Date(delhiData$Date, "%m/%d/%Y")
hyderabadData$Date<-as.Date(hyderabadData$Date, "%m/%d/%Y")

ggplot(delhiData, aes(Year,AQI))+geom_point()
#Interpretations:
  # For Delhi, the pollution was more in 2018 and 2019. However, in 2020, due to
    # lockdown, the pollution has decreased.
ggplot(hyderabadData, aes(Year,AQI))+geom_point()
#Interpretations:
  # For Hyderabad, the situation is better when compared to Delhi. However,
    # it remains the second most affected state. The pollution has decreased
    # in subsequent years.


ggplot(delhiData, aes(Date, AQI)) + geom_smooth()
ggplot(hyderabadData, aes(Date, AQI)) + geom_smooth()

ggplot(delhiData, aes(PM2.5, AQI)) + geom_point()
ggplot(delhiData, aes(PM2.5, AQI)) + geom_point(aes(color = Year))

ggplot(delhiData, aes(PM2.5, PM10)) + geom_point()
ggplot(delhiData, aes(PM2.5, PM10)) + geom_point(aes(color = Year))

#Co-relation for the pollutants based on highest impacted city

#Delhi Data_Correlation--------------------------------------
#Correlation for Delhi Data
corrDelhi <- cor(delhiData[,4:16])
corrDelhi

#Heatmap plot
corrplot(corrDelhi, method = "color")
corrplot.mixed(corrDelhi, lower.col = "black", number.cex = .7)
heatmap(corrDelhi)
#Interpretations:
# However, apart from PM2.5 and PM10, No2 is responsible for pollution
  # specifically in Delhi. This can be observed from the correlation graph.


#Hyderabad Data_Correlation--------------------------------------
#Correlation for Hyderabad Data
corrHyderabad <- cor(hyderabadData[,4:16])
corrHyderabad

#Heatmap plot
corrplot(corrHyderabad, method = "color")
corrplot.mixed(corrHyderabad, lower.col = "black", number.cex = .7)
heatmap(corrHyderabad)
#Interpretations:
# In Hyderabad, PM2.5 is responsible for the highest pollution as 
  # compared to other pollutants.


# CONCLUSION:--------------------
  # From this dataset of 6236 observations, we found that there are certain cities
    # like Delhi and Hyderabad that have been affected by various pollutants like
    # PM2.5, PM10, and NOx. The pollution levels are different corresponding to
    # different years. Based on the inferences from the data, certain
    # strategies have been formulated along with the ethical, social, and
    # societal issues that the firms face in daily life.

# STRATEGIES:----------------------------
  # 1. If the company utilizes harmful pollutants in its manufacturing processes,
      # the company should also organize regular green drives in an attempt to
      # improve the quality of the air.

  # 2. After the identification of priority pollutants, the company has to identify
      # the measures to control sources of pollution and to implement control measures.

  # 3. Such companies that emit harmful pollutants into the atmosphere must be
      # situated away from the residential areas.

  # 4. To design programs that reduce air pollution from the company's
      # own operations and also from the suppliers.

  # 5. To tighten and regulate the emission standards for the companies.

  # 6. Specifically in Delhi, the most polluted city, the concentration of NO2
      # is higher so the government must place a cap on the NO2 emissions in the
      # state in order to keep the air quality index under control.

  # 7. In Hyderabad, there is a higher concentration of PM 2.5 so the companies
      # in Hyderabad must install chimneys to reduce the particulate matter emissions.

# ETHICAL ISSUES:-----------------
  # 1. Lack of transparency by companies in clearly mentioning the
      # details about their emissions to society.

  # 2. Non-compliance of norms by the companies engaged in harmful emissions
      # by situating their premises in the residential areas due to
      # lack of availability of space.

  # 3. The setting of conflicting goals by the companies in managing air pollution.

# SOCIETAL ISSUES:-----------------
  # 1. PM2.5 is considered harmful for human health and this has a direct
      # impact on the health of the citizens at large especially the vulnerable
      # sectors of the society that are more prone to the respiratory illnesses
      # caused by air pollution.

  # 2. Air pollution also has an impact on renewable energy which in turn affects
      # society at large. Due to the prevalence of smog caused by harmful
      # pollutants, the sunlight cannot fully penetrate and it reduces the
      # solar panels energy output.

# SOCIAL ISSUES:-----------------
  # 1. The surge in harmful emissions by companies across the cities have
      # reached alarming levels and this is a major cause of climate. Climate
      # change is being manifested by an increase in temperatures, ozone layer
      # depletion and global warming.

  # 2 Air pollution harms cognitive functioning across all life stages,
      # from prenatal development to childhood, to adulthood, and even into old age.