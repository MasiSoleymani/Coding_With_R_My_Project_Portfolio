
# ----------calling different libraries for visualization, data wrangeling,and more
library("dplyr") 
library("ggplot2") 
library("scales") 
library("zoo")
library("tidyverse")
library("tidyr")
library("lubridate")
library(car) 
require(stats)
library(corrplot)
library(caTools)
library(MLmetrics)
library("repr")

#----------------Raw Data: Uploading and Reading Date
getwd()
MyData <- read.csv("Walmart_Store_sales.csv")
head(MyData)
View(MyData)

# see demintion of data; Number of rows and columns , and class of data
dim(MyData)
class(MyData)


# Data Preparation & Exploratory Data Analysis - data structure 
str(MyData)

#statistical summary of data:
summary(MyData)

#Finding missing values using is.na function
colSums(is.na(MyData))

##Finding duplicate values 
all(duplicated(MyData) == TRUE)


#-----------------Basic Statistics tasks----------
#---------------Q1: Which store has maximum sales--------------


Max_StoreWeekSale<- aggregate(Weekly_Sales ~ Store, data = MyData, sum)
colnames(Max_StoreWeekSale)[2] <- "Total_Sales"
Max_StoreWeekSale <-arrange(Max_StoreWeekSale, desc(Total_Sales))
Max_StoreWeekSale[1,]
View(Max_StoreWeekSale)

#  Which store has maximum sales?
# As shown in the View function output Store no. 20 has the maximum sales and the value is =  301397792.46.

Max_StoreWeekSale[2,]
Max_StoreWeekSale[45,]

#Store 4 is the second largest store in terms of sales and the value is 29,9543,953 
#Store 33 has the least sales 37,160,222 


#--Q2:A) Which store has maximum standard deviation i.e., the sales vary a lot. Also, B)find out the coefficient of mean to standard deviation--------

#A)  Standard Deviation of stores' Weekly_Sales:

StoreSales_Variation<-summarise(group_by(MyData,Store),sd(Weekly_Sales), mean(Weekly_Sales))

#Changing column names
colnames(StoreSales_Variation)[2] <- "StandardDeviation_Sales_by_Store"
colnames(StoreSales_Variation)[3] <- "Mean_Sales_by_Store"

#B) Creating Coefficient of Variation for Sales by Store in StoreSales_Variation dataframe 
StoreSales_Variation<- mutate(StoreSales_Variation,CoefSD_Sales_by_Store = (StandardDeviation_Sales_by_Store/Mean_Sales_by_Store)*100)

#Store with highest Standard deviation
StoreSales_Variation[which.max(StoreSales_Variation$StandardDeviation_Sales_by_Store), ]

#Storing store number with max std deviation value
store_sales_max_std <- StoreSales_Variation[which.max(StoreSales_Variation$StandardDeviation_Sales_by_Store), ]$Store

max_sd <- StoreSales_Variation[which.max(StoreSales_Variation$StandardDeviation_Sales_by_Store), ]$StandardDeviation_Sales_by_Store

#Storing CV value for max std deviation
CoefSD_max_sd <- StoreSales_Variation[which.max(StoreSales_Variation$StandardDeviation_Sales_by_Store), ]$CoefSD_Sales_by_Store

view(StoreSales_Variation)

#Arranged StandardDeviation_Sales_by_Store in descending order
StoreSales_Variation <-arrange(StoreSales_Variation, desc(StandardDeviation_Sales_by_Store)) 

#Choosing the first store that comes in this order
StoreSales_Variation[1,] 
view(StoreSales_Variation)

#Output:
"Store no.  14 has the maximum standard deviation of  317569.95 Coefficient of Variation =  15.713674.

#--------------Q3- Which store/s has good quarterly growth rate in Q3’2012----------  

#Creating new dataframe to do alterations 
"""

SecondDataFrame<-MyData

view(SecondDataFrame)

#Creating a month- year column in SecondDataFrame : 4 and 10 in the code bewlow refers to the Quarter 2 and 3.
SecondDataFrame$month_Year = substr(SecondDataFrame$Date, 4, 10)



Q3_2012 <- filter(SecondDataFrame,month_Year == "07-2012" | month_Year== "08-2012" | month_Year== "09-2012")
Q2_2012 <- filter(SecondDataFrame,month_Year == "04-2012" | month_Year== "05-2012" | month_Year== "06-2012")


#Aggregating sales by store for Q3-2012 
Q3_2012_Sales<-summarise(group_by(Q3_2012,Store),sum(Weekly_Sales))

#Changing column names
colnames(Q3_2012_Sales)[2] <- "Q3_2012_Sales_by_Store"



#Aggregating sales by store for Q2-2012 
Q2_2012_Sales<-summarise(group_by(Q2_2012,Store),sum(Weekly_Sales))

#Changing column names
colnames(Q2_2012_Sales)[2] <- "Q2_2012_Sales_by_Store"

#merging two quarters data by store
Q3_2012_Growthrate <- merge ( Q2_2012_Sales , Q3_2012_Sales , by = 'Store')

#Creating Growth rate column for Sales by Store in the above dataframe 
Q3_2012_Growthrate <- mutate(Q3_2012_Growthrate, Growth_Rate = ((Q3_2012_Sales_by_Store - Q2_2012_Sales_by_Store)*100) / Q2_2012_Sales_by_Store)


#Creating only positive growth rates
positive_growthrate <- filter(Q3_2012_Growthrate, Growth_Rate > 0 ) 
positive_growthrate<-arrange(positive_growthrate, desc(Growth_Rate)) 
View(positive_growthrate)
a<- positive_growthrate$Store

#insight:
#as shown in the positive_growthrate database, Stores  7, 16, 35, 26, 39, 41, 44, 24, 40, 23 has the positive growth rate
#Store 7 has highest growth rate which  is 13.3307760.

# Rest of stores has negative growth rate:

#Creating  Negative growth rates
Negative_growthrate <- filter(Q3_2012_Growthrate, Growth_Rate < 0 ) 
Negative_growthrate<-arrange(Negative_growthrate, desc(Growth_Rate)) 
View(Negative_growthrate)
a<- Negative_growthrate$Store

#insight:
#Store 14 has the highest negative growth rate.


#-----------Q4- Some holidays have a negative impact on sales. Find out holidays which have higher sales than the mean sales in non-holiday season for all stores together----

#Creating Holidays Data dataframe
Holiday_date <- c("12-02-2010", "11-02-2011", "10-02-2012", "08-02-2013","10-09-2010", "09-09-2011", "07-09-2012", "06-09-2013","26-11-2010", "25-11-2011", "23-11-2012", "29- 11-2013","31-12-2010", "30-12-2011", "28-12-2012", "27-12-2013")
Events <-c(rep("Super Bowl", 4), rep("Labour Day", 4),rep("Thanksgiving", 4), rep("Christmas", 4))
Holidays_Data <- data.frame(Events,Holiday_date)
view(Holidays_Data)

#merging both dataframes
data3<-merge(MyData,Holidays_Data, by.x= "Date", by.y="Holiday_date", all.x = TRUE)
view(data3)

# in the column created titled Events there are some null values: Replacing null values in Event with No_Holiday  

data3$Events = as.character(data3$Events) 
data3$Events[is.na(data3$Events)]= "No_Holiday" 
view(data3) 

# or:
head(data3)

#Creating dataframe to find the mean of sales for Non_Holiday and Holiday.
Holiday_Sales<-aggregate(Weekly_Sales ~ Events, data = data3, mean)
#Changing Weekly_Sales column's name. Since it is the second column in Holiday_Sales data frame, no 2 is used in the code:
colnames(Holiday_Sales)[2] <- "Mean_Sales_by_Event_Type"
View(Holiday_Sales)
head(Holiday_Sales)

# Christmas and Labour Day has negative impact on sales whereas Thanks giving and Super Bowl has positive impact on sales

# Checking negative impact based on holiday date and non- holiday date
#Filtering holiday dates and finding mean of Weekly Sales 
Holiday_date <- filter(data3,Holiday_Flag ==1)
Holiday_Date_Sales<-summarise(group_by(Holiday_date,Date),mean(Weekly_Sales))
head(Holiday_Date_Sales)


#Caluclating mean of Weekly Sales for non holidays
mean_non_holiday_sales <- mean(filter(data3,Holiday_Flag ==0)$Weekly_Sales) 
Holiday_Date_Sales$higher_than_non_holiday <- Holiday_Date_Sales[,2] > mean_non_holiday_sales
View(Holiday_Date_Sales)

"""Insight:
As shown in the Holiday_Date_Sales data frame, 
Among the varioue holidays, Super Bowl, Thanks giving, and Labour day have higher sales than mean sales of  Non Holiday dates and creating positive impact on sales.
Date that created negative impact on sales includes: 9th Sept 2011, 10th Sept 2010 ,30th Dec 2011, 31st Dec 2010.
Christmas related dates (30- Dec-2011, 31-Dec-2010 ) have low sales than mean and negative impact on sales, whereas all the dates related to Super Bowl and Thanks giving have high sales than mean.
Labour Day has overall positive impact on sales inspite of having two days below mean value.


# It is surprising that  Christmans related dates has low sales than sales mean. 
To dive deeper we need to compute the dates before Christmas related dates, that is date prior to 30- Dec-2011, 31-Dec-2010.
"""

weekly_sales <- aggregate(Weekly_Sales~Date, data=MyData,mean)
weekly_sales$Date <-as.Date(weekly_sales$Date, "%d-%m-%Y")
weekly_sales <-arrange(weekly_sales,Date)
weekly_sales$Date <-factor(weekly_sales$Date)


options(repr.plot.width = 14, repr.plot.height = 8)

# plotting weekly mean sales
d <- ggplot(data=weekly_sales, aes(x=Date, y=Weekly_Sales, group=1)) +
  geom_line(color="springgreen")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_discrete(breaks = levels(weekly_sales$Date)[c(T, rep(F, 9))])+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Week") + ylab("Mean Sales of Week")

#Plotting Christmas
d +ggtitle('CHRISTMAS')+
  geom_point(aes(x = factor("2010-12-31"), y = 898500.4), color = "red", size = 2) +
  geom_point(aes(x = factor("2011-12-30"), y = 1023165.8), color = "red", size = 2) +
  geom_hline(aes(yintercept = mean_non_holiday_sales), linetype="dashed")

"""Insight
According to the plot created, daets prior to Christmas related dates tat we calculated above, that is dates before 30- Dec-2011, 31-Dec-201 , have 
high sales. So that is why the Christmas dates mentioned above, that is 30- Dec-2011, 31-Dec-2010  have low sales than mean and negative impact on sales.
In other words, since customer do their shopping before 30- Dec-2011, 31-Dec-201, these two dates has a low sales. 
"""

#-------Q5- Provide a monthly and semester view of sales in units and give insights------

#A)Monthly Sales
##Converting date into factor

x<-as.factor(SecondDataFrame$Date)
head(x)

#defining what is the original format of  date
original_format_date<-strptime(x,format="%d-%m-%Y")

#defining what is the desired format of your date
SecondDataFrame$Mon_Year<-as.Date(original_format_date,format="%Y-%m-%d")

SecondDataFrame$Mon_Year = as.yearmon(SecondDataFrame$Mon_Year)
view(SecondDataFrame)

#Aggregating data by 'Month -Year' and Finding sum of Sales and convrting it into dataframe
#creating data frame
Month_Year_Sales<-summarise(group_by(SecondDataFrame,Mon_Year),sum(Weekly_Sales))
# changing the tile of second column of Month_Year_Sales data frame to Sales_by_Month"
colnames(Month_Year_Sales)[2] <- "Sales_by_Month"

#either: 
view(Month_Year_Sales)
#or:
Month_Year_Sales<- as.data.frame(Month_Year_Sales)


#Converting year-mon to factor for plotting so that order wont change
Month_Year_Sales$Mon_Year<- as.character(Month_Year_Sales$Mon_Year)
Month_Year_Sales$Mon_Year<- factor(Month_Year_Sales$Mon_Year, levels=Month_Year_Sales$Mon_Year)




p <- ggplot(data=Month_Year_Sales, aes(x=Mon_Year, y=Sales_by_Month, group=1)) +
  geom_line(color="tomato")+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  ggtitle('Monthly Sales - 2010 to 2012')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Month") + ylab("Total Sales in a Month")
p


"""Insights:
  As it is expected, the sales are highest in December and Lowest in January. 
 Obviously, due to Christmas holiday season sales increases in December.   
"""


#B) Sales by Semester

#converting to date format
SecondDataFrame$Date <- dmy(SecondDataFrame$Date)
#creating semester column with year
SecondDataFrame$sem <- semester(SecondDataFrame$Date, with_year=TRUE)
view(SecondDataFrame)

#creating a dataframe 'SalesbySem' which has total sales for every sem
SalesbySem <- aggregate(Weekly_Sales~sem,data=SecondDataFrame, sum)
view(SalesbySem)

# Addding a new column by Rewriting semester and year to a different format
SalesbySem$sem_year <- paste(substr(SalesbySem$sem,1,4),'-S',substr(SalesbySem$sem,6,6),sep = '')
view(SalesbySem)

#Plotting the graph semester vs Sales
q <- ggplot(data=SalesbySem, aes(x=sem_year, y=Weekly_Sales, group=1)) +
  geom_line(color="slateblue4")+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  ggtitle('Semester Sales - 2010 to 2012')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Semester") + ylab("Total Sales in a Semester")
q

"""Insights
The sales are higher in second semester of every year.
A steep drop in sales is seen in the  first semester of 2010 due to lack of data for th January 2010
and a steep drop in sales seen for the second semester of 2012 due to absence of Nov-Dec 2012 data.
"""

#-------Q6: For Store 1 – Build  prediction models to forecast demand--------
#creating new data frame using main data frame

data4 <- MyData


#using filter function selecting  store 1 as prediction required only for this Store
data4<- dplyr::filter(data4, Store ==1)

# changing the current  date  format  to the new format:  year, month, and day format using lubridate function  and arranging date in ascending order as per dates
data4$Date <- lubridate::dmy(data4$Date)
data4 <- dplyr::arrange(data4,Date)


#Creating a Week_number column in dataframe
data4$Week_Number <- seq(1:length(unique(data4$Date)))

#adding quarter & month columns
data4$month <- lubridate::month(data4$Date)
data4$quarter <- lubridate::quarter(data4$Date)



# creating Holiday_date vector
Holiday_date <- c("12-02-2010", "11-02-2011", "10-02-2012", "08-02-2013","10-09-2010", "09-09-2011", "07-09-2012", "06-09-2013","26-11-2010", "25-11-2011", "23-11-2012", "29-11-2013","31-12-2010", "30-12-2011", "28-12-2012", "27-12-2013")

#assigning date format to Holiday_date vector using lubridate to make it similar to date format in Data4 dataset.
Holiday_date <- lubridate::dmy(Holiday_date)

#Creating Events vector
Events <-c(rep("Super Bowl", 4), rep("Labour Day", 4),rep("Thanksgiving", 4), rep("Christmas", 4))

#Creating new dataframe  with two newly created data : Events and Holiday_date showing date of each holiday.
Holidays_Data <- data.frame(Events,Holiday_date)


#merging both dataframes, data4 and Holidays_Data
data4<-merge(data4,Holidays_Data, by.x= "Date", by.y="Holiday_date", all.x = TRUE)

#Replacing null values in Event with No_Holiday
data4$Events = as.character(data4$Events)
data4$Events[is.na(data4$Events)]= "No_Holiday"
view(data4)


#linear regression graph
par(mfrow=c(3,3))
for(i in 3:11){
  plot(data4[,i], 
       data4$Weekly_Sales,
       main=names(data4[i]), 
       ylab="Weekly Sales", xlab =" ",
       col='red',
       abline(lm(data4[,i] ~ data4$Weekly_Sales, data = data4), col = "blue"))
}

#Boxplot for checking outliers & removing them
par(mfrow=c(1,1))


#Creating a dataframe for outlier treatment
data5 <- data4

#As we are predicting sales, we require to remove outliers in Sales based on Various parameters
#Temperature Outlier treatment -- found 5 outlier and removed them
boxplot(data5$Weekly_Sales ~ cut(data5$Temperature, pretty(data5$Temperature)), main="Temperature vs 
Weekly Sales", xlab ="Temperature", ylab="Weekly Sales", cex.axis=0.5, col="Steel Blue")
outliers_temp <- boxplot(data5$Weekly_Sales ~ cut(data5$Temperature, pretty(data5$Temperature)), 
                         main="Temperature vs Weekly Sales", cex.axis=0.5,plot=FALSE)$out
data5<- data5[-which(data5$Weekly_Sales %in% outliers_temp),]


#CPI Outlier treatment-found 1 outlier and removed them
boxplot(data5$Weekly_Sales ~ cut(data5$CPI, pretty(data5$CPI)), main="CPI vs Weekly Sales",xlab ="CPI", ylab="Weekly Sales", cex.axis=0.5,col="Steel Blue")
outliers_CPI <- boxplot(data5$Weekly_Sales ~ cut(data5$CPI, pretty(data5$CPI)), main="CPI vs Weekly Sales", cex.axis=0.5,plot=FALSE)$out
data5<- data5[-which(data5$Weekly_Sales %in% outliers_CPI),]

#Unemployment outlier treatment--found 3 outlier and removed them
boxplot(data5$Weekly_Sales ~ cut(data5$Unemployment, pretty(data5$Unemployment)), main="Unemployment vs Weekly Sales",xlab ="Unemployment", ylab="Weekly Sales",  cex.axis=0.5,col="Steel Blue")
outliers_Unemployment <- boxplot(data5$Weekly_Sales ~ cut(data5$Unemployment, pretty(data5$Unemployment)), main="Unemployment vs Weekly Sales", cex.axis=0.5,plot=FALSE)$out
data5<- data5[-which(data5$Weekly_Sales %in% outliers_Unemployment),]

#fuel price outlier treatment -- found 2 outliers and removed
boxplot(data5$Weekly_Sales ~ cut(data5$Fuel_Price, pretty(data5$Fuel_Price)), main="Fuel_Price vs Weekly Sales", xlab ="Fuel Price", ylab="Weekly Sales", cex.axis=0.5,col="Steel Blue")
outliers_fuel_price <- boxplot(data5$Weekly_Sales ~ cut(data5$Fuel_Price, pretty(data5$Fuel_Price)), main="Fuel_Price vs Weekly Sales", cex.axis=0.5,plot=FALSE)$out
data5<- data5[-which(data5$Weekly_Sales %in% outliers_fuel_price),]

#Outlier treatment for Holiday Flag - No outliers found
boxplot(data5$Weekly_Sales ~ data5$Holiday_Flag, main = 'Weekly Sales - Holiday_Flag',xlab ="Holiday Flag", ylab="Weekly Sales",col="Steel Blue" )

#outlier treatment for month - 4 outliers found and removed
boxplot(data5$Weekly_Sales ~ data5$month, main = 'Weekly Sales - month', xlab ="Month", ylab="Weekly Sales", col="Steel Blue")
outliers_month <- boxplot(data5$Weekly_Sales ~ data5$month, main = 'Weekly Sales - month',plot=FALSE)$out
data5<- data5[-which(data5$Weekly_Sales %in% outliers_month),]

#outlier treatment for quarter - 2 outliers found and removed
outliers_quarter <- boxplot(data5$Weekly_Sales ~ data5$quarter, main = 'Weekly Sales - quarter',xlab ="Quarters", ylab="Weekly Sales", col="Steel Blue")$out
data5<- data5[-which(data5$Weekly_Sales %in% outliers_quarter),]



"""number of removed outliers:
Outliers removed from data in order:
  
Temperature 5 outliers
CPI Outlier treatment 1 outliers
Unemployment 3 outliers
Fuel price 2 outliers
Holiday Flag - No outliers
Month - 4 outliers
Quarter - 2 outliers
"""

#Removing unnecessary columns and changing structure of Events

data5$Date <-NULL
data5$Store <- NULL
data5$Events <- as.factor(data5$Events)
str(data5)

data5$Holiday_Flag <- as.numeric(data5$Holiday_Flag)
data5$Week_Number <- as.numeric(data5$Week_Number)
data5$quarter <- as.numeric(data5$quarter)


#correlation matrix and corr plot
## the more red it is the higher correlatin they have.the deeper red it has a positive 
#corr the deeper green it is the higher negative corr.
corr = cor(data5[, c(1:9)])
View(corr)
corrplot(corr, method = "color", cl.pos = 'n', rect.col = "black",  tl.col = "indianred4", 
         addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7, 
         cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))




# create dummy variable by : model.matrix

Events <- as.factor(data5$Events)
dummy_Events <- data.frame(model.matrix(~Events))[,-1]

quarter <- as.factor(data5$quarter)
dummy_quarter <- data.frame(model.matrix(~quarter))[,-1]

month <- as.factor(data5$month)
dummy_month <- data.frame(model.matrix(~month))[,-1]


data5 <- cbind(data5,dummy_Events,dummy_quarter,dummy_month)

view(data5)

"""
Q6: First Part: 
Utilize variables like date and restructure dates as 1 for 5 Feb 2010 (starting from the earliest date in order). 
Hypothesize if CPI, unemployment, and fuel price have any impact on sales.
Considering parameters - Weekly Sales, Fuel Price, Week number, Unemployment,Event(categorical), month(categorical)

# Some explanation about  function set.seed(123), and sample.split:
sample.split pick randon 70% of the data save it in a sample data set by a name given by me. this function  
split my data to whatever ratoi I give to it set.seed(123) initialize randomness which means in the first step
I pick random 70% not top 70% down 30%. 70% data is random from entire data it might be top middle bottom.
but if I define seed 1,2,3 that means everytime those particular records will be picked, 
it will not do 
if I do not write this, every time I run this it could be a differnet set 
of 70% which means my final result might vary slightly. If you want to generate the same result everytime you 
should make sure you set the seed.But why 1,2,3? there is no reason behind it. you can
set to any number, make sure you stick to that number every time you
use the same one so that everytime it take the same record and run the traing 
on the same set of data.
"""
# Splitting dataset into training set and test set
set.seed(123)
library(caTools)

#Considering all parameters - Weekly Sales, Holiday Flag, Fuel price, CPI,Unemployment, Weeknumber, Event(categorical),  month(categorical)
dataset <- data5[, c(1,4,5,6,7,11:12, 18:28 )]
#Creating a sample split and divided test & training sets in 30-70 ratio respectively
sample = sample.split(dataset, SplitRatio = 0.7) # Returns a vector with T for 70% of data
trainingSet = subset(dataset, sample == T)
testSet = subset(dataset, sample == F)

# Create model 
"""
weekly_sales is target, data is trainign, and dot (.) after the Weekly_Sales means all the 
columns written in dataset, that is (1,4,5,6,7,11:12, 18:28)  except for the one which I am 
defined as target must be considered as independent.

"""
model = lm(formula = Weekly_Sales ~ . , data = trainingSet)
summary(model)


#_____________
options(repr.plot.width = 10, repr.plot.height = 10)

# Visualizing train set results
y_pred_train = predict(model, newdata = trainingSet)
ggplot() + 
  geom_point(aes(x=trainingSet$Weekly_Sales,y=y_pred_train), size=3,colour = "Blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3))+
  scale_x_continuous(labels = label_number(suffix = " K", scale = 1e-3))+
  ggtitle('Comparision of Actual Sales vs Predicted Sales - Train Data')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Actual Sales") + ylab("Predicted Sales")

# Visualizing the test set results
y_pred_test = predict(model, newdata = testSet)
ggplot() + 
  geom_point(aes(x=testSet$Weekly_Sales,y=y_pred_test), size =3, colour = "Blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3))+
  scale_x_continuous(labels = label_number(suffix = " K", scale = 1e-3))+
  ggtitle('Comparision of Actual Sales vs Predicted Sales - Test Data')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Actual Sales") + ylab("Predicted Sales")



#____Parameters to validate the accuracy of the model and improvise.
# MAPE function  comoputes the mean absolute  percentage error. i pass actual and predicted weekly_sales
MAPE(y_pred_test,testSet$Weekly_Sales)
# RMSE compute the Root mean square error
RMSE(y_pred_test,testSet$Weekly_Sales)



#checking multi collinearity
car::vif(model)


"""
Insights:
our last model  is a good model: due to following reasons
Relatively descent R square value,
VIF values low indicating Less multi Collinearity,
Avoiding rank deficient error warnings caused by NA rows,
MAPE value  is in acceptable range,
Month can be considered as important independent variable for prediction as its p value are highly significant,
Fuel Price, Unemployment together can be considered as important factors than CPI alone.

This model can be further improvised by considering all Stores data for prediction
Using Advanced models like Decision Trees, Random Forest,
and
Using K cross validation techniques for Sampling data.



Thank you. 

"""










