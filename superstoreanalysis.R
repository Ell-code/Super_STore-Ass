#install.packages("janitor")
#install.packages("tibble")
#install.packages("tidyverse")
#install.packages("rmarkdown")

#set working directory
#set working directory
getwd()

#Load the Libraries
library(tidyverse)
library(RColorBrewer)
library(ggfortify) # convert time series data to a data_Frame
library(doBy) # To use the order_BY function
library(plyr)
library(skimr)
library(janitor)


super_store<-read.csv("SuperStore.csv")
str(super_store)
# getting the summarization of all the data
skim_without_charts(super_store)

#Testing for missing values
missing <- sapply(super_store, function(x) sum(is.na(x)))
missing

#Transactions per customer
customers<- aggregate(super_store$Customer.Name, by=list(super_store$Customer.Name), FUN=length)
colnames(customers)<-c("Customer","Count")
head(customers)

#Profitability by customer 
store_profits <- aggregate(super_store$Profit, by=list(Category=super_store$Customer.Name), FUN=sum)
colnames(store_profits)<-c("Customer","profit")
head(store_profits)

##Number of orders from each city
citypop <- aggregate(super_store$City, by=list(super_store$City), FUN=length)
colnames(citypop)<-c("City","Count")
head(citypop)

citypor <- aggregate(super_store$Profit, by=list(super_store$City), FUN=sum)
colnames(citypor)<-c("City","profit")
head(citypor)
city_i =  top_n(citypor, 20)
ggplot() + geom_bar(data = city_i, aes(x = city_i$City, y = city_i$profit), stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + xlab("Customers") + ylab("Total Transaction") + ggtitle("Top 20 city based on Transaction")

#cutomers prefered shipping mode

shippment <- aggregate(super_store$Ship.Mode, by=list(super_store$Ship.Mode), FUN=length)
colnames(shippment)<-c("Ship_Mode","Total_Count")
head(shippment)

shipmode <- super_store[,5]
#assign integer to shipmode
shipdata<-data.frame(super_store$Customer.Name,super_store$Ship.Mode)
colnames(shipdata)<-c("customer","shipmode")
head(shipdata)

#Number of times each type of shipmode was used by each customer
ship1<-count(shipdata,customer,shipmode)
head(ship1)

#Extract the shipmode mostly used by each customer
ship2 <- ship1 %>%
  group_by(customer) %>%
  filter(n==max(n))
shipp <- ddply(ship2, .(customer), head, n = 1)
head(shipp)

#Regular customers in descending order
c_des<-customers[order(customers$Count,decreasing = TRUE),]
head(c_des)

#Top 45 regular customers
top30<-customers[customers$Count > quantile(customers$Count,prob=1-0.063),]
top30_ordered<-top30[order(top30$Count,decreasing = TRUE),]
head(top30_ordered)

#Plot the number of transactions of the top 45 regular customers
ggplot(top30_ordered,aes(x=Count,colour='black'))+geom_histogram(fill='black',binwidth = 1,boundary = 0)+ggtitle("Top 30 regular customers(1/3/2014-30/12/2017)")+geom_vline(aes(xintercept=mean(Count)),linetype="dashed")+geom_text(aes(x=mean(top30_ordered$Count ),y=15),label="Mean 27.76",hjust=1, size=3,col=1)+geom_text(aes(x=median(top30_ordered$Count ),y=10),label="Median 27",hjust=1, size=3,col=1)+xlab("Number of Transactions")+ylab("Frequency")+geom_vline(aes(xintercept=median(Count)))+ scale_x_continuous(breaks = seq(0,40,1))

"
During the period March 2014- December 2017, 15 out of the 45 regular customers, each have 25 transactions at the superstore, 
while 50% of the customers have 27 or less transactions. In addition, the histogram appears to be slightly positively skewed (median < mean) with an average of 27.76.
"

# adding a new column to the data set so as to change the orderdate dataset to Date format

 new_table <- super_store %>%
  mutate(new_date = parse_date(super_store$Order.Date, "%d-%b-%y"))

view(new_table)

# create a daily date
dates <- as.Date(new_table$new_date, "%m/%d/%Y")
head(dates)

# Extract each days from our dates data
days <- weekdays(as.Date(dates, '%Y-%m-%d'))
head(days)

# Extracting the year

years <- substring(dates, 1,4)
head(years)

Sales_Date <- data.frame(days, super_store$Sales, dates, years)
head(Sales_Date)

# plot the graph and change the level of order
Sales_Date$days <- factor(Sales_Date$days, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(data = Sales_Date, aes(x=Sales_Date$days, y=super_store$Sales)) + geom_bar(stat = "identity", show.legend = TRUE, fill = "blue") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Total Sales for each year per days") + facet_wrap(~years, ncol = 2) + ylab("Sales in Dollar") + xlab("days")

'
From 2018 on ward Fridays appears to have the lowest sales annually.
'

dates1 <- as.Date(new_table$new_date, "%d/%m/%Y")
head(dates1)

saless <- data.frame(dates1, super_store$Sales)
salest <- ts(saless[, -1], frequency = 12, start = c(2014, 03), end = c(2018, 12))
head(salest)

plot(salest, xlab = "years", ylab="sales in dollars", main="Super Store Sales")

ggplot() + geom_bar(data = super_store, aes(x = super_store$Category, y = super_store$Sales, fill = Region), stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot() + geom_bar(data = super_store, aes(x = super_store$Category, y = super_store$Sales), stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot() + geom_bar(data = super_store, aes(x = super_store$Category, y = super_store$Profit), stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Creating a categories dataframe for both profit and sales
catego <- aggregate(super_store$Sales, by=list(super_store$Category), FUN=sum)
colnames(catego)<-c("Categories","Sales")
head(catego)

catepr <- aggregate(super_store$Profit, by=list(super_store$Category), FUN=sum)
colnames(catepr) <- c("Categories", "Profit")
head(catepr)

categor <- data.frame(catego, catepr$Profit)
head(categor)

ggplot() + geom_bar(data = catego, aes(x=categor$Categories, y=categor$Sales), stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Total Sales") + xlab("Categories")
ggplot() + geom_bar(data = catego, aes(x=categor$Categories, y=categor$catepr.Profit), stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Total Profit") + xlab("Categories")


# Creating a segment dataframe for both profot and sales
segmen <- aggregate(super_store$Sales, by=list(super_store$Segment), FUN=sum)
colnames(segmen)<-c("Segment","Sales")
head(segmen)

segpro <- aggregate(super_store$Profit, by=list(super_store$Segment), FUN=sum)
colnames(segpro) <- c("Segment", "Profit")
head(segpro)

segmental <- data.frame(segmen, segpro$Profit)
head(segmental)

library(scales)
ggplot() + geom_bar(data = segmental, aes(x=segmental$Segment, y=segmental$Sales), stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Total Sales") + xlab("Segment")
ggplot() + geom_bar(data = segmental, aes(x=segmental$Segment, y=segmental$segpro.Profit), stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Total Profit") + xlab("Segment") + scale_y_continuous(labels = comma)

