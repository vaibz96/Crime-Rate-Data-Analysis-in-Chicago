#################################################
#  Company    : Stevens 
#  Purpose    : Project
#  Names      : Mansi Padalia, Akash Dhotre, Vaibhav Shanbhag
#  Id			    : 10442254, 10439028, 10433151
#  Date       : Sep 28, 2018
#  Email ID   : mpadalia@stevens.edu, adhotre@stevens.edu, vshanbha@stevens.edu

rm(list=ls())


#set working directory-folder where all files come to/from R
#help(setwd)
setwd("/Users/pratik/Courses/Data Mining/Project")


#install.packages("dplyr")   # for data manipulations
#install.packages("datasets.load")   # for getting test datasets
#install.packages("ggplot2")  # for getiing plots an dgrammer 

# we use it everytime we open RStudio you need to load them again
library("ggplot2")
library("datasets.load")
library("dplyr")

# to read it in the work space
chicagoCrimeData=read.csv("Crimes_Data_Chicago.csv")

#####################  Data Exploration and Processing ############################

#Get the glimpse of data. Glimpse will give the exact view and str will not give the missing ones
#glimpse(chicagoCrimeData)

#str() is a useful command for this which displays the internal structure of the data neatly.
#str(chicagoCrimeData)

#str() is a compact version of summary(), which provides a detailed summary of the data
#summary(chicagoCrimeData)

#to View the data
#View(chicagoCrimeData)

#Data is stored by incident level, means there should be each record for each case number, Check the data and remove if there are any duplicate records by case number
#Lets remove the duplicate rows using duplicated() and subset() functions
chicagoCrimeData <- subset(chicagoCrimeData, !duplicated(chicagoCrimeData$CASE.))
#summary(chicagoCrimeData)
#View(chicagoCrimeData)
#Missing Value Imputations

#Function to use to deal with missing values is is.na function
chicagoCrimeData <- subset(chicagoCrimeData, !is.na(chicagoCrimeData$LATITUDE))
chicagoCrimeData <- subset(chicagoCrimeData, !is.na(chicagoCrimeData$WARD))
#View(chicagoCrimeData)
#Finding and removing missing values using Which() function
#which(is.na(crime$LOCATION))
#crime <- crime[-which(is.na(crime$LOCATION)), ]
#View(crime)
#Removing illogical values from case variable
chicagoCrimeData <- chicagoCrimeData[chicagoCrimeData$CASE.!= 'CASE#',]
#View(chicagoCrimeData)

#To check how the date variable is stored
#head(chicagoCrimeData$DATE..OF.OCCURRENCE)

#Currently, date is stored as a factor variable, we need to convert that to a R Data object using POSIXlt() function
chicagoCrimeData$date <- as.POSIXct(chicagoCrimeData$DATE..OF.OCCURRENCE, format= "%m/%d/%Y %H:%M")
#head(chicagoCrimeData$date)               
#View(chicagoCrimeData)

#Separating time stamp from date variable
#library(chron)
require(chron)
chicagoCrimeData$time <- times(format(chicagoCrimeData$date, "%H:%M:%S"))
#head(chicagoCrimeData$time)

#Next step is to find the distributions to see during what time of the day, crime rate is more.
#Lets devide the day into foue equal intervals and check the distributions for each of them
time.tag <- chron(times= c("00:00:00", "06:00:00", "12:00:00", "18:00:00","23:59:00"))
#time.tag

chicagoCrimeData$time.tag <- cut(chicagoCrimeData$time, breaks= time.tag,
                      labels= c("00-06","06-12", "12-18", "18-00"), include.lowest=TRUE)
#table(chicagoCrimeData$time.tag)

#With our timestamps in order and stored in a separate variable, we can recode the date variable to contain just the date part by stripping the timestamps.
chicagoCrimeData$date <- as.POSIXct(strptime(chicagoCrimeData$date,
                                  format= "%Y-%m-%d"))
#head(chicagoCrimeData$date)

#Lets use the date of incident to find out which week of the month and which month of the year the crime occured
chicagoCrimeData$day <- weekdays(chicagoCrimeData$date, abbreviate= TRUE)
chicagoCrimeData$month <- months(chicagoCrimeData$date, abbreviate= TRUE)

#There are many categories in cryme type, lets combine few to make the analysis easier
#Get the list of chicagoCrimeData types
#table(chicagoCrimeData$PRIMARY.DESCRIPTION)

#Get the count of unique crime types
#length(unique(chicagoCrimeData$PRIMARY.DESCRIPTION))

#Lets group/combine the Crime types based on the description
chicagoCrimeData$chicagoCrimeData <- as.character(chicagoCrimeData$PRIMARY.DESCRIPTION)
chicagoCrimeData$chicagoCrimeData <- ifelse(chicagoCrimeData$chicagoCrimeData %in% c("CRIM SEXUAL ASSAULT", "PROSTITUTION", "SEX OFFENSE"), 'SEXUAL', chicagoCrimeData$chicagoCrimeData)
chicagoCrimeData$chicagoCrimeData <- ifelse(chicagoCrimeData$chicagoCrimeData %in% c("MOTOR VEHICLE THEFT"),"MVT", chicagoCrimeData$chicagoCrimeData)
chicagoCrimeData$chicagoCrimeData <- ifelse(chicagoCrimeData$chicagoCrimeData %in% c("GAMBLING", "INTERFERE WITH PUBLIC OFFICER", "INTERFERENCE WITH PUBLIC OFFICER", "INTIMIDATION",
                                         "LIQUOR LAW VIOLATION", "OBSCENITY", "NON-CRIMINAL", "PUBLIC PEACE VIOLATION",
                                         "PUBLIC INDECENCY", "STALKING", "NON-CRIMINAL (SUBJECT SPECIFIED)"),"NONVIO", chicagoCrimeData$chicagoCrimeData)
chicagoCrimeData$chicagoCrimeData <- ifelse(chicagoCrimeData$chicagoCrimeData == "CRIMINAL DAMAGE", "DAMAGE",chicagoCrimeData$chicagoCrimeData)
chicagoCrimeData$chicagoCrimeData <- ifelse(chicagoCrimeData$chicagoCrimeData == "CRIMINAL TRESPASS","TRESPASS", chicagoCrimeData$chicagoCrimeData)
chicagoCrimeData$chicagoCrimeData <- ifelse(chicagoCrimeData$chicagoCrimeData %in% c("NARCOTICS", "OTHER NARCOTIC VIOLATION", "OTHER NARCOTIC VIOLATION"), "DRUG", chicagoCrimeData$chicagoCrimeData)
chicagoCrimeData$chicagoCrimeData <- ifelse(chicagoCrimeData$chicagoCrimeData == "DECEPTIVE PRACTICE", "FRAUD", chicagoCrimeData$chicagoCrimeData)
chicagoCrimeData$chicagoCrimeData <- ifelse(chicagoCrimeData$chicagoCrimeData %in% c("OTHER OFFENSE", "OTHER OFFENSE"), "OTHERS", chicagoCrimeData$chicagoCrimeData)
chicagoCrimeData$chicagoCrimeData <- ifelse(chicagoCrimeData$chicagoCrimeData %in% c("KIDNAPPING", "WEAPONS VIOLATION", "OFFENSE INVOLVING CHILDREN"), "VIO", chicagoCrimeData$chicagoCrimeData)
#table(chicagoCrimeData$chicagoCrimeData)


#Currently we have arrest variable saying Yes/No, lets change that to 1/0

chicagoCrimeData$ARREST <- ifelse(as.character(chicagoCrimeData$ARREST) == "Y", 1, 0)
#View(chicagoCrimeData)

############################   Visualizations  ##############################
#Plotting frequency for each type of crime
#library(ggplot2)
#qplot(chicagoCrimeData$chicagoCrimeData, xlab = "Crime", main ="Crimes in Chicago") + scale_y_continuous("Number of crimes")

#To check how the crimes are distributed what part of the day, what day of the week and  month
#Temoporal Distributions

#qplot(chicagoCrimeData$time.tag, xlab="Time of day", main = "Crimes by time of day") + scale_y_continuous("Number of crimes")
chicagoCrimeData$day <- factor(chicagoCrimeData$day, levels= c("Mon", "Tue", "Wed","Thu", "Fri", "Sat", "Sun"))
#qplot(chicagoCrimeData$day, xlab= "Day of week", main= "Crimes by day of week")+ scale_y_continuous("Number of crimes")
chicagoCrimeData$month <- factor(chicagoCrimeData$month, levels= c("Jan", "Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
#qplot(chicagoCrimeData$month, xlab= "Month", main= "Crimes by month") + scale_y_continuous("Number of crimes")

#At a crime/time.tag level. there will be four rows for each time level - Lets roll up the data using aggregate function
temp <- aggregate(chicagoCrimeData$chicagoCrimeData, by= list(chicagoCrimeData$chicagoCrimeData,chicagoCrimeData$time.tag), FUN= length)
names(temp) <- c("chicagoCrimeData", "time.tag", "count")

#Lets construct the plot to see the most type of crime occur and time 
#ggplot(temp, aes(x= chicagoCrimeData, y= factor(time.tag))) + geom_tile(aes(fill= count)) + scale_x_discrete("chicagoCrimeData", expand = c(0,0)) +
#  scale_y_discrete("Time of day", expand = c(0,-2)) + scale_fill_gradient("Number of crimes", low = "white", high = "red") + theme_bw() + ggtitle("Crimes by time of day") + theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))

#lets run the analysis to check which day of the week and which month of the year we see the most crimes
#View(chicagoCrimeData)
library(plyr)
temp <- ddply(chicagoCrimeData, .(chicagoCrimeData,day), summarise, count = length(date))

#ggplot(temp, aes(x= chicagoCrimeData, y= day, fill= count)) + geom_tile(aes(fill= count)) + scale_x_discrete("Crime", expand = c(0,0)) + scale_y_discrete("Day of week", expand = c(0,-2)) + scale_fill_gradient("Number of crimes", low = "white", high = "green") + theme_bw() + ggtitle("Crimes by day of week") +
#  theme(panel.grid.major = element_line(colour = NA), panel.grid.minor =
#          element_line(colour = NA))

library(plyr)
temp <- ddply(chicagoCrimeData, .(chicagoCrimeData,month), summarise, count = length(date))

#ggplot(temp, aes(x= chicagoCrimeData, y= month, fill= count)) + geom_tile(aes(fill= count)) + scale_x_discrete("Crime", expand = c(0,0)) + scale_y_discrete("Month", expand = c(0,-2)) + scale_fill_gradient("Number of crimes", low = "white", high = "steelblue") + theme_bw() + ggtitle("Crimes by Month") + 
#  theme(panel.grid.major = element_line(colour = NA), panel.grid.minor =
#          element_line(colour = NA))


################################ Spatial distributions #####################################

#Reading Chicago beats shape file      
#install.packages("maptools")
#library(maptools)
#beat.shp <- readShapePoly("geo_export_074b9ade-a735-4887-af12-fe5c787a5558.shp")
#plot(beat.shp)

#Reading chicago city police stations csv file
#PoliceStations=read.csv("Police_Stations.csv")
#View(PoliceStations)

# select variables 
#myvars <- c("DISTRICT.NAME", "LATITUDE", "LONGITUDE")
#Stations <- PoliceStations[myvars]
#View(Stations)


#chicagoCrimeData.agg <- ddply(chicagoCrimeData, .(chicagoCrimeData, ARREST, BEAT, date, X.COORDINATE,
#                            Y.COORDINATE, time.tag, day, month), summarise, count = length(date),
#                   .progress= 'text')

#Converting shape file into data frame

#install.packages("rgdal")
#library(rgdal)
#require(rgdal)
#beats <- readOGR(dsn=".",layer="geo_export_074b9ade-a735-4887-af12-fe5c787a5558")
#beats.df <- as(beats, "data.frame")
#View(beats.df)


### Modeling

chicagoCrimeDataCluster <- chicagoCrimeData
chicagoCrimeDataCluster$IUCR<-as.numeric(factor(chicagoCrimeDataCluster$IUCR,levels=(c("0110","0130","0141","0142","0261","0262","0263","0264","0265","0266","0271","0272","0273","0274","0275","0281","0291","0312","0313","031A","031B","0320","0325","0326","0330","0331","0334","0337","033A","033B","0340","041A","041B","0420","0430","0440","0450","0451","0452","0453","0454","0460","0461","0462","0470","0475","0479","0480","0481","0482","0483","0484","0485","0486","0487","0488","0489","0490","0491","0492","0493","0494","0495","0496","0497","0498","0510","051A","051B","0520","0530","0545","0550","0551","0552","0553","0554","0555","0556","0557","0558","0560","0580","0581","0583","0584","0610","0620","0630","0650","0810","0820","0850","0860","0865","0870","0880","0890","0895","0910","0915","0917","0918","0920","0925","0927","0928","0930","0935","0937","0938","1010","1020","1025","1030","1035","1050","1055","1090","1110","1120","1121","1122","1130","1135","1140","1150","1151","1152","1153","1154","1155","1156","1160","1170","1185","1195","1200","1205","1206","1210","1220","1230","1235","1240","1241","1242","1245","1255","1260","1261","1265","1305","1310","1320","1330","1335","1340","1345","1350","1360","1365","1370","1375","141A","141B","141C","142A","142B","1435","143A","143B","143C","1440","1450","1460","1475","1476","1477","1478","1479","1480","1481","1505","1506","1507","1510","1511","1512","1513","1515","1520","1521","1525","1526","1530","1531","1535","1536","1537","1540","1541","1542","1544","1549","1562","1563","1564","1565","1566","1570","1572","1574","1576","1578","1580","1582","1585","1590","1610","1611","1620","1621","1622","1623","1624","1625","1626","1627","1630","1631","1632","1633","1640","1650","1651","1661","1670","1680","1681","1682","1690","1691","1692","1693","1694","1695","1696","1697","1710","1715","1720","1725","1750","1751","1752","1753","1754","1755","1775","1780","1790","1791","1792","1811","1812","1821","1822","1840","1850","1860","1900","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024","2025","2026","2027","2028","2029","2030","2031","2032","2033","2034","2040","2050","2060","2070","2080","2090","2091","2092","2093","2094","2095","2110","2111","2120","2160","2170","2210","2220","2230","2240","2250","2251","2500","2820","2825","2826","2830","2840","2850","2851","2860","2870","2890","2895","2900","3000","3100","3200","3300","3400","3610","3710","3720","3730","3731","3740","3750","3751","3760","3770","3800","3910","3920","3960","3966","3970","3975","3980","4210","4220","4230","4240","4255","4310","4386","4387","4388","4389","4410","4420","4510","4625","4650","4651","4652","4740","4750","4800","4810","4860","5000","5001","5002","5003","5004","5007","5009","500E","500N","5011","501A","501H","502P","502R","502T","5110","5111","5112","5120","5121","5122","5130","5131","5132"))))
chicagoCrimeDataCluster$IUCR<-chicagoCrimeDataCluster$IUCR/1000
crimeForCluster <- na.omit(chicagoCrimeDataCluster[, c(4,8,10,11)])
#is.numeric(chicagoCrimeDataCluster$IUCR)
#View(chicagoCrimeDataCluster)
#str(crimeForCluster)

#install.packages("cluster")
#install.packages("factoextra")
#install.packages("NbClust")

require(cluster)    # clustering algorithms
require(factoextra)  # clustering algorithms & visualization
require(NbClust)
#Dertemines and visualize the optimal number of clusters using within cluster sums of squares


wss <- function(k) {
  kmeans((crimeForCluster), k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
install.packages("tidyverse")
require(tidyverse)
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#Silhouette method 
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(crimeForCluster, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(crimeForCluster))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

#Gap Statistic Method by Another Method
# compute gap statistic
set.seed(123)
gap_stat <- clusGap(crimeForCluster, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

nb <- NbClust(crimeForCluster, distance = "euclidean", min.nc = 2,
              max.nc = 15, method = "Complete")
fviz_nbclust(nb)

#NbClust

require("NbClust")
set.seed(123)
res.nb <- NbClust(crimeForCluster, distance = "euclidean",
                  min.nc = 2, max.nc = 10, 
                  method = "complete") 

nb <- NbClust(crimeForCluster, distance = "euclidean", min.nc = 2,max.nc = 15, method = "kmeans")
fviz_nbclust(nb)

res.nb # print the results

# All gap statistic values
res.nb$All.index

# Best number of clusters
res.nb$Best.nc

# Best partition
res.nb$Best.partition

nb <- NbClust(crimeDataSetForCluster, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")
# Print the result
nb


#It's possible to visualize the result using the function fviz_nbclust() [in factoextra], as follow:

fviz_nbclust(nb) + theme_minimal()


# k-means Clustering

kmeans_3<- kmeans(crimeForCluster[,c(-4)],3,nstart = 10)
str(kmeans_3)
kmeans_3$cluster
table(kmeans_3$cluster,crimeForCluster[,4])


###### C50

# Cluster 1

crimeData_clus_1 <- chicagoCrimeData[kmeans_3$cluster == 1,c(4,8,10,11)]
crimeData_clus_1<-na.omit(crimeData_clus_1);
View(crimeData_clus_1)

#index <- seq (1,nrow(crimeData_clus_1),by=5)
index<-sample(nrow(crimeData_clus_1),as.integer(.10*nrow(crimeData_clus_1)))

test<-crimeData_clus_1[index,]
training<-crimeData_clus_1[-index,]

View(training)
#is.data.frame(training)

library('C50')

mytree <- C5.0( factor(WARD)~., data =training )

prediction<-predict( mytree ,test , type="class" )
table(actual=test[,4],prediction)
wrong<- (test[,4]!=prediction)
rate<-sum(wrong)/length(wrong)
rate
accuracy_Cluster1<-(1-rate)*100
accuracy_Cluster1

# Cluster 2

crimeData_clus_2 <- chicagoCrimeData[kmeans_3$cluster == 2,c(4,8,10,11)]
crimeData_clus_2<-na.omit(crimeData_clus_2);
View(crimeData_clus_2)

#index <- seq (1,nrow(crimeData_clus_2),by=5)
index<-sample(nrow(crimeData_clus_2),as.integer(.10*nrow(crimeData_clus_2)))

test<-crimeData_clus_2[index,]
training<-crimeData_clus_2[-index,]

View(training)

library('C50')

mytree <- C5.0( factor(WARD)~., data =training )

prediction<-predict( mytree ,test , type="class" )
table(actual=test[,4],prediction)
wrong<- (test[,4]!=prediction)
rate<-sum(wrong)/length(wrong)
rate
accuracy_Cluster2<-(1-rate)*100

# Cluster 3

crimeData_clus_3 <- chicagoCrimeData[kmeans_3$cluster == 2,c(4,8,10,11)]
crimeData_clus_3<-na.omit(crimeData_clus_3);
View(crimeData_clus_3)

#index <- seq (1,nrow(crimeData_clus_3),by=5)
index<-sample(nrow(crimeData_clus_3),as.integer(.10*nrow(crimeData_clus_3)))

test<-crimeData_clus_3[index,]
training<-crimeData_clus_3[-index,]

View(training)

library('C50')

mytree <- C5.0( factor(WARD)~., data =training )

prediction<-predict( mytree ,test , type="class" )
table(actual=test[,4],prediction)
wrong<- (test[,4]!=prediction)
rate<-sum(wrong)/length(wrong)
rate
accuracy_Cluster3<-(1-rate)*100

average_cluster_accuracy <- (accuracy_Cluster1 + accuracy_Cluster2+accuracy_Cluster3)/3

####### KKNN

chicagoCrimeData_knn<-chicagoCrimeData[, c(4,8,10,11)]
index<-sample(nrow(chicagoCrimeData_knn),as.integer(.33*nrow(chicagoCrimeData_knn)))

test<-chicagoCrimeData_knn[index,]
training<-chicagoCrimeData_knn[-index,]
library(kknn)
predict_k10<-kknn(formula=WARD~., training, test, k=3, kernel="rectangular")

fit<-fitted(predict_k10)
fit_round<-round(fit)
fit_round
table(test$WARD,fit_round)
wrong<-test$WARD!=fit_round
rate<-sum(wrong)/length(wrong)
rate
Accuracy_knn<-(1-rate)*100
Accuracy_knn
