#install packages
install.packages("splitstackshape") install.packages("car")
#zomato analysis - open required libraries
library(readr)
library(ggplot2)
library(ggmap)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(ggrepel)
library(RgoogleMaps)
library(splitstackshape)
#zomato dataset - data cleaning
zomato <- read_csv("~/Documents/MYcadeMYa/current/LSE/fourth year 2018:19/ST309 Elementary Data Analytics/Project/zomato-restaurants-data/zomato.csv")
View(zomato)
summary(zomato)
#removing redundant and non-explained variables
drops <- c("Address","Restaurant Name","Locality","Locality
 Verbose","Switch to order menu", "Is delivering now", "Rating color",
"Rating text")
zomatoclean <-zomato[ , !(names(zomato) %in% drops)] #removing restaurants with <30 votes
zomatoclean <- zomatoclean[!zomatoclean$Votes<30,] #checking for missing values
anyNA(zomatoclean)
sum(is.na(zomatoclean)) zomatoclean[!complete.cases(zomatoclean),]
#check data class of variables
str(zomatoclean)
View(zomatoclean)
attach(zomatoclean)
#splitting cuisine
zomatoclean<-cSplit(zomatoclean, "Cuisines", ",")
#convert characters into numeric
#Has Online Delivery
zomatoclean$od=10
zomatoclean$od[zomatoclean$"Has Online delivery"=="Yes"]=1 
zomatoclean$od[zomatoclean$"Has Online delivery"=="No"]=0 
zomatoclean$od <- as.numeric(zomatoclean$od)
#Has Table booking
zomatoclean$tb=10
zomatoclean$tb[zomatoclean$"Has Table booking"=="Yes"]=1
zomatoclean$tb[zomatoclean$"Has Table booking"=="No"]=0
zomatoclean$tb <- as.numeric(zomatoclean$tb)
#convert meal prices to USD, Price of 01.02.2018
zomatoclean$currencytoUSD <- 1 
zomatoclean$currencytoUSD[zomatoclean$"Country Code"==1]=63.66 
zomatoclean$currencytoUSD[zomatoclean$"Country Code"==14]=1.244 
zomatoclean$currencytoUSD[zomatoclean$"Country Code"==30]=3.164 
zomatoclean$currencytoUSD[zomatoclean$"Country Code"==37]=1.226 
zomatoclean$currencytoUSD[zomatoclean$"Country Code"==94]=13386 
zomatoclean$currencytoUSD[zomatoclean$"Country Code"==148]=1.352 
zomatoclean$currencytoUSD[zomatoclean$"Country Code"==162]=51.44 
zomatoclean$currencytoUSD[zomatoclean$"Country Code"==166]=3.64 
zomatoclean$currencytoUSD[zomatoclean$"Country Code"==184]=1.308 
zomatoclean$currencytoUSD[zomatoclean$"Country Code"==189]=11.84 
zomatoclean$currencytoUSD[zomatoclean$"Country Code"==191]=154.03 
zomatoclean$currencytoUSD[zomatoclean$"Country Code"==208]=3.734 
zomatoclean$currencytoUSD[zomatoclean$"Country Code"==214]=3.672 
zomatoclean$currencytoUSD[zomatoclean$"Country Code"==215]=0.701
zomatoclean$USDaveragecost <- zomatoclean$"Average Cost for two" / zomatoclean$currencytoUSD
#adding/converting variables
#adding a country names variable
zomatoclean$country <- 1
zomatoclean$country[zomatoclean$"Country Code"==1]='India' 
zomatoclean$country[zomatoclean$"Country Code"==14]='Australia' 
zomatoclean$country[zomatoclean$"Country Code"==30]='Brazil' 
zomatoclean$country[zomatoclean$"Country Code"==37]='Canada' 
zomatoclean$country[zomatoclean$"Country Code"==94]='Indonesia' 
zomatoclean$country[zomatoclean$"Country Code"==148]='New Zealand' 
zomatoclean$country[zomatoclean$"Country Code"==162]='Phillipines' 
zomatoclean$country[zomatoclean$"Country Code"==166]='Qatar' 
zomatoclean$country[zomatoclean$"Country Code"==184]='Singapore' 
zomatoclean$country[zomatoclean$"Country Code"==189]='South Africa' 
zomatoclean$country[zomatoclean$"Country Code"==191]='Sri Lanka' 
zomatoclean$country[zomatoclean$"Country Code"==208]='Turkey' 
zomatoclean$country[zomatoclean$"Country Code"==214]='UAE' 
zomatoclean$country[zomatoclean$"Country Code"==215]='United Kingdom' 
zomatoclean$country[zomatoclean$"Country Code"==216]='United States'
#adding a touristy city variable
zomatoclean$touristy <- 0
zomatoclean$touristy[zomatoclean$City== "London"]=1
zomatoclean$touristy[zomatoclean$City== "Singapore"]=1
zomatoclean$touristy[zomatoclean$City== "Dubai"]=1
zomatoclean$touristy[zomatoclean$City== "New Delhi"]=1
zomatoclean$touristy[zomatoclean$City== "Delhi"]=1
zomatoclean$touristy[zomatoclean$City== "Mumbai"]=1
zomatoclean$touristy[zomatoclean$City== "Agra"]=1
zomatoclean$touristy[zomatoclean$City== "Johannesburg"]=1
zomatoclean$touristy[zomatoclean$City== "Orlando"]=1
zomatoclean$touristy[zomatoclean$City== "Chennai"]=1

zomatoclean$touristy[zomatoclean$City== "Jaipur"]=1
zomatoclean$touristy[zomatoclean$City== "Jakarta"]=1
zomatoclean$touristy[zomatoclean$City== "Doha"]=1
zomatoclean$touristy[zomatoclean$City== "Auckland"]=1
zomatoclean$touristy[zomatoclean$City== "Rest of Hawaii"]=1
zomatoclean$touristy[zomatoclean$City== "Kolkata"]=1
zomatoclean$touristy[zomatoclean$City== "Rio de Janeiro"]=1
zomatoclean$touristy[zomatoclean$City== "Abu Dhabi"]=1
zomatoclean$touristy[zomatoclean$City== "Colombo"]=1
#exploratory analysis - geographical distribution of ratings
count(zomato$`Country Code`)
bar1 <- ggplot(zomatoclean) + geom_bar(aes(country),stat="count") + labs(title="Barchart of number of restaurants by country", x="Country", y="Number of restaurants")
p1f <- ggplot(zomatoclean) + geom_point(aes(y=`Aggregate rating`, x=USDaveragecost)) + labs(title="Plot of Aggregate Rating against Average Cost (USD) (Global)", x="Average cost for two in USD", y="Aggregate Rating") + facet_wrap(vars(country), ncol=3)
p1f
#subsetting data - exploring India
zomatoindia <- subset(zomatoclean, country=="India") View(zomatoindia)
# adding city center location and euclidian distance of the restaurant from city centre.
# Data on city center location from https://latitudelongitude.org/in/ summary(zomatoindia$City)
zomatoindia$centrelong <- 0
zomatoindia$centrelat <- 0
zomatoindia$centrelong[zomatoindia$City== "New Delhi"]=77.22897 
zomatoindia$centrelong[zomatoindia$City== "Gurgaon"]=77.02635 
zomatoindia$centrelong[zomatoindia$City== "Noida"]=77.33 
zomatoindia$centrelong[zomatoindia$City== "Faridabad"]=77.31977 
zomatoindia$centrelong[zomatoindia$City== "Ahmedabad"]=72.58727 
zomatoindia$centrelong[zomatoindia$City== "Bhubaneshwar"]=85.83385 
zomatoindia$centrelong[zomatoindia$City== "Lucknow"]=80.92313 
zomatoindia$centrelong[zomatoindia$City== "Agra"]=78.01667 
zomatoindia$centrelong[zomatoindia$City== "Amritsar"]=74.87476 
zomatoindia$centrelong[zomatoindia$City== "Bangalore"]=77.59369 
zomatoindia$centrelong[zomatoindia$City== "Chennai"]=80.27847 
zomatoindia$centrelong[zomatoindia$City== "Dehradun"]=78.03392 
zomatoindia$centrelong[zomatoindia$City== "Goa"]=73.82624 
zomatoindia$centrelong[zomatoindia$City== "Jaipur"]=75.78781 
zomatoindia$centrelong[zomatoindia$City== "Kochi"]=76.26022 
zomatoindia$centrelong[zomatoindia$City== "Kolkata"]=88.36304 
zomatoindia$centrelong[zomatoindia$City== "Mangalore"]=74.85603
zomatoindia$centrelong[zomatoindia$City== "Mumbai"]=72.88261 
zomatoindia$centrelong[zomatoindia$City== "Puducherry"]=79.82979 
zomatoindia$centrelong[zomatoindia$City== "Pune"]=73.85535

zomatoindia$centrelong[zomatoindia$City== "Vadodara"]=73.20812 
zomatoindia$centrelong[zomatoindia$City== "Bhopal"]=77.40289 
zomatoindia$centrelong[zomatoindia$City== "Coimbatore"]=76.96612 
zomatoindia$centrelong[zomatoindia$City== "Guwahati"]=91.7458 
zomatoindia$centrelong[zomatoindia$City== "Indore"]=75.8333 
zomatoindia$centrelong[zomatoindia$City== "Kanpur"]=80.34627 
zomatoindia$centrelong[zomatoindia$City== "Ludhiana"]=75.85229 
zomatoindia$centrelong[zomatoindia$City== "Mysore"]=76.63925 
zomatoindia$centrelong[zomatoindia$City== "Patna"]=85.11936 
zomatoindia$centrelong[zomatoindia$City== "Surat"]=72.83023 
zomatoindia$centrelong[zomatoindia$City== "Varanasi"]=83.01041 
zomatoindia$centrelong[zomatoindia$City== "Vizag"]=83.20161 
zomatoindia$centrelong[zomatoindia$City== "Allahabad"]=81.83329 
zomatoindia$centrelong[zomatoindia$City== "Chandigarh"]=76.7884 
zomatoindia$centrelong[zomatoindia$City== "Hyderabad"]=78.45636 
zomatoindia$centrelong[zomatoindia$City== "Aurangabad"]=75.34226 
zomatoindia$centrelong[zomatoindia$City== "Nagpur"]=79.08491 
zomatoindia$centrelong[zomatoindia$City== "Nashik"]=73.79096 
zomatoindia$centrelong[zomatoindia$City== "Ranchi"]=85.33856
zomatoindia$centrelong[zomatoindia$City== "Ghaziabad"]=77.43777 
zomatoindia$centrelong[zomatoindia$City== "Secunderabad"]=78.54263
zomatoindia$centrelong[zomatoindia$City== "Mohali"]=76.72211 
zomatoindia$centrelong[zomatoindia$City== "Panchkula"]=76.7884 
zomatoindia$centrelat[zomatoindia$City== "New Delhi"]=28.65381 
zomatoindia$centrelat[zomatoindia$City== "Gurgaon"]=28.4601 
zomatoindia$centrelat[zomatoindia$City== "Noida"]=28.58 
zomatoindia$centrelat[zomatoindia$City== "Faridabad"]=28.41252 
zomatoindia$centrelat[zomatoindia$City== "Ahmedabad"]=23.02579 
zomatoindia$centrelat[zomatoindia$City== "Bhubaneshwar"]=20.27241
zomatoindia$centrelat[zomatoindia$City== "Lucknow"]=26.83928 
zomatoindia$centrelat[zomatoindia$City== "Agra"]=27.18333 
zomatoindia$centrelat[zomatoindia$City== "Amritsar"]=31.63661 
zomatoindia$centrelat[zomatoindia$City== "Bangalore"]=12.97194
zomatoindia$centrelat[zomatoindia$City== "Chennai"]=13.08784 
zomatoindia$centrelat[zomatoindia$City== "Dehradun"]=30.32443 
zomatoindia$centrelat[zomatoindia$City== "Goa"]=15.49574 
zomatoindia$centrelat[zomatoindia$City== "Jaipur"]=26.91962 
zomatoindia$centrelat[zomatoindia$City== "Kochi"]=9.93988 
zomatoindia$centrelat[zomatoindia$City== "Kolkata"]=22.56263
zomatoindia$centrelat[zomatoindia$City== "Mangalore"]=12.91723
zomatoindia$centrelat[zomatoindia$City== "Mumbai"]=19.07283 
zomatoindia$centrelat[zomatoindia$City== "Puducherry"]=11.93381
zomatoindia$centrelat[zomatoindia$City== "Pune"]=18.51957 
zomatoindia$centrelat[zomatoindia$City== "Vadodara"]=22.29941
zomatoindia$centrelat[zomatoindia$City== "Bhopal"]=23.25469 
zomatoindia$centrelat[zomatoindia$City== "Coimbatore"]=11.00555
zomatoindia$centrelat[zomatoindia$City== "Guwahati"]=26.1844 
zomatoindia$centrelat[zomatoindia$City== "Indore"]=22.71792 
zomatoindia$centrelat[zomatoindia$City== "Kanpur"]=26.4478 
zomatoindia$centrelat[zomatoindia$City== "Ludhiana"]=30.90015
zomatoindia$centrelat[zomatoindia$City== "Mysore"]=12.29791 
zomatoindia$centrelat[zomatoindia$City== "Patna"]=25.60222 
zomatoindia$centrelat[zomatoindia$City== "Surat"]=21.19594 
zomatoindia$centrelat[zomatoindia$City== "Varanasi"]=25.31668
zomatoindia$centrelat[zomatoindia$City== "Vizag"]=17.68009

zomatoindia$centrelat[zomatoindia$City== "Allahabad"]=25.44894 
zomatoindia$centrelat[zomatoindia$City== "Chandigarh"]=30.73629 
zomatoindia$centrelat[zomatoindia$City== "Hyderabad"]=17.38405 
zomatoindia$centrelat[zomatoindia$City== "Aurangabad"]=19.87757 
zomatoindia$centrelat[zomatoindia$City== "Nagpur"]=21.14631 
zomatoindia$centrelat[zomatoindia$City== "Nashik"]=19.99727 
zomatoindia$centrelat[zomatoindia$City== "Ranchi"]=23.34777 
zomatoindia$centrelat[zomatoindia$City== "Ghaziabad"]=28.66249
zomatoindia$centrelat[zomatoindia$City== "Secunderabad"]=17.50427
zomatoindia$centrelat[zomatoindia$City== "Mohali"]=30.67995 
zomatoindia$centrelat[zomatoindia$City== "Panchkula"]=0.73629
zomatoindia$centredist <- sqrt((zomatoindia$centrelong-zomatoindia$Longitude)^2+ (zomatoindia$centrelat-zomatoindia$Latitude)^2)
#add Capital_dummy variable
zomatoindia$capital <- 0 zomatoindia$capital[zomatoindia$City== "New Delhi"]=1
#add number of cuisines variable
zomatoindia$cuisinenumber<-8 zomatoindia$cuisinenumber <-
ifelse(zomatoindia$Cuisines_2 %in% NA, 1, ifelse(zomatoindia$Cuisines_3 %in% NA, 2,
ifelse(zomatoindia$Cuisines_4 %in% NA, 3, ifelse(zomatoindia$Cuisines_5 %in% NA, 4,
ifelse(zomatoindia$Cuisines_6 %in% NA, 5, ifelse(zomatoindia$Cuisines_7 %in% NA, 6,
ifelse(zomatoindia$Cuisines_8 %in% NA, 7, 8)))))))
zomatoindiaprice0 <- zomatoindia[!zomatoindia$USDaveragecost==0,]
#exploratory analysis
install.packages("tidyverse") library(tidyverse)
hist(zomatoindia$`Aggregate rating`)
#relationship between touristy and aggregate rating
p3 <- ggplot(zomatoindia) + geom_boxplot(aes(x=`touristy`, y=`Aggregate rating`, group=`touristy`, fill=`touristy`)) + labs(title="Aggregate Ratings for restaurants in 'Touristy cities'", x='Touristy city?', y="Aggregate Rating") + guides(fill=FALSE)
p3
#subset of Indian restaurants with price>0

hist_touristy_1 <- ggplot(zomatoindia[zomatoindia$touristy==1,]) + geom_histogram(aes(zomatoindia[zomatoindia$touristy==1,]$`Aggregate rating`),) + labs(title="Histogram of Aggregate Rating in 'Touristy cities'",x="Aggregate Rating",y="Frequency")
hist_touristy_1
hist_touristy_0 <- ggplot(zomatoindia[zomatoindia$touristy==0,]) + geom_histogram(aes(zomatoindia[zomatoindia$touristy==0,]$`Aggregate rating`),) + labs(title="Histogram of Aggregate Rating in 'non-Touristy cities'",x="Aggregate Rating",y="Frequency")
hist_touristy_0
box2 <- ggplot(zomatoindia) + geom_boxplot(aes(y=zomatoindia$`Aggregate rating`, x=zomatoindia$`Price range`, group=zomatoindia$`Price range`, fill=`Price range`)) + labs(title="Boxplots of aggregate rating, by price range", x="Price range", y="Aggregate rating")
box2
p1 <- ggplot(zomatoindiaprice0) + geom_point(aes(y=`Aggregate rating`, x=USDaveragecost)) + labs(title="Plot of Aggregate Rating against Average Cost (USD)", x="Average cost for two in USD", y="Aggregate Rating")
p1 + geom_smooth(aes(y=`Aggregate rating`, x=`USDaveragecost`)) p1
p4od <- ggplot(zomatoindia) + geom_boxplot(aes(x=`od`, y=`Aggregate rating`, fill=`od`)) + labs(title="Aggregate Ratings for restaurants offering online delivery", x="Online delivery") + guides(fill=FALSE)
p4od
p4tb <- ggplot(zomatoindia) + geom_boxplot(aes(x=`tb`, y=`Aggregate rating`, fill=`tb`)) + labs(title="Aggregate Ratings for restaurants offering table booking", x="Table booking") + guides(fill=FALSE)
p4tb
cor(zomatoclean$od,zomatoclean$dn)
cor(zomatoclean$od,zomatoclean$tb)
install.packages("moments") library(moments)
hist(`Aggregate rating`, prob=TRUE) skewness(`Aggregate rating`) kurtosis(`Aggregate rating`)
hist(`Average Cost for two`, prob=TRUE) skewness(`Average Cost for two`) kurtosis(`Average Cost for two`)
install.packages("yarrr")
library(yarrr)
pirateplot(formula=`Aggregate rating`~`Price range`, data=zomato,
pal="xmen")
pirateplot(formula=`Aggregate rating`~`Price range`, data=zomatoclean,
pal="xmen")
pirateplot(formula=`Aggregate rating`~`Price range`, data=zomatoindia,
pal="xmen")

#clustering analysis
library('stats')
# To conduct a clustering analysis we can use only quantitative data. Hence we create a new dataset. Given that many restaurants are in null island we also need to make sure that distance variable that we get makes sense. For this reason we look at the distance variable and see where values 'jump'.
summary(zomatoindia)
zomatoindianumeric <- data.frame(zomatoindia["dn"], zomatoindia["tb"],
 zomatoindia["od"], zomatoindia["touristy"], zomatoindia["Aggregate
rating"],zomatoindia["USDaveragecost"], zomatoindia["centredist"]) zomatoindianumeric <-subset(zomatoindianumeric,
zomatoindianumeric$centredist<20)
# We need to standardise the data to be able to do clustering normaliseddata <- scale(zomatoindianumeric)
# Calculate Euclidian distance
distance <- dist(normaliseddata)
# To evaluate the number of clusters needed we create a new function (using
 Elbow plot)
set.seed=42
evaluationvariable <- function(normaliseddata,nc=20){
clustering <- (nrow(normaliseddata)-1)*sum(apply(normaliseddata,2,var)) for(i in 2:nc)
clustering[i]<-sum(kmeans(normaliseddata,centers=i)$withinss) plot(1:nc, clustering, type="b", xlab="Number of Clusters", ylab="Within
   group sums of squares")
}
evaluationvariable(normaliseddata, nc=20)
# In addition, we can run it as the following (another way to run the same
 procedure)
k.max <- 20
data <- scaled_data wss <- sapply(1:k.max,
function(k){kmeans(normaliseddata, k, nstart=50,iter.max = 20 )$tot.withinss})
wss
plot(1:k.max, wss,
type="b", pch = 19, frame = FALSE, xlab="Number of clusters K",
ylab="Total within-clusters sum of squares")
# We can see that 15 clusters lwoers within group variation to a
 sufficiently low extent
k.means.fit <- kmeans(normaliseddata, 15)
attributes(k.means.fit) k.means.fit$centers k.means.fit$cluster k.means.fit$tot.withinss

# Now we need to construct a classification mechanism to study those
 clusters
zomatoindianumeric$clusterised <- as.factor(k.means.fit$cluster)
treeclust1 <- rpart(cluster1~ .-clusterised-cluster7-cluster2-cluster3-cluster4-cluster5-cluster6-cluster8-cluster9-cluster10-cluster11-cluster12-cluster13-cluster14-cluster15, data= zomatoindianumeric, cp=.02)
rpart.plot(treeclust1, box.palette="RdBu", shadow.col="gray", nn=TRUE)
mean(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster1==1]) 
var(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster1==1]) 
mean(zomatoindianumeric$touristy[zomatoindianumeric$cluster1==1]) 
mean(zomatoindianumeric$od[zomatoindianumeric$cluster1==1]) 
mean(zomatoindianumeric$tb[zomatoindianumeric$cluster1==1]) 
mean(zomatoindianumeric$USDaveragecost[zomatoindianumeric$cluster1==1])
mean(zomatoindianumeric$centredist[zomatoindianumeric$cluster1==1]) 
summary(zomatoindianumeric)
summary(zomatoindianumeric$USDaveragecost)
treeclust3 <-rpart(cluster3~ .-clusterised-cluster7-cluster2-cluster1-cluster4-cluster5-cluster6-cluster8-cluster9-cluster10-cluster11-cluster12-cluster13-cluster14-cluster15, data= zomatoindianumeric, cp=.02)
rpart.plot(treeclust3, box.palette="RdBu", shadow.col="gray", nn=TRUE)
mean(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster4==4]) 
var(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster4==4]) 
mean(zomatoindianumeric$touristy[zomatoindianumeric$cluster4==4]) 
mean(zomatoindianumeric$od[zomatoindianumeric$cluster4==4]) 
mean(zomatoindianumeric$tb[zomatoindianumeric$cluster4==4]) 
mean(zomatoindianumeric$USDaveragecost[zomatoindianumeric$cluster4==4]) 
mean(zomatoindianumeric$centredist[zomatoindianumeric$cluster4==4]) 
summary(zomatoindianumeric)
summary(zomatoindianumeric$USDaveragecost)
treeclust4 <-rpart(cluster4~ .-clusterised-cluster7-cluster2-cluster3-cluster1-cluster5-cluster6-cluster8-cluster9-cluster10-cluster11-cluster12-cluster13-cluster14-cluster15, data= zomatoindianumeric, cp=.02)
rpart.plot(treeclust4, box.palette="RdBu", shadow.col="gray", nn=TRUE)
# In this cluster table booking is important. mean(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster4==4]) var(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster4==4]) mean(zomatoindianumeric$touristy[zomatoindianumeric$cluster4==4]) mean(zomatoindianumeric$od[zomatoindianumeric$cluster4==4]) mean(zomatoindianumeric$tb[zomatoindianumeric$cluster4==4]) mean(zomatoindianumeric$USDaveragecost[zomatoindianumeric$cluster4==4]) mean(zomatoindianumeric$centredist[zomatoindianumeric$cluster4==4]) summary(zomatoindianumeric)

summary(zomatoindianumeric$USDaveragecost)
treeclust6 <-rpart(cluster6~ .-clusterised-cluster7-cluster2-cluster3-cluster4-cluster5-cluster1-cluster8-cluster9-cluster10-cluster11-cluster12-cluster13-cluster14-cluster15, data= zomatoindianumeric, cp=.02)
rpart.plot(treeclust6, box.palette="RdBu", shadow.col="gray", nn=TRUE)
mean(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster8==8])
mean(zomatoindianumeric$touristy[zomatoindianumeric$cluster8==8]) 
mean(zomatoindianumeric$od[zomatoindianumeric$cluster8==8]) 
mean(zomatoindianumeric$tb[zomatoindianumeric$cluster8==8]) 
mean(zomatoindianumeric$USDaveragecost[zomatoindianumeric$cluster8==8])
mean(zomatoindianumeric$centredist[zomatoindianumeric$cluster8==8]) 
summary(zomatoindianumeric)
treeclust7 <-rpart(cluster7~ .-clusterised-cluster1-cluster2-cluster3-cluster4-cluster5-cluster6-cluster8-cluster9-cluster10-cluster11-cluster12-cluster13-cluster14-cluster15, data= zomatoindianumeric, cp=.02)
rpart.plot(treeclust7, box.palette="RdBu", shadow.col="gray", nn=TRUE)
mean(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster7==7]) 
mean(zomatoindianumeric$touristy[zomatoindianumeric$cluster7==7])
mean(zomatoindianumeric$od[zomatoindianumeric$cluster7==7]) 
mean(zomatoindianumeric$tb[zomatoindianumeric$cluster7==7]) 
mean(zomatoindianumeric$USDaveragecost[zomatoindianumeric$cluster7==7])
mean(zomatoindianumeric$centredist[zomatoindianumeric$cluster7==7]) 
summary(zomatoindianumeric)
treeclust8 <-rpart(cluster8~ .-clusterised-cluster1-cluster2-cluster3-cluster4-cluster5-cluster6-cluster7-cluster9-cluster10-cluster11-cluster12
-cluster13-cluster14-cluster15, data= zomatoindianumeric, cp=.02)
rpart.plot(treeclust8, box.palette="RdBu", shadow.col="gray", nn=TRUE)
# Examine this cluster mean(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster8==8]) mean(zomatoindianumeric$touristy[zomatoindianumeric$cluster8==8]) mean(zomatoindianumeric$od[zomatoindianumeric$cluster8==8]) mean(zomatoindianumeric$tb[zomatoindianumeric$cluster8==8]) mean(zomatoindianumeric$USDaveragecost[zomatoindianumeric$cluster8==8]) mean(zomatoindianumeric$centredist[zomatoindianumeric$cluster8==8]) summary(zomatoindianumeric)
# None in a tourity place.All have online delivery. Very cheap in 2nd quarter. Very close to city centre (almost in 1st quartile).

treeclust9 <-rpart(cluster9~ .-clusterised-cluster7-cluster2-cluster3-cluster4-cluster5-cluster6-cluster8-cluster1-cluster10-cluster11-cluster12-cluster13-cluster14-cluster15, data= zomatoindianumeric, cp=.02)
rpart.plot(treeclust9, box.palette="RdBu", shadow.col="gray", nn=TRUE)
mean(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster9==9])
var(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster9==9]) 
mean(zomatoindianumeric$touristy[zomatoindianumeric$cluster9==9]) 
mean(zomatoindianumeric$od[zomatoindianumeric$cluster9==9]) 
mean(zomatoindianumeric$tb[zomatoindianumeric$cluster9==9]) 
mean(zomatoindianumeric$USDaveragecost[zomatoindianumeric$cluster9==9])
mean(zomatoindianumeric$centredist[zomatoindianumeric$cluster9==9])
summary(zomatoindianumeric)
treeclust11 <-rpart(cluster11~ .-clusterised-cluster7-cluster2-cluster3-cluster4-cluster5-cluster6-cluster8-cluster9-cluster10-cluster1-cluster12-cluster13-cluster14-cluster15, data= zomatoindianumeric, cp=.02)
rpart.plot(treeclust11, box.palette="RdBu", shadow.col="gray", nn=TRUE)
# Cluster 11 looks shorter than others. Let's examine it.
mean(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster11==11]) 
var(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster11==11]) 
mean(zomatoindianumeric$touristy[zomatoindianumeric$cluster11==11])
mean(zomatoindianumeric$od[zomatoindianumeric$cluster11==11]) 
mean(zomatoindianumeric$tb[zomatoindianumeric$cluster11==11]) 
mean(zomatoindianumeric$USDaveragecost[zomatoindianumeric$cluster11==11]) 
mean(zomatoindianumeric$centredist[zomatoindianumeric$cluster11==11]) 
summary(zomatoindianumeric)
# The cost of restrautanrs in this cluster appear to be higher than the
 average cost of restaurants
# and higher than the 3rd quartile. As such those are quite expensive restaurants. However, they have
# average rating. Almost all of them are in touristy places (99.2%) and all of them have online delivery.
# All of them also have table booking. They are close to average distance to city centre.
# Located in touristy cities (approximately 65%).
treeclust12 <-rpart(cluster12~ .-clusterised-cluster7-cluster2-cluster3-cluster4-cluster5-cluster6-cluster8-cluster9-cluster10-cluster11-cluster1-cluster13-cluster14-cluster15, data= zomatoindianumeric, cp=.02)
rpart.plot(treeclust12, box.palette="RdBu", shadow.col="gray", nn=TRUE)
# In this cluster the distance appears to play a key role. mean(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster12==12])

var(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster12==12])
mean(zomatoindianumeric$touristy[zomatoindianumeric$cluster12==12])
mean(zomatoindianumeric$od[zomatoindianumeric$cluster12==12]) 
mean(zomatoindianumeric$tb[zomatoindianumeric$cluster12==12]) 
mean(zomatoindianumeric$USDaveragecost[zomatoindianumeric$cluster12==12]) 
mean(zomatoindianumeric$centredist[zomatoindianumeric$cluster12==12]) 
summary(zomatoindianumeric)
# Those have a high average rating (3.93 - higher than 3rd quartile). All in touristy places. Low cost - lower than median.
# And higher distance from city centre (higher than 3rd quartile). No table booking for any of them.
# And all of them have online delivery.
treeclust13 <-rpart(cluster13~ .-clusterised-cluster7-cluster2-cluster3-cluster4-cluster5-cluster6-cluster8-cluster9-cluster10-cluster11-cluster1-cluster12-cluster14-cluster15, data= zomatoindianumeric, cp=.02)
rpart.plot(treeclust13, box.palette="RdBu", shadow.col="gray", nn=TRUE)
mean(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster13==13]) 
var(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster13==13])
mean(zomatoindianumeric$touristy[zomatoindianumeric$cluster13==13])
mean(zomatoindianumeric$od[zomatoindianumeric$cluster13==13]) 
mean(zomatoindianumeric$tb[zomatoindianumeric$cluster13==13]) 
mean(zomatoindianumeric$USDaveragecost[zomatoindianumeric$cluster13==13]) 
mean(zomatoindianumeric$centredist[zomatoindianumeric$cluster13==13])
summary(zomatoindianumeric)
treeclust14 <-rpart(cluster14~ .-clusterised-cluster7-cluster2-cluster3-cluster4-cluster5-cluster6-cluster8-cluster9-cluster10-cluster11-cluster1-cluster13-cluster12-cluster15, data= zomatoindianumeric, cp=.02)
rpart.plot(treeclust14, box.palette="RdBu", shadow.col="gray", nn=TRUE)
mean(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster14==14]) 
var(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster14==14]) 
mean(zomatoindianumeric$touristy[zomatoindianumeric$cluster14==14]) 
mean(zomatoindianumeric$od[zomatoindianumeric$cluster14==14]) 
mean(zomatoindianumeric$tb[zomatoindianumeric$cluster14==14]) 
mean(zomatoindianumeric$USDaveragecost[zomatoindianumeric$cluster14==14]) 
mean(zomatoindianumeric$centredist[zomatoindianumeric$cluster14==14]) 
summary(zomatoindianumeric)
treeclust15 <-rpart(cluster15~ .-clusterised-cluster7-cluster2-cluster3-cluster4-cluster5-cluster6-cluster8-cluster9-cluster10-cluster11-cluster1-cluster13-cluster14-cluster12, data= zomatoindianumeric, cp=.02)
rpart.plot(treeclust15, box.palette="RdBu", shadow.col="gray", nn=TRUE)

mean(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster15==15]) 
var(zomatoindianumeric$Aggregate.rating[zomatoindianumeric$cluster15==15]) 
mean(zomatoindianumeric$touristy[zomatoindianumeric$cluster15==15]) 
mean(zomatoindianumeric$od[zomatoindianumeric$cluster15==15]) 
mean(zomatoindianumeric$tb[zomatoindianumeric$cluster15==15]) 
mean(zomatoindianumeric$USDaveragecost[zomatoindianumeric$cluster15==15]) 
mean(zomatoindianumeric$centredist[zomatoindianumeric$cluster15==15]) 
summary(zomatoindianumeric)
#regression analysis - using ncvTest and Anova to test regression assumptions
#prepare data frame for regression
attach(zomatoindia)
str(zomatoindia)
zomatoreg<-data.frame(`Aggregate rating`, `Average Cost for two`, `od`,
`tb`, `touristy`, `capital`, `centredist`, `cuisinenumber`)
#stepwise regression
null=lm(`Aggregate rating`~1,zomatoreg) #fits the model with only an
intercept
summary(null)
full<-lm(`Aggregate rating`~`Average Cost for two`+ `od`+
`tb`+`touristy`+`capital`+`centredist`+`cuisinenumber`,zomatoreg)#fits the model with all variables
summary(full) #fits the model with all explanatory variables
stepback=step(full, data=zomatoreg, direction="backward")
summary(stepback)
anova(stepback)
car::ncvTest(stepback)
stepfor=step(null, data=zomatoreg, direction="forward")
summary(stepfor)
anova(stepfor)
car::ncvTest(stepfor)
stepboth=step(null, scope=list(lower=null, upper=full),direction="forward")
summary(stepboth)
anova(stepboth)
durbinWatsonTest(stepboth)
car::ncvTest(stepboth)
#further regression - just macrocontextual factors
reg1 <- lm(`Aggregate rating`~ touristy+capital+centredist, zomatoreg) summary(reg1)
anova(reg1)
car::ncvTest(reg1)
#further regression - just restaurant-specific factors
reg2 <- lm(`Aggregate rating`~ tb+od+`Average Cost for two`+cuisinenumber, zomatoreg)
summary(reg2)
anova(reg2)
car::ncvTest(reg2)
#testing interactions
zomatoreg$tourct<-zomatoreg$touristy*zomatoreg$centredist

reg3<-lm(`Aggregate rating`~tourct+centredist+touristy+tb+od+`Average Cost for two`+cuisinenumber, zomatoreg)
summary(reg3)
anova(reg3)
car::ncvTest(reg3)
zomatoreg$capcui<-zomatoreg$cuisinenumber*zomatoreg$capital reg4<-lm(`Aggregate rating`~capcui+capital+touristy+tb+od+`Average Cost for
two`+cuisinenumber, zomatoreg) summary(reg4)
anova(reg4)
car::ncvTest(reg4)
zomatoreg$distod<-zomatoreg$centredist*zomatoreg$od
reg5<-lm(`Aggregate rating`~distod+capital+touristy+tb+od+`Average Cost for
two`+cuisinenumber, zomatoreg) summary(reg5)
anova(reg5)
car::ncvTest(reg5)
#testing regression assumptions
plot(stepboth)
plot(reg1)
plot(reg2)
plot(reg3)
plot(reg4)
plot(reg5)
