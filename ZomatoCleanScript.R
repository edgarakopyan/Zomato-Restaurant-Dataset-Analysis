#zomato dataset - data cleaning

library(readr)
zomato <- read_csv("~/Documents/MYcadeMYa/current/LSE/fourth year 2018:19/ST309 Elementary Data Analytics/Project/zomato-restaurants-data/zomato.csv")
View(zomato)
summary(zomato)
#removing redundant variables 

drops <- c("Address","Locality","Locality Verbose","Switch to order menu", "Rating color", "Rating text")
zomatoclean <-zomato[ , !(names(zomato) %in% drops)]
View(zomatoclean)

#removing restaurants with one review only > if "Votes = <10 then remove row 
zomatoclean <- zomatoclean[!zomatoclean$Votes<30,]
View(zomatoclean)

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

#checking for missing values 
anyNA(zomatoclean)
sum(is.na(zomatoclean))
zomatoclean[!complete.cases(zomatoclean),]

#subset of restaurants with price>0
zomatocleanprice0 <- zomatoclean[!zomatoclean$USDaveragecost==0,]

summary(zomatoclean)

install.packages("tidyverse")
library(tidyverse)

#convert characters into numeric 
#Has Table booking 
zomatoclean$od=10
zomatoclean$od[zomatoclean$"Has Online delivery"=="Yes"]=1
zomatoclean$od[zomatoclean$"Has Online delivery"=="No"]=0

zomatoclean$dn=10
zomatoclean$dn[zomatoclean$"Is delivering now"=="Yes"]=1
zomatoclean$dn[zomatoclean$"Is delivering now"=="No"]=0

zomatoclean$tb=10
zomatoclean$tb[zomatoclean$"Has Table booking"=="Yes"]=1
zomatoclean$tb[zomatoclean$"Has Table booking"=="No"]=0

cor(zomatoclean$od,zomatoclean$dn)
cor(zomatoclean$od,zomatoclean$tb)
