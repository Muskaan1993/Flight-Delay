# Flight-Delay
# course : CS 513-B
#kddmproject
#ANN

rm(list=ls())

data<-read.csv("C:\\Users\\anki\\Desktop\\Stevens\\SEM 1\\513\\Project\\kddm\\2008.csv")


odataset<-data
#odataset<-read.csv("C:/Users/anki/Desktop/Stevens/SEM 1/513/Project/kddm/new_data_set.csv")
attach(odataset)


odataset$DepDelay_cat[DepDelay < -5] <- "EARLY"
odataset$DepDelay_cat[DepDelay >= -5 & DepDelay <= 5] <- "ONTIME"
odataset$DepDelay_cat[DepDelay > 5] <- "LATE"

odataset$ArrDelay_cat[ArrDelay < -5] <- "EARLY"
odataset$ArrDelay_cat[ArrDelay >= -5 & ArrDelay <= 10] <- "ONTIME"
odataset$ArrDelay_cat[ArrDelay >  10]<-"LATE"


#odataset$ArrDelay_cat[ArrDelay >  5 & ArrDelay<=10] <- "late"
#odataset$ArrDelay_cat[ArrDelay >  10] <- "very late"


#filtereddataset <- cbind(Month,DayofMonth,DayOfWeek,DepTime,CRSDepTime,ArrTime,CRSArrTime,UniqueCarrier,Origin,Dest,Distance,Diverted,Cancelled,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay,ArrDelay,DepDelay,"ArrDelay_cat"=odataset$ArrDelay_cat,"DepDelay_cat"=odataset$DepDelay_cat)
filtereddataset <- data.frame(Month,DayofMonth,DayOfWeek,DepTime,CRSDepTime,ArrTime,CRSArrTime,UniqueCarrier,Origin,Dest,Distance,Diverted,Cancelled,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay,ArrDelay,DepDelay,"ArrDelay_cat"=odataset$ArrDelay_cat,"DepDelay_cat"=odataset$DepDelay_cat)

detach (odataset)

attach(filtereddataset)

#departure delay
#Diverted
#we are filtering out the flights which are cancelled
filtereddataset<-filtereddataset[filtereddataset[,"Cancelled"]==0,]


*****
CODE

#####################################START########################################

rm(list=ls())

data<-read.csv("C:\\Users\\anki\\Desktop\\Stevens\\SEM 1\\513\\Project\\kddm\\2008.csv")


odataset<-data
#odataset<-read.csv("C:/Users/anki/Desktop/Stevens/SEM 1/513/Project/kddm/new_data_set.csv")
attach(odataset)


odataset$DepDelay_cat[DepDelay < -5] <- "EARLY"
odataset$DepDelay_cat[DepDelay >= -5 & DepDelay <= 5] <- "ONTIME"
odataset$DepDelay_cat[DepDelay > 5] <- "LATE"

odataset$ArrDelay_cat[ArrDelay < -5] <- "EARLY"
odataset$ArrDelay_cat[ArrDelay >= -5 & ArrDelay <= 10] <- "ONTIME"
odataset$ArrDelay_cat[ArrDelay >  10]<-"LATE"


#odataset$ArrDelay_cat[ArrDelay >  5 & ArrDelay<=10] <- "late"
#odataset$ArrDelay_cat[ArrDelay >  10] <- "very late"


#filtereddataset <- cbind(Month,DayofMonth,DayOfWeek,DepTime,CRSDepTime,ArrTime,CRSArrTime,UniqueCarrier,Origin,Dest,Distance,Diverted,Cancelled,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay,ArrDelay,DepDelay,"ArrDelay_cat"=odataset$ArrDelay_cat,"DepDelay_cat"=odataset$DepDelay_cat)
filtereddataset <- data.frame(Month,DayofMonth,DayOfWeek,DepTime,CRSDepTime,ArrTime,CRSArrTime,UniqueCarrier,Origin,Dest,Distance,Diverted,Cancelled,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay,ArrDelay,DepDelay,"ArrDelay_cat"=odataset$ArrDelay_cat,"DepDelay_cat"=odataset$DepDelay_cat)

detach (odataset)

attach(filtereddataset)

#departure delay
#Diverted
#we are filtering out the flights which are cancelled
filtereddataset<-filtereddataset[filtereddataset[,"Cancelled"]==0,]

#we are filtering out the flights which are Diverted
filtereddataset<-filtereddataset[filtereddataset[,"Diverted"]==0,]

#we are filtering out the flights which are latedeparture
filtereddataset<-filtereddataset[filtereddataset[,"DepDelay"]>=5,]



#we are filtering out the flights which are early arrival-removed in minus
#filtereddataset<-filtereddataset[filtereddataset[,"ArrDelay"]>0,]

filtereddataset<-filtereddataset[filtereddataset$UniqueCarrier =="WN" | filtereddataset$UniqueCarrier =="OO" | filtereddataset$UniqueCarrier =="AA", ] 

filtereddataset<-filtereddataset[filtereddataset$Origin =="ATL" | filtereddataset$Origin =="ORD" | filtereddataset$Origin =="LAX", ] 


#removing arrival time NA
filtereddataset<-filtereddataset[complete.cases(filtereddataset[,6]),]

#filtereddataset<-read.csv("C:\\Users\\anki\\Desktop\\Stevens\\SEM 1\\513\\Project\\kddm\\new2.csv",stringsAsFactors = FALSE)
attach(filtereddataset)

#filtereddataset$ArrDelay_cat[ArrDelay >= -5 & ArrDelay <= 5] <- "ONTIME"
#filtereddataset$ArrDelay_cat[ArrDelay < -5] <- "Early"
#filtereddataset$ArrDelay_cat[ArrDelay >  5] <- "late"
library(plyr)
#View(data_new)
data<-join(filtereddataset,count(filtereddataset,'Dest'))
#View(data)

attach(filtereddataset)
filtereddataset$Dest_Type[data$freq > 500 & data$freq<1000] <- "Med Busy"
filtereddataset$Dest_Type[data$freq < 500] <- "Less Busy"
filtereddataset$Dest_Type[data$freq >1000] <- "High Busy"

detach(odataset)

#View(filtereddataset)
summary(filtereddataset)

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
mmnorm <-function(x) {z<-((x-min(x))/(max(x)-min(x)));return(z) }

#making new data set with 76719 records

write.csv(filtereddataset,file="C:\\Users\\anki\\Desktop\\Stevens\\SEM1\\513\\Project\\kddm\\new4.csv")
#filtereddataset<-read.csv("C:\\Users\\anki\\Desktop\\Stevens\\SEM1\\513\\Project\\kddm\\new4.csv")
data<-filtereddataset
#View(data)

#############################################################################
#####################-----------Knn---------------------

data_new<-cbind(Month=mmnorm(data$Month),
                DayOfMonth=mmnorm(data$DayofMonth),
                DayOfWeek=mmnorm(data$DayOfWeek),
                CRSDepTime=mmnorm(data$CRSDepTime),
                CRSArrTime=mmnorm(data$CRSArrTime) ,
                UniqueCarrier=mmnorm(as.numeric(factor(data$UniqueCarrier))),
                Origin=mmnorm(as.numeric(factor(data$Origin))),
                Dest=mmnorm(as.numeric(factor(data$Dest_Type))),
                ArrDelay_cat=as.character(data$ArrDelay_cat)
)
idx1<-seq(1:5000)
data_new<-data_new[idx1,]
idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))
training<-data_new[idx,]
test<-data_new[-idx,]
library(class)

####to find proper k value::
#running knn 50 time for itterative  k   starting from k=1 to k=20 
# here which k's average error rate is minimumm,that k is best.

for (j in 1:40){
  counter<- 0
  total<-0
  for (i in 1:50) {
    newpredict<-knn(training[,-9],test[,-9],training[,9],k <- j)
    newresults<-cbind(test,as.character(newpredict) )
    wrong<-newresults[,9]!=newresults[,10]
    rate<-sum(wrong)/length(wrong)
    
    #print(rate)
    rates<-rbind(rate,rate)
    total<-total+rate
    counter<-counter+1
  }
  #rates
  print(j)
  avg=total/counter
  print(avg)
}
#####################

avg

newpredict<-knn(training[,-9],test[,-9],training[,9],k=30)

newresults<-cbind(test,as.character(newpredict) )
head(newresults)
table(newresults[,9],newresults[,10])

#############################################################################
#####################-----------KKnn---------------------

rm(list=ls())
library(kknn)
?kknn

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
mmnorm <-function(x) {z<-((x-min(x))/(max(x)-min(x)));return(z) }

filtereddataset<-read.csv("C:/Users/anki/Desktop/Stevens/SEM 1/513/Project/kddm/new4.csv")

attach(filtereddataset)

filtereddataset$ArrDelay_catnew[ArrDelay < -5] <- "Early"
filtereddataset$ArrDelay_catnew[ArrDelay >= -5 & ArrDelay <= 5] <- "ONTIME"
filtereddataset$ArrDelay_catnew[ArrDelay >  5 & ArrDelay <  20] <- "late"
filtereddataset$ArrDelay_catnew[ArrDelay >=  20] <- "VERY_late"

summary(ArrDelay)

data<-filtereddataset

data_new_k<-cbind(
  DayOfMonth=mmnorm(data$DayofMonth),
  DayOfWeek=mmnorm(data$DayOfWeek),
  CRSDepTime=mmnorm(data$CRSDepTime),
  CRSArrTime=mmnorm(data$CRSArrTime) ,
  UniqueCarrier=mmnorm(as.numeric(factor(data$UniqueCarrier))),
  Origin=mmnorm(as.numeric(factor(data$Origin))),
  Dest=mmnorm(as.numeric(factor(data$Dest_Type))),
  ArrDelay_cat=as.character(data$ArrDelay_catnew)
)
data_new_k<-as.data.frame(data_new_k)
data_new_k<-na.omit(data_new_k)
factor(data_new_k$ArrDelay_cat)
is.data.frame(data_new_k)
idx1<-seq(1:5000)
data_new_k<-data_new_k[idx1,]
idx<-sample(nrow(data_new_k),as.integer(.70*nrow(data_new_k)))
trainingk<-data_new_k[idx,]
testk<-data_new_k[-idx,]
is.data.frame(trainingk)
#applying kknn
predict_1 <- kknn(formula=ArrDelay_cat~., trainingk, testk, k=38,kernel="optimal")
head(predict_1)
fitWalc <- fitted(predict_1)
results <- cbind(testk$ArrDelay_cat, fitWalc)
wrong <- results[,1]!=results[,2]
rateWalc <- sum(wrong)/length(wrong)
rateWalc


#######################
####to find proper k value::
#running knn 50 time for itterative  k   starting from k=1 to k=20 
# here which k's average error rate is minimumm,that k is best.

filtereddataset<-read.csv("C:/Users/anki/Desktop/Stevens/SEM 1/513/Project/kddm/new4.csv")

filtereddataset$ArrDelay_catnew[ArrDelay < -5] <- "Early"
filtereddataset$ArrDelay_catnew[ArrDelay >= -5 & ArrDelay <= 5] <- "ONTIME"
filtereddataset$ArrDelay_catnew[ArrDelay >  5 & ArrDelay <  100] <- "late"
filtereddataset$ArrDelay_catnew[ArrDelay >= 100] <- "VERY_late"

data_new<-cbind(Month=mmnorm(data$Month),
                DayOfMonth=mmnorm(data$DayofMonth),
                DayOfWeek=mmnorm(data$DayOfWeek),
                CRSDepTime=mmnorm(data$CRSDepTime),
                CRSArrTime=mmnorm(data$CRSArrTime) ,
                UniqueCarrier=mmnorm(as.numeric(factor(data$UniqueCarrier))),
                Origin=mmnorm(as.numeric(factor(data$Origin))),
                Dest=mmnorm(as.numeric(factor(data$Dest_Type))),
                #DepDelay=mmnorm(data$DepDelay),
                ArrDelay_cat=as.character(data$ArrDelay_catnew)
)

idx1<-seq(1:5000)
data_new<-data_new[idx1,]

idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))


training<-data_new[idx,]
test<-data_new[-idx,]
for (j in 1:40){
  counter<- 0
  total<-0
  for (i in 1:50) {
    newpredict<-knn(training[,-9],test[,-9],training[,9],k <- j)
    newresults<-cbind(test,as.character(newpredict) )
    wrong<-newresults[,9]!=newresults[,10]
    rate<-sum(wrong)/length(wrong)
    
    #print(rate)
    rates<-rbind(rate,rate)
    total<-total+rate
    counter<-counter+1
  }
  #rates
  print(j)
  avg=total/counter
  print(avg)
}

#############################################################################
#####################-----------ANN---------------------

data_new<-cbind(DayOfWeek=data$DayOfWeek,
                CRSDepTime=data$CRSDepTime,
                CRSArrTime=data$CRSArrTime,
                UniqueCarrier=as.numeric(factor(data$UniqueCarrier)),
                Origin=as.numeric(factor(data$Origin)),
                Dest=as.numeric(factor(data$Dest_Type)),
                DepDelay=mmnorm(data$DepDelay),
                ArrDelay_cat=as.numeric(factor(data$ArrDelay_catnew))
)
data_new<-as.data.frame (data_new)
data_new<-na.omit(data_new)
factor(data_new$ArrDelay_cat)
is.data.frame(data_new)
idx1<-seq(1:100)
data_new<-data_new[idx1,]

idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))

training<-data_new[idx,]
test<-data_new[-idx,]
is.data.frame(training)

if(!require(neuralnet)) {
  install.packages("neuralnet"); require(neuralnet)}

rate<-0
wrong<-0
net.ArrDelay <- neuralnet(ArrDelay_cat~DayOfWeek+CRSDepTime+CRSArrTime+
                            UniqueCarrier+Origin+Dest+DepDelay,
                          training,hidden=5, threshold=0.01,stepmax=1e6)
#to plot the neuralnet
plot(net.ArrDelay)
net.result1 <- compute(net.ArrDelay, subset(test, select=-ArrDelay_cat))
fit <- round(net.result1$net.result, digits = 0)
results <- cbind(test$ArrDelay_cat, fit)
wrong <- results[,1]!=results[,2]
rate <- sum(wrong)/length(wrong)
rate

#############################################################################
#####################-----------CART---------------------

data_new<-data.frame(DayOfMonth=(filtereddataset$DayofMonth_cat),
                     DayOfWeek=(filtereddataset$week_cat),
                     CRSDepTime=(na.zero(filtereddataset$CRSDepTime)),
                     CRSArrTime=(na.zero(filtereddataset$CRSArrTime)),
                     UniqueCarrier=(filtereddataset$UniqueCarrier),
                     Origin=(filtereddataset$Origin),
                     Dest=(filtereddataset$Dest_Type),
                     Distance=(filtereddataset$Distance_cat),
                     ArrDelay_cat=(filtereddataset$ArrDelay_cat)
                     
)
set.seed(9850)

#creating training and test dataset based on 70%-30% ratio
idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))
training<-data_new[idx,]
test<-data_new[-idx,]

#fitting the classification tree using the rpart function
library(rpart)
dtm <- rpart(ArrDelay_cat~.,data= training, method= "class")

printcp(dtm)

#gives the summary
summary(dtm)
#plotting the tree
library(rpart.plot)

#predicting for test data ad testing the accuracy
rpart.plot(dtm, type=1, extra=101)
p3 <- predict(dtm,test,type="class")
table(test[,9], predicted= p3)

#############################################################################
#####################-----------C5.0 ---------------------
############Before Pruning
data <-filtereddataset
data_new<-cbind(
  DayOfMonth=data$DayofMonth,
  DayOfWeek=data$DayOfWeek,
  CRSDepTime=data$CRSDepTime,
  CRSArrTime=data$CRSArrTime,
  UniqueCarrier=data$UniqueCarrier,
  Origin=data$Origin,
  Dest=data$Dest_Type,
  DepDelay=data$DepDelay,
  weatherdly=data$WeatherDelay,
  NASdly=data$NASDelay,
  Securitydly=data$SecurityDelay,
  LateAircraftdly=data$LateAircraftDelay,
  ArrDelay_cat=as.numeric(factor(data$ArrDelay_cat))
)

#View(data_new)
detach (filtereddataset)
attach(data_new)
#install.packages("C50")
require(C50)
set.seed(9850)
idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))

#generating training dataset
training<-data_new[idx,]
nrow(training)

#generating test dataset
test<-data_new[-idx,]

#applying c5.0
m1 <- C5.0(training[,-13],factor(training[,13]))

#gives summary of the tree
summary(m1)

#plotting the tree
plot(m1)

#to  check  the accuracy of model
result<-predict(m1,test,type="class")
rTable<-table(predict=result,test=test[,13])
accuracy=(sum(diag(rTable))/nrow(test))
accuracy

############After Pruning

data <-filtereddataset
data_new<-cbind(UniqueCarrier=data$UniqueCarrier,
                DepDelay=data$DepDelay,
                weatherdly=data$WeatherDelay,
                NASdly=data$NASDelay,
                Securitydly=data$SecurityDelay,
                LateAircraftdly=data$LateAircraftDelay,
                ArrDelay_cat=as.numeric(factor(data$ArrDelay_cat))
)

idx1<-seq(1:1000)
data_new<-data_new[idx1,]
idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))

#generating training dataset 
training<-data_new[idx,]
nrow(training)

#generating test dataset 
test<-data_new[-idx,]
m1 <- C5.0(training[,-7],factor(training[,7]),trials=10)

#gives summary of the tree
summary(m1)

#plotting the tree
plot(m1)

#to  check  the accuracy of model
result<-predict(m1,test,type="class")
rTable<-table(predict=result,test=test[,7])
accuracy=(sum(diag(rTable))/nrow(test))
accuracy

#####################################END########################################







#we are filtering out the flights which are Diverted
filtereddataset<-filtereddataset[filtereddataset[,"Diverted"]==0,]

#we are filtering out the flights which are latedeparture
filtereddataset<-filtereddataset[filtereddataset[,"DepDelay"]>=5,]



#we are filtering out the flights which are early arrival-removed in minus
#filtereddataset<-filtereddataset[filtereddataset[,"ArrDelay"]>0,]

filtereddataset<-filtereddataset[filtereddataset$UniqueCarrier =="WN" | filtereddataset$UniqueCarrier =="OO" | filtereddataset$UniqueCarrier =="AA", ] 

filtereddataset<-filtereddataset[filtereddataset$Origin =="ATL" | filtereddataset$Origin =="ORD" | filtereddataset$Origin =="LAX", ] 


#removing arrival time NA
filtereddataset<-filtereddataset[complete.cases(filtereddataset[,6]),]

#filtereddataset<-read.csv("C:\\Users\\anki\\Desktop\\Stevens\\SEM 1\\513\\Project\\kddm\\new2.csv",stringsAsFactors = FALSE)
attach(filtereddataset)

#filtereddataset$ArrDelay_cat[ArrDelay >= -5 & ArrDelay <= 5] <- "ONTIME"
#filtereddataset$ArrDelay_cat[ArrDelay < -5] <- "Early"
#filtereddataset$ArrDelay_cat[ArrDelay >  5] <- "late"
library(plyr)
#View(data_new)
data<-join(filtereddataset,count(filtereddataset,'Dest'))
#View(data)



attach(filtereddataset)

filtereddataset$Dest_Type[data$freq > 500 & data$freq<1000] <- "Med Busy"
filtereddataset$Dest_Type[data$freq < 500] <- "Less Busy"
filtereddataset$Dest_Type[data$freq >1000] <- "High Busy"

detach(odataset)

#View(filtereddataset)
summary(filtereddataset)

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
mmnorm <-function(x) {z<-((x-min(x))/(max(x)-min(x)));return(z) }




#making new data set with 76719 records

write.csv(filtereddataset,file="C:\\Users\\anki\\Desktop\\Stevens\\SEM 1\\513\\Project\\kddm\\new4.csv")
#filtereddataset<-read.csv("C:\\Users\\anki\\Desktop\\Stevens\\SEM 1\\513\\Project\\kddm\\new4.csv")
data<-filtereddataset


#View(data)


data_new<-cbind(DayOfWeek=data$DayOfWeek,
                CRSDepTime=data$CRSDepTime,
                CRSArrTime=data$CRSArrTime,
                UniqueCarrier=as.numeric(factor(data$UniqueCarrier)),
                Origin=as.numeric(factor(data$Origin)),
                Dest=as.numeric(factor(data$Dest_Type)),
                DepDelay=mmnorm(data$DepDelay),
                ArrDelay_cat=as.numeric(factor(data$ArrDelay_catnew))
)
data_new<-as.data.frame (data_new)
data_new<-na.omit(data_new)
factor(data_new$ArrDelay_cat)
is.data.frame(data_new)
idx1<-seq(1:100)
data_new<-data_new[idx1,]

idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))

training<-data_new[idx,]
test<-data_new[-idx,]
is.data.frame(training)

if(!require(neuralnet)) {
  install.packages("neuralnet"); require(neuralnet)}

rate<-0
wrong<-0
net.ArrDelay <- neuralnet(ArrDelay_cat~DayOfWeek+CRSDepTime+CRSArrTime+
                            UniqueCarrier+Origin+Dest+DepDelay,
                          training,hidden=5, threshold=0.01,stepmax=1e6)
#to plot the neuralnet
plot(net.ArrDelay)
net.result1 <- compute(net.ArrDelay, subset(test, select=-ArrDelay_cat))
fit <- round(net.result1$net.result, digits = 0)
results <- cbind(test$ArrDelay_cat, fit)
wrong <- results[,1]!=results[,2]
rate <- sum(wrong)/length(wrong)
rate

*****************************

# course : CS 513-B
#kddmproject
#cs 5 after pruning

rm(list=ls())
library(plyr)


data<-read.csv("D:/CS 513B - KDDM/Project/Final/26thApril.csv")
#View (data)
filtereddataset<-sample(data)


filtereddataset$CarrierDelay[is.na(filtereddataset$CarrierDelay)] <- 0
filtereddataset$WeatherDelay[is.na(filtereddataset$WeatherDelay)] <- 0
filtereddataset$NASDelay[is.na(filtereddataset$NASDelay)] <- 0
filtereddataset$SecurityDelay[is.na(filtereddataset$SecurityDelay)] <- 0
filtereddataset$LateAircraftDelay[is.na(filtereddataset$LateAircraftDelay)] <- 0



attach(filtereddataset)

filtereddataset$DayofMonth_cat[DayofMonth > 15] <- "second_half"
filtereddataset$DayofMonth_cat[DayofMonth <= 15] <- "First_half"

filtereddataset$week_cat[DayOfWeek == 1 | DayOfWeek ==7] <- "weekend"
filtereddataset$week_cat[DayOfWeek < 7 & DayOfWeek > 1] <- "weekday"

filtereddataset$ArrDelay_cat[ArrDelay <= 5] <- "EARLY"
filtereddataset$ArrDelay_cat[ArrDelay > 5 & ArrDelay <= 60] <- "ONTIME"
filtereddataset$ArrDelay_cat[ArrDelay >  60] <- "LATE"

#filtereddataset$ArrDelay_cat[ArrDelay < -5] <- "EARLY"
#filtereddataset$ArrDelay_cat[ArrDelay >= -5 & ArrDelay <= 10] <- "ONTIME"
#filtereddataset$ArrDelay_cat[ArrDelay >  10] <- "LATE"

filtereddataset$Distance_cat[Distance <= 1000] <- "short_distance"
filtereddataset$Distance_cat[Distance > 1000 & Distance <=  2000] <- "Mid_distance"
filtereddataset$Distance_cat[Distance > 2000] <- "Long_distance"

#View(filtereddataset)
filtereddataset<-join(filtereddataset,count(filtereddataset,'Dest'))


filtereddataset$Dest_Type[filtereddataset$freq < 500] <- "Less Busy"
filtereddataset$Dest_Type[filtereddataset$freq > 500 & filtereddataset$freq<1000] <- "Med Busy"
filtereddataset$Dest_Type[filtereddataset$freq >1000] <- "High Busy"


filtereddataset$ArrDelay_value[filtereddataset$ArrDelay_cat == "ONTIME"] <- 1
filtereddataset$ArrDelay_value[filtereddataset$ArrDelay_cat == "Mid Delay"] <- 2
filtereddataset$ArrDelay_value[filtereddataset$ArrDelay_cat == "High Delay"] <- 3

data <-filtereddataset


data_new<-cbind(UniqueCarrier=data$UniqueCarrier,
                DepDelay=data$DepDelay,
                weatherdly=data$WeatherDelay,
                NASdly=data$NASDelay,
                Securitydly=data$SecurityDelay,
                LateAircraftdly=data$LateAircraftDelay,
                ArrDelay_cat=as.numeric(factor(data$ArrDelay_cat))
)

idx1<-seq(1:1000)
data_new<-data_new[idx1,]
idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))

#generating training dataset 
training<-data_new[idx,]
nrow(training)

#generating test dataset 
test<-data_new[-idx,]
m1 <- C5.0(training[,-7],factor(training[,7]),trials=10)

#gives summary of the tree
summary(m1)

#plotting the tree
plot(m1)

#to  check  the accuracy of model
result<-predict(m1,test,type="class")
rTable<-table(predict=result,test=test[,7])
accuracy=(sum(diag(rTable))/nrow(test))
accuracy


***************
# course : CS 513-B
#kddmproject
#cs 5 before pruning

rm(list=ls())
library(plyr)


data<-read.csv("D:/CS 513B - KDDM/Project/Final/26thApril.csv")
#View (data)
filtereddataset<-sample(data)


filtereddataset$CarrierDelay[is.na(filtereddataset$CarrierDelay)] <- 0
filtereddataset$WeatherDelay[is.na(filtereddataset$WeatherDelay)] <- 0
filtereddataset$NASDelay[is.na(filtereddataset$NASDelay)] <- 0
filtereddataset$SecurityDelay[is.na(filtereddataset$SecurityDelay)] <- 0
filtereddataset$LateAircraftDelay[is.na(filtereddataset$LateAircraftDelay)] <- 0



attach(filtereddataset)

filtereddataset$DayofMonth_cat[DayofMonth > 15] <- "second_half"
filtereddataset$DayofMonth_cat[DayofMonth <= 15] <- "First_half"

filtereddataset$week_cat[DayOfWeek == 1 | DayOfWeek ==7] <- "weekend"
filtereddataset$week_cat[DayOfWeek < 7 & DayOfWeek > 1] <- "weekday"

filtereddataset$ArrDelay_cat[ArrDelay <= 5] <- "EARLY"
filtereddataset$ArrDelay_cat[ArrDelay > 5 & ArrDelay <= 60] <- "ONTIME"
filtereddataset$ArrDelay_cat[ArrDelay >  60] <- "LATE"

#filtereddataset$ArrDelay_cat[ArrDelay < -5] <- "EARLY"
#filtereddataset$ArrDelay_cat[ArrDelay >= -5 & ArrDelay <= 10] <- "ONTIME"
#filtereddataset$ArrDelay_cat[ArrDelay >  10] <- "LATE"

filtereddataset$Distance_cat[Distance <= 1000] <- "short_distance"
filtereddataset$Distance_cat[Distance > 1000 & Distance <=  2000] <- "Mid_distance"
filtereddataset$Distance_cat[Distance > 2000] <- "Long_distance"

#View(filtereddataset)
filtereddataset<-join(filtereddataset,count(filtereddataset,'Dest'))


filtereddataset$Dest_Type[filtereddataset$freq < 500] <- "Less Busy"
filtereddataset$Dest_Type[filtereddataset$freq > 500 & filtereddataset$freq<1000] <- "Med Busy"
filtereddataset$Dest_Type[filtereddataset$freq >1000] <- "High Busy"


filtereddataset$ArrDelay_value[filtereddataset$ArrDelay_cat == "ONTIME"] <- 1
filtereddataset$ArrDelay_value[filtereddataset$ArrDelay_cat == "Mid Delay"] <- 2
filtereddataset$ArrDelay_value[filtereddataset$ArrDelay_cat == "High Delay"] <- 3

data <-filtereddataset


data_new<-cbind(
  DayOfMonth=data$DayofMonth,
  DayOfWeek=data$DayOfWeek,
  CRSDepTime=data$CRSDepTime,
  CRSArrTime=data$CRSArrTime,
  UniqueCarrier=data$UniqueCarrier,
  Origin=data$Origin,
  Dest=data$Dest_Type,
  DepDelay=data$DepDelay,
  weatherdly=data$WeatherDelay,
  NASdly=data$NASDelay,
  Securitydly=data$SecurityDelay,
  LateAircraftdly=data$LateAircraftDelay,
  ArrDelay_cat=as.numeric(factor(data$ArrDelay_cat))
)

#View(data_new)


detach (filtereddataset)

attach(data_new)


#install.packages("C50")
require(C50)

set.seed(9850)


idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))

#generating training dataset
training<-data_new[idx,]
nrow(training)

#generating test dataset
test<-data_new[-idx,]

#applying C5.0
m1 <- C5.0(training[,-13],factor(training[,13]))

#gives summary of the tree
summary(m1)

#plotting the tree
plot(m1)

#to  check  the accuracy of model
result<-predict(m1,test,type="class")
rTable<-table(predict=result,test=test[,13])
accuracy=(sum(diag(rTable))/nrow(test))
accuracy

************
# course : CS 513-B
#kddmproject

rm(list=ls())

#install.packages("plyr")
library(plyr)

#defining the function for  normalization
mmnorm<-function(x)
{
  z<-((x-min(x))/(max(x)-min(x)))
  return(z)
}

#defining the na.zero function
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}


#rm(list=ls())

data<-read.csv("D:/CS 513B - KDDM/Project/Final/20April.csv")
View (data)
filtereddataset<-sample(data)


filtereddataset$CarrierDelay[is.na(filtereddataset$CarrierDelay)] <- 0
filtereddataset$WeatherDelay[is.na(filtereddataset$WeatherDelay)] <- 0
filtereddataset$NASDelay[is.na(filtereddataset$NASDelay)] <- 0
filtereddataset$SecurityDelay[is.na(filtereddataset$SecurityDelay)] <- 0
filtereddataset$LateAircraftDelay[is.na(filtereddataset$LateAircraftDelay)] <- 0



attach(filtereddataset)

filtereddataset$DayofMonth_cat[DayofMonth > 15] <- "second_half"
filtereddataset$DayofMonth_cat[DayofMonth <= 15] <- "First_half"

filtereddataset$week_cat[DayOfWeek == 1 | DayOfWeek ==7] <- "weekend"
filtereddataset$week_cat[DayOfWeek < 7 & DayOfWeek > 1] <- "weekday"

#filtereddataset$ArrDelay_cat[ArrDelay < -5] <- "EARLY"
#filtereddataset$ArrDelay_cat[ArrDelay >= -5 & ArrDelay <= 10] <- "ONTIME"
#filtereddataset$ArrDelay_cat[ArrDelay >  10] <- "LATE"

filtereddataset$ArrDelay_cat[ArrDelay <= 5] <- "EARLY"
filtereddataset$ArrDelay_cat[ArrDelay > 5 & ArrDelay <= 60] <- "ONTIME"
filtereddataset$ArrDelay_cat[ArrDelay >  60] <- "LATE"

filtereddataset$Distance_cat[Distance <= 1000] <- "short_distance"
filtereddataset$Distance_cat[Distance > 1000 & Distance <=  2000] <- "Mid_distance"
filtereddataset$Distance_cat[Distance > 2000] <- "Long_distance"

View(filtereddataset)
filtereddataset<-join(filtereddataset,count(filtereddataset,'Dest'))
View(filtereddataset)

filtereddataset$Dest_Type[filtereddataset$freq < 500] <- "Less Busy"
filtereddataset$Dest_Type[filtereddataset$freq > 500 & filtereddataset$freq<1000] <- "Med Busy"
filtereddataset$Dest_Type[filtereddataset$freq >1000] <- "High Busy"


data_new<-data.frame(DayOfMonth=(filtereddataset$DayofMonth_cat),
                     DayOfWeek=(filtereddataset$week_cat),
                     CRSDepTime=(na.zero(filtereddataset$CRSDepTime)),
                     CRSArrTime=(na.zero(filtereddataset$CRSArrTime)),
                     UniqueCarrier=(filtereddataset$UniqueCarrier),
                     Origin=(filtereddataset$Origin),
                     Dest=(filtereddataset$Dest_Type),
                     Distance=(filtereddataset$Distance_cat),
                     ArrDelay_cat=(filtereddataset$ArrDelay_cat)
                     
)


set.seed(9850)
#creating training and test dataset  based on 70%-30% ratio
idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))
training<-data_new[idx,]
test<-data_new[-idx,]

#fitting the classification tree using the rpart function
library(rpart)
dtm <- rpart(ArrDelay_cat~.,data= training, method= "class")

printcp(dtm)

#gives the summary
summary(dtm)

#plotting the tree
library(rpart.plot)

#predicting for test data ad testing the accuracy
rpart.plot(dtm, type=1, extra=101)
p3 <- predict(dtm,test,type="class")
table(test[,9], predicted= p3)

***********
# course : CS 513-B
#kddmproject

rm(list=ls())

data<-read.csv("C:\\Users\\anki\\Desktop\\Stevens\\SEM 1\\513\\Project\\kddm\\2008.csv")


odataset<-data
#odataset<-read.csv("C:/Users/anki/Desktop/Stevens/SEM 1/513/Project/kddm/new_data_set.csv")
attach(odataset)


odataset$DepDelay_cat[DepDelay < -5] <- "EARLY"
odataset$DepDelay_cat[DepDelay >= -5 & DepDelay <= 5] <- "ONTIME"
odataset$DepDelay_cat[DepDelay > 5] <- "LATE"

odataset$ArrDelay_cat[ArrDelay < -5] <- "EARLY"
odataset$ArrDelay_cat[ArrDelay >= -5 & ArrDelay <= 10] <- "ONTIME"
odataset$ArrDelay_cat[ArrDelay >  10]<-"LATE"


#odataset$ArrDelay_cat[ArrDelay >  5 & ArrDelay<=10] <- "late"
#odataset$ArrDelay_cat[ArrDelay >  10] <- "very late"


#filtereddataset <- cbind(Month,DayofMonth,DayOfWeek,DepTime,CRSDepTime,ArrTime,CRSArrTime,UniqueCarrier,Origin,Dest,Distance,Diverted,Cancelled,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay,ArrDelay,DepDelay,"ArrDelay_cat"=odataset$ArrDelay_cat,"DepDelay_cat"=odataset$DepDelay_cat)
filtereddataset <- data.frame(Month,DayofMonth,DayOfWeek,DepTime,CRSDepTime,ArrTime,CRSArrTime,UniqueCarrier,Origin,Dest,Distance,Diverted,Cancelled,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay,ArrDelay,DepDelay,"ArrDelay_cat"=odataset$ArrDelay_cat,"DepDelay_cat"=odataset$DepDelay_cat)

detach (odataset)

attach(filtereddataset)

#departure delay
#Diverted
#we are filtering out the flights which are cancelled
filtereddataset<-filtereddataset[filtereddataset[,"Cancelled"]==0,]

#we are filtering out the flights which are Diverted
filtereddataset<-filtereddataset[filtereddataset[,"Diverted"]==0,]

#we are filtering out the flights which are latedeparture
filtereddataset<-filtereddataset[filtereddataset[,"DepDelay"]>=5,]



#we are filtering out the flights which are early arrival-removed in minus
#filtereddataset<-filtereddataset[filtereddataset[,"ArrDelay"]>0,]

filtereddataset<-filtereddataset[filtereddataset$UniqueCarrier =="WN" | filtereddataset$UniqueCarrier =="OO" | filtereddataset$UniqueCarrier =="AA", ] 

filtereddataset<-filtereddataset[filtereddataset$Origin =="ATL" | filtereddataset$Origin =="ORD" | filtereddataset$Origin =="LAX", ] 


#removing arrival time NA
filtereddataset<-filtereddataset[complete.cases(filtereddataset[,6]),]

#filtereddataset<-read.csv("C:\\Users\\anki\\Desktop\\Stevens\\SEM 1\\513\\Project\\kddm\\new2.csv",stringsAsFactors = FALSE)
attach(filtereddataset)

#filtereddataset$ArrDelay_cat[ArrDelay >= -5 & ArrDelay <= 5] <- "ONTIME"
#filtereddataset$ArrDelay_cat[ArrDelay < -5] <- "Early"
#filtereddataset$ArrDelay_cat[ArrDelay >  5] <- "late"
library(plyr)
#View(data_new)
data<-join(filtereddataset,count(filtereddataset,'Dest'))
#View(data)



attach(filtereddataset)

filtereddataset$Dest_Type[data$freq > 500 & data$freq<1000] <- "Med Busy"
filtereddataset$Dest_Type[data$freq < 500] <- "Less Busy"
filtereddataset$Dest_Type[data$freq >1000] <- "High Busy"

detach(odataset)

#View(filtereddataset)
summary(filtereddataset)

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
mmnorm <-function(x) {z<-((x-min(x))/(max(x)-min(x)));return(z) }




#making new data set with 76719 records

write.csv(filtereddataset,file="C:\\Users\\anki\\Desktop\\Stevens\\SEM 1\\513\\Project\\kddm\\new4.csv")
#filtereddataset<-read.csv("C:\\Users\\anki\\Desktop\\Stevens\\SEM 1\\513\\Project\\kddm\\new4.csv")
data<-filtereddataset


#View(data)



data_new<-cbind(Month=mmnorm(data$Month),
                DayOfMonth=mmnorm(data$DayofMonth),
                DayOfWeek=mmnorm(data$DayOfWeek),
                CRSDepTime=mmnorm(data$CRSDepTime),
                CRSArrTime=mmnorm(data$CRSArrTime) ,
                UniqueCarrier=mmnorm(as.numeric(factor(data$UniqueCarrier))),
                Origin=mmnorm(as.numeric(factor(data$Origin))),
                Dest=mmnorm(as.numeric(factor(data$Dest_Type))),
                ArrDelay_cat=as.character(data$ArrDelay_cat)
)
idx1<-seq(1:5000)
data_new<-data_new[idx1,]
idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))
training<-data_new[idx,]
test<-data_new[-idx,]
library(class)



####to find proper k value::
#running knn 50 time for itterative  k   starting from k=1 to k=20 
# here which k's average error rate is minimumm,that k is best.

for (j in 1:40){
  counter<- 0
  total<-0
  for (i in 1:50) {
    newpredict<-knn(training[,-9],test[,-9],training[,9],k <- j)
    newresults<-cbind(test,as.character(newpredict) )
    wrong<-newresults[,9]!=newresults[,10]
    rate<-sum(wrong)/length(wrong)
    
    #print(rate)
    rates<-rbind(rate,rate)
    total<-total+rate
    counter<-counter+1
  }
  #rates
  print(j)
  avg=total/counter
  print(avg)
}
######################

avg

newpredict<-knn(training[,-9],test[,-9],training[,9],k=30)

newresults<-cbind(test,as.character(newpredict) )
head(newresults)
table(newresults[,9],newresults[,10])




#############################################################################

#####################-----------KKnn---------------------

rm(list=ls())
library(kknn)
?kknn

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
mmnorm <-function(x) {z<-((x-min(x))/(max(x)-min(x)));return(z) }



filtereddataset<-read.csv("C:/Users/anki/Desktop/Stevens/SEM 1/513/Project/kddm/new4.csv")

attach(filtereddataset)

filtereddataset$ArrDelay_catnew[ArrDelay < -5] <- "Early"
filtereddataset$ArrDelay_catnew[ArrDelay >= -5 & ArrDelay <= 5] <- "ONTIME"
filtereddataset$ArrDelay_catnew[ArrDelay >  5 & ArrDelay <  20] <- "late"
filtereddataset$ArrDelay_catnew[ArrDelay >=  20] <- "VERY_late"

summary(ArrDelay)

data<-filtereddataset

data_new_k<-cbind(
  DayOfMonth=mmnorm(data$DayofMonth),
  DayOfWeek=mmnorm(data$DayOfWeek),
  CRSDepTime=mmnorm(data$CRSDepTime),
  CRSArrTime=mmnorm(data$CRSArrTime) ,
  UniqueCarrier=mmnorm(as.numeric(factor(data$UniqueCarrier))),
  Origin=mmnorm(as.numeric(factor(data$Origin))),
  Dest=mmnorm(as.numeric(factor(data$Dest_Type))),
  ArrDelay_cat=as.character(data$ArrDelay_catnew)
)
data_new_k<-as.data.frame(data_new_k)
data_new_k<-na.omit(data_new_k)
factor(data_new_k$ArrDelay_cat)
is.data.frame(data_new_k)
idx1<-seq(1:5000)
data_new_k<-data_new_k[idx1,]
idx<-sample(nrow(data_new_k),as.integer(.70*nrow(data_new_k)))
trainingk<-data_new_k[idx,]
testk<-data_new_k[-idx,]
is.data.frame(trainingk)
#applying kknn
predict_1 <- kknn(formula=ArrDelay_cat~., trainingk, testk, k=38,kernel="optimal")
head(predict_1)
fitWalc <- fitted(predict_1)
results <- cbind(testk$ArrDelay_cat, fitWalc)
wrong <- results[,1]!=results[,2]
rateWalc <- sum(wrong)/length(wrong)
rateWalc


#######################
####to find proper k value::
#running knn 50 time for itterative  k   starting from k=1 to k=20 
# here which k's average error rate is minimumm,that k is best.

filtereddataset<-read.csv("C:/Users/anki/Desktop/Stevens/SEM 1/513/Project/kddm/new4.csv")

filtereddataset$ArrDelay_catnew[ArrDelay < -5] <- "Early"
filtereddataset$ArrDelay_catnew[ArrDelay >= -5 & ArrDelay <= 5] <- "ONTIME"
filtereddataset$ArrDelay_catnew[ArrDelay >  5 & ArrDelay <  100] <- "late"
filtereddataset$ArrDelay_catnew[ArrDelay >= 100] <- "VERY_late"

data_new<-cbind(Month=mmnorm(data$Month),
                DayOfMonth=mmnorm(data$DayofMonth),
                DayOfWeek=mmnorm(data$DayOfWeek),
                CRSDepTime=mmnorm(data$CRSDepTime),
                CRSArrTime=mmnorm(data$CRSArrTime) ,
                UniqueCarrier=mmnorm(as.numeric(factor(data$UniqueCarrier))),
                Origin=mmnorm(as.numeric(factor(data$Origin))),
                Dest=mmnorm(as.numeric(factor(data$Dest_Type))),
                #DepDelay=mmnorm(data$DepDelay),
                ArrDelay_cat=as.character(data$ArrDelay_catnew)
)

idx1<-seq(1:5000)
data_new<-data_new[idx1,]

idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))


training<-data_new[idx,]
test<-data_new[-idx,]
for (j in 1:40){
  counter<- 0
  total<-0
  for (i in 1:50) {
    newpredict<-knn(training[,-9],test[,-9],training[,9],k <- j)
    newresults<-cbind(test,as.character(newpredict) )
    wrong<-newresults[,9]!=newresults[,10]
    rate<-sum(wrong)/length(wrong)
    
    #print(rate)
    rates<-rbind(rate,rate)
    total<-total+rate
    counter<-counter+1
  }
  #rates
  print(j)
  avg=total/counter
  print(avg)
}
##########################################













