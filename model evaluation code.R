library(openair)
library(tidyr)


# original data with all missing observations 
Original_Data<- read.csv("/PM10/PM10 original 2018.csv",header = TRUE)

Original_Data<- read.csv("/MVTS   EXP2/original data/PM10 original 2018.csv",header = TRUE)

station=colnames(Original_Data)
S=length(Original_Data)
Cluster_Avg<-read.csv("/PM10/model 1.csv",header = TRUE)
Cluster_Avg=Cluster_Avg[,-1]

Original_Data$Edinburgh.St.Leonards 

Cluster_Avg_AND_Enviroment_type<-read.csv("/PM10/model 2.csv",header = TRUE)
Cluster_Avg_AND_Enviroment_type=Cluster_Avg_AND_Enviroment_type[,-1]
Cluster_Avg_AND_Region<-read.csv("/PM10/model 3.csv",header = TRUE)
Cluster_Avg_AND_Region=Cluster_Avg_AND_Region[,-1]
First_1NN<-read.csv("/PM10/model 4.csv",header = TRUE)
First_1NN=First_1NN[,-1]
Avg_2NN<-read.csv("/PM10/model 5.csv",header = TRUE)
Avg_2NN=Avg_2NN[,-1]

Median<-read.csv("/PM10/model 6.csv",header = TRUE)
Median=Median[,-1]


Date <- read.csv("/MVTS   EXP2/Date 2018 hourly.csv",header = TRUE)
Station_list<- read.csv("/MVTS   EXP2/Stations List 167.csv",header = TRUE)

############################################################################################
############################################################################################
# Compare imputation methods/models   based ostatstic 
############################################################################################
############################################################################################

set.seed(10)

obs.1<-cbind(Date, Original_Data)
obs <- data.frame(obs.1$date,stack(obs.1[2:ncol(obs.1)]))
names(obs)=c("date","obs", "site")

mode.1.1 <- cbind(Date,Cluster_Avg)
mode.1 <- data.frame(mode.1.1$date,stack(mode.1.1[2:ncol(mode.1.1)]))
names(mode.1)=c("date","mod", "site")

mode.1.2 <- cbind(Date,Cluster_Avg_AND_Enviroment_type)
mode.2 <- data.frame(mode.1.2$date,stack(mode.1.2[2:ncol(mode.1.2)]))
names(mode.2)=c("date","mod", "site")

mode.1.3 <- cbind(Date,Cluster_Avg_AND_Region)
mode.3 <- data.frame(mode.1.3$date,stack(mode.1.3[2:ncol(mode.1.3)]))
names(mode.3)=c("date","mod", "site")


mode.1.4 <- cbind(Date,First_1NN)
mode.4 <- data.frame(mode.1.4$date,stack(mode.1.4[2:ncol(mode.1.4)]))
names(mode.4)=c("date","mod", "site")


mode.1.5 <- cbind(Date,Avg_2NN)
mode.5 <- data.frame(mode.1.5$date,stack(mode.1.5[2:ncol(mode.1.5)]))
names(mode.5)=c("date","mod", "site")


mode.1.6 <- cbind(Date,Median)
mode.6 <- data.frame(mode.1.6$date,stack(mode.1.6[2:ncol(mode.1.6)]))
names(mode.6)=c("date","mod", "site")


CA <- data.frame(obs, mod = mode.1$mod, model = "model 1 (CA)")
CA_ENV <- data.frame(obs, mod = mode.2$mod, model = "model 2 (CA_ENV)")
CA_REG <- data.frame(obs, mod = mode.3$mod, model = "model 3 (CA_REG)")
First_NN <- data.frame(obs, mod = mode.4$mod, model = "model 4 (1NN)")
Avg_2NN <- data.frame(obs, mod = mode.6$mod, model = "model 5 (2NN)")
Median <- data.frame(obs, mod = mode.8$mod ,model = "model 6 (Median)")


modData.00<- rbind(CA, CA_ENV, CA_REG, First_NN, Avg_2NN,Median)

idex<-which(is.na(CA$obs), arr.ind=TRUE)
CA.2 <- CA[-idex,]
CA_ENV.2 <- CA_ENV[-idex,]
CA_REG.2 <- CA_REG[-idex,]
First_NN.2 <- First_NN[-idex,]
Avg_2NN.2 <- Avg_2NN[-idex,]
Median.00 <- Median[-idex,]

modData <- rbind(CA.2, CA_ENV.2, CA_REG.2, First_NN.2, Avg_2NN.2,Median.00)
nrow(modData)


# model statstic based on models performance
Model_Perf <- modStats(modData, obs = "obs", mod = "mod", group = "model", type = c("model"))
Model_Perf
# model statstic based on models performance with seasons
Model_Perf <- modStats(modData, obs = "obs", mod = "mod", group = "model", type = c("model","season"))
Model_Perf
# model statstic based on models performance with each site
Model_Perf.2 <- modStats(modData, obs = "obs", mod = "mod", type = c("model","site"))
Model_Perf.2
# model statstic based on models performance based on station type
joined_df <- merge(modData, Station_list, by.x = "site", by.y = "Site.Name", all.x = TRUE, all.y = FALSE)
Model_Perf.1 <- modStats(joined_df, obs = "obs", mod = "mod", type =c("model", "Environment.Type") )
Model_Perf.1


############################################################################################
############################################################################################
# Taylor’s Diagram
############################################################################################
############################################################################################
library(openair)
require(tidyverse)

set.seed(10)
# cahneg the formate of the date to DATE
date <- as.Date(Date$date, format="%d/%m/%Y %H:%M")   

obs.1<-cbind(date, Original_Data)
obs <- data.frame(obs.1$date,stack(obs.1[2:ncol(obs.1)]))
names(obs)=c("date","obs", "site")

mode.1.1 <- cbind(date,Cluster_Avg)
mode.1 <- data.frame(mode.1.1$date,stack(mode.1.1[2:ncol(mode.1.1)]))
names(mode.1)=c("date","mod", "site")

mode.1.2 <- cbind(date,Cluster_Avg_AND_Enviroment_type)
mode.2 <- data.frame(mode.1.2$date,stack(mode.1.2[2:ncol(mode.1.2)]))
names(mode.2)=c("date","mod", "site")

mode.1.3 <- cbind(date,Cluster_Avg_AND_Region)
mode.3 <- data.frame(mode.1.3$date,stack(mode.1.3[2:ncol(mode.1.3)]))
names(mode.3)=c("date","mod", "site")


mode.1.4 <- cbind(date,First_1NN)
mode.4 <- data.frame(mode.1.4$date,stack(mode.1.4[2:ncol(mode.1.4)]))
names(mode.4)=c("date","mod", "site")

mode.1.5 <- cbind(date,Avg_2NN)
mode.5 <- data.frame(mode.1.5$date,stack(mode.1.5[2:ncol(mode.1.5)]))
names(mode.5)=c("date","mod", "site")


mode.1.6 <- cbind(date,Median)
mode.6 <- data.frame(mode.1.6$date,stack(mode.1.6[2:ncol(mode.1.6)]))
names(mode.6)=c("date","mod", "site")



CA <- data.frame(obs, mod = mode.1$mod, model = "model 1 (CA)")
CA_ENV <- data.frame(obs, mod = mode.2$mod, model = "model 2 (CA_ENV)")
CA_REG <- data.frame(obs, mod = mode.3$mod, model = "model 3 (CA_REG)")
First_NN <- data.frame(obs, mod = mode.4$mod, model = "model 4 (1NN)")
Avg_2NN <- data.frame(obs, mod = mode.6$mod, model = "model 5 (2NN)")
Median <- data.frame(obs, mod = mode.8$mod ,model = "model 6 (Median)")


modData.00<- rbind(CA, CA_ENV, CA_REG, First_NN, Avg_2NN,Median)

idex<-which(is.na(CA$obs), arr.ind=TRUE)
CA.2 <- CA[-idex,]
CA_ENV.2 <- CA_ENV[-idex,]
CA_REG.2 <- CA_REG[-idex,]
First_NN.2 <- First_NN[-idex,]
Avg_2NN.2 <- Avg_2NN[-idex,]
Median.00 <- Median[-idex,]

modData <- rbind(CA.2, CA_ENV.2, CA_REG.2, First_NN.2, Avg_2NN.2,Median.00)


TaylorDiagram(modData, obs = "obs", mod = "mod", group = "model",strip=TRUE, main="Taylor’s Diagram showing PM10 models performance ",pcex=30,fontsize = 16 )



############################################################################################
############################################################################################
# Compare monthly average for ENV types
############################################################################################
############################################################################################
Cluster_List_All<-read.csv("/PM10/Extracted pm10 cluster.csv")
Cluster_List_All=Cluster_List_All[,-1] # remove the index
#-----------------------------------------------------------------------------------#
# EXtract information for stations we need for each pollutant#
#-----------------------------------------------------------------------------------#
Cluster_List <- Cluster_List_All[Cluster_List_All$Station %in% station,]

series_and_type=matrix(NA,nrow(Original_Data),6)  # to store cluster avg with same env type

Station_Env_Type="Background Rural"
list<-which(Cluster_List$Environment.Type==Station_Env_Type )
list
length(list)
series_and_type[,1]=rowMeans(Original_Data[,list], na.rm = TRUE)


Station_Env_Type="Background Suburban"
list<-which(Cluster_List$Environment.Type==Station_Env_Type )
list
length(list)
series_and_type[,2]=rowMeans(Original_Data[,list], na.rm = TRUE)


Station_Env_Type="Background Urban"
list<-which(Cluster_List$Environment.Type==Station_Env_Type )
list
length(list)
series_and_type[,3]=rowMeans(Original_Data[,list], na.rm = TRUE)


Station_Env_Type="Industrial Suburban"
list<-which(Cluster_List$Environment.Type==Station_Env_Type )
list
length(list)
series_and_type[,4]=rowMeans(Original_Data[,list], na.rm = TRUE)


Station_Env_Type="Industrial Urban"
list<-which(Cluster_List$Environment.Type==Station_Env_Type )
list
length(list)
series_and_type[,5]=rowMeans(Original_Data[,list], na.rm = TRUE)


Station_Env_Type="Traffic Urban"
list<-which(Cluster_List$Environment.Type==Station_Env_Type )
list
length(list)
series_and_type[,6]=rowMeans(Original_Data[,list], na.rm = TRUE)

colnames(series_and_type)<- c("Background.Rural" ,   "Background.Suburban", "Background.Urban"  ,  "Industrial.Suburban" ,"Industrial.Urban"  ,"Traffic.Urban")
S.modDatax<-data.frame(Date, series_and_type)

library(openair)

timePlot(S.modDatax, pollutant = c("Background.Rural" ,   "Background.Suburban", "Background.Urban"  ,  "Industrial.Suburban" ,"Industrial.Urban"  ,"Traffic.Urban"),percentile=c(50, 95),cols = "brewer1",avg.time = "month",lwd = 2,lty = 1, cex.lab = 3,ylab = "PM10 (ug/m3)", group = TRUE , main ="The monthly averge concentrations of PM10 at each environment type for year 2018", fontsize=16)
timePlot(S.modDatax, pollutant = c("Background.Rural"), percentile=c(50, 95), cols = "brewer1",avg.time = "month",lwd = 2,lty = 1, cex.lab = 3,ylab = "PM10 (ug/m3)", group = TRUE , main ="The monthly averge concentrations of PM10 at each environment type for year 2018", fontsize=16)
Plot.data <- timeVariation(S.modDatax, ci=TRUE, pollutant = c("Background.Rural" ,   "Background.Suburban", "Background.Urban"  ,  "Industrial.Suburban" ,"Industrial.Urban"  ,"Traffic.Urban"), ylab = "PM10 (ug/m3)", main = "........)" ) 

############################################################################################
############################################################################################
#  Conditional quantiles to compre models
############################################################################################
############################################################################################
library(openair)
# limited the Q q to some data 
modData$obs <- replace(modData$obs, which(modData$obs < 0), NA)
modData$mod <- replace(modData$mod, which(modData$mod < 0), NA)
modData$obs <- replace(modData$obs, which(modData$obs > 110), NA)
modData$mod <- replace(modData$mod, which(modData$mod > 110), NA)


# all models
conditionalQuantile(modData, obs = "obs", mod = "mod",xbin=40,type = "model",main="Conditional quantile plots comparing imputed and observed PM10 models",xlim = c(0,280),ylim = c(0,280), fontsize=20)

# based on station types
Median.00$obs <- replace(Median.00$obs, which(Median.00$obs < 0), NA)
Median.00$mod <- replace(Median.00$mod, which(Median.00$mod < 0), NA)
Median.00$obs <- replace(Median.00$obs, which(Median.00$obs > 110), NA)
Median.00$mod <- replace(Median.00$mod, which(Median.00$mod > 110), NA)

joined_df_PM10 <- merge(Median.00, Station_list, by.x = "site", by.y = "Site.Name", all.x = TRUE, all.y = FALSE)

conditionalQuantile(joined_df_PM10, obs = "obs", xbin=60,mod = "mod",type = "Environment.Type",main="Conditional quantile plots comparing imputed and observed PM10 (Median.ENV imputation)", fontsize=16,xlim = c(0,180),ylim = c(0,180))

# based on site 
conditionalQuantile(Median.00, obs = "obs", xbin=40,mod = "mod", type = "site",main="Conditional quantile plots comparing imputed and observed PM10 (Median imputation) ", fontsize=16)#,xlim = c(-5,300),ylim = c(-5,300))


