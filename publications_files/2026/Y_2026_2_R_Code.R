#### R Codes
## The R code for  
## Fayaz M. 
## Comparison of three functional regression methods on air pollution 
## throughout the first two COVID-19 lockdown phases across 31 Iranian 
## provinces. JAPH. 2026;11(2):217-230.


### Written by: Mohammad Fayaz , PhD
### E-mail: Mohammad.Fayaz.89@gmail.com



###### R Codes for FOF Functions
#install.packages("pak")


### Calling Libraries
library(refund)
library(mgcv)
library(data.table)
library(fdapace)
library(pak)
library(fields)
library(tidyverse)
library(tidyfun)
library(viridis)

#pak::pkg_install("refundr")

### Reading Data sets
DS <- fread("...\\Appendix_E_DataSet_Modified.csv")

#### Select Data 
DS[,list(min(GDate),max(GDate)),by=list(Waves, COVID_Status)]
DS[,list(min(GDate),max(GDate)),by=Index]

#DS <- DS[order(Waves,COVID_Status, GAS, Province, GDate, )]



########### Wave 1
# CO1
CO1_W1_NOCOVID_2019 <- subset(DS, Index =="CO1_W1_NOCOVID_2019")
CO1_W1_COVID_2020   <- subset(DS, Index =="CO1_W1_COVID_2020")
# CO2
CO2_W1_NOCOVID_2019 <- subset(DS, Index =="CO2_W1_NOCOVID_2019")
CO2_W1_COVID_2020   <- subset(DS, Index =="CO2_W1_COVID_2020")
# NO2
NO2_W1_NOCOVID_2019 <- subset(DS, Index =="NO2_W1_NOCOVID_2019")
NO2_W1_COVID_2020   <- subset(DS, Index =="NO2_W1_COVID_2020")
# O3
O3_W1_NOCOVID_2019 <- subset(DS, Index =="O3_W1_NOCOVID_2019")
O3_W1_COVID_2020   <- subset(DS, Index =="O3_W1_COVID_2020")
# SO2
SO2_W1_NOCOVID_2019 <- subset(DS, Index =="SO2_W1_NOCOVID_2019")
SO2_W1_COVID_2020   <- subset(DS, Index =="SO2_W1_COVID_2020")
# AEI
AEI_W1_NOCOVID_2019 <- subset(DS, Index =="AEI_W1_NOCOVID_2019")
AEI_W1_COVID_2020   <- subset(DS, Index =="AEI_W1_COVID_2020")
# HCHO
HCHO_W1_NOCOVID_2019 <- subset(DS, Index =="HCHO_W1_NOCOVID_2019")
HCHO_W1_COVID_2020   <- subset(DS, Index =="HCHO_W1_COVID_2020")
# Pressure
Pressure_W1_NOCOVID_2019 <- subset(DS, Index =="Pressure_W1_NOCOVID_2019")
Pressure_W1_COVID_2020   <- subset(DS, Index =="Pressure_W1_COVID_2020")
# TPrecipitation
TPrecipitation_W1_NOCOVID_2019 <- subset(DS, Index =="TPrecipitation_W1_NOCOVID_2019")
TPrecipitation_W1_COVID_2020   <- subset(DS, Index =="TPrecipitation_W1_COVID_2020")
# MTemp
MTemp_W1_NOCOVID_2019 <- subset(DS, Index =="MTemp_W1_NOCOVID_2019")
MTemp_W1_COVID_2020   <- subset(DS, Index =="MTemp_W1_COVID_2020")
# UWind
UWind_W1_NOCOVID_2019 <- subset(DS, Index =="UWind_W1_NOCOVID_2019")
UWind_W1_COVID_2020   <- subset(DS, Index =="UWind_W1_COVID_2020")

########### Wave 2
# CO1
CO1_W2_NOCOVID_2020 <- subset(DS, Index =="CO1_W2_NOCOVID_2020")
CO1_W2_COVID_2021  <- subset(DS, Index =="CO1_W2_COVID_2021")
# CO2
CO2_W2_NOCOVID_2020 <- subset(DS, Index =="CO2_W2_NOCOVID_2020")
CO2_W2_COVID_2021  <- subset(DS, Index =="CO2_W2_COVID_2021")
# NO2
NO2_W2_NOCOVID_2020 <- subset(DS, Index =="NO2_W2_NOCOVID_2020")
NO2_W2_COVID_2021  <- subset(DS, Index =="NO2_W2_COVID_2021")
# O3
O3_W2_NOCOVID_2020 <- subset(DS, Index =="O3_W2_NOCOVID_2020")
O3_W2_COVID_2021  <- subset(DS, Index =="O3_W2_COVID_2021")
# SO2
SO2_W2_NOCOVID_2020 <- subset(DS, Index =="SO2_W2_NOCOVID_2020")
SO2_W2_COVID_2021  <- subset(DS, Index =="SO2_W2_COVID_2021")
# AEI
AEI_W2_NOCOVID_2020 <- subset(DS, Index =="AEI_W2_NOCOVID_2020")
AEI_W2_COVID_2021  <- subset(DS, Index =="AEI_W2_COVID_2021")
# HCHO
HCHO_W2_NOCOVID_2020 <- subset(DS, Index =="HCHO_W2_NOCOVID_2020")
HCHO_W2_COVID_2021  <- subset(DS, Index =="HCHO_W2_COVID_2021")
# Pressure
Pressure_W2_NOCOVID_2020 <- subset(DS, Index =="Pressure_W2_NOCOVID_2020")
Pressure_W2_COVID_2021  <- subset(DS, Index =="Pressure_W2_COVID_2021")
# TPrecipitation
TPrecipitation_W2_NOCOVID_2020 <- subset(DS, Index =="TPrecipitation_W2_NOCOVID_2020")
TPrecipitation_W2_COVID_2021  <- subset(DS, Index =="TPrecipitation_W2_COVID_2021")
# MTemp
MTemp_W2_NOCOVID_2020 <- subset(DS, Index =="MTemp_W2_NOCOVID_2020")
MTemp_W2_COVID_2021  <- subset(DS, Index =="MTemp_W2_COVID_2021")
# UWind
UWind_W2_NOCOVID_2020 <- subset(DS, Index =="UWind_W2_NOCOVID_2020")
UWind_W2_COVID_2021  <- subset(DS, Index =="UWind_W2_COVID_2021")


########### Data Preparation 
############################ CO1
############## Wave 1 
####### NO COVID
CO1_W1_NOCOVID_2019_DS <- data.table(argvals = CO1_W1_NOCOVID_2019$Days,
                                     subj    = CO1_W1_NOCOVID_2019$Province,
                                     y       = CO1_W1_NOCOVID_2019$Real_Value *1000 )
CO1_W1_NOCOVID_2019_DS_AVG <- CO1_W1_NOCOVID_2019_DS[,mean(y, na.rm=T),by=argvals]
#CO1_W1_NOCOVID_2019_DS <- CO1_W1_NOCOVID_2019_DS[complete.cases(CO1_W1_NOCOVID_2019_DS),]
CO1_W1_NOCOVID_2019_DS_Ly <-  split(CO1_W1_NOCOVID_2019_DS$y     , CO1_W1_NOCOVID_2019_DS$subj)
CO1_W1_NOCOVID_2019_DS_Lt <-  split(CO1_W1_NOCOVID_2019_DS$argvals, CO1_W1_NOCOVID_2019_DS$subj)
CO1_W1_NOCOVID_2019_DS_DesignPlot <- CreateDesignPlot(CO1_W1_NOCOVID_2019_DS_Lt,  sort(unique(unlist(CO1_W1_NOCOVID_2019_DS_Lt))))
CO1_W1_NOCOVID_2019_DS_FCPA <- FPCA(CO1_W1_NOCOVID_2019_DS_Ly, 
                                    CO1_W1_NOCOVID_2019_DS_Lt, 
                                     list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
CreatePathPlot(CO1_W1_NOCOVID_2019_DS_FCPA)
CO1_W1_NOCOVID_2019_DS_FCPA_RC <- CO1_W1_NOCOVID_2019_DS_FCPA$xiEst %*%  t(CO1_W1_NOCOVID_2019_DS_FCPA$phi)
matplot(t(CO1_W1_NOCOVID_2019_DS_FCPA_RC + CO1_W1_NOCOVID_2019_DS_AVG$V1),type="l")
plot(x=1:37, y  = CO1_W1_NOCOVID_2019_DS_FCPA_RC[10,] + CO1_W1_NOCOVID_2019_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = CO1_W1_NOCOVID_2019_DS_Ly[[10]],type="l",col="red")

CO1_W1_NOCOVID_2019_DS_FCPA_RC_MEAN <- CO1_W1_NOCOVID_2019_DS_FCPA_RC+ CO1_W1_NOCOVID_2019_DS_AVG$V1


####### COVID
CO1_W1_COVID_2020_DS <- data.table(argvals = CO1_W1_COVID_2020$Days,
                                   subj    = CO1_W1_COVID_2020$Province,
                                   y       = CO1_W1_COVID_2020$Real_Value *1000)
CO1_W1_COVID_2020_DS_AVG <- CO1_W1_COVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#CO1_W1_COVID_2020_DS <- CO1_W1_COVID_2020_DS[complete.cases(CO1_W1_COVID_2020_DS),]
CO1_W1_COVID_2020_DS_Ly <-  split(CO1_W1_COVID_2020_DS$y     , CO1_W1_COVID_2020_DS$subj)
CO1_W1_COVID_2020_DS_Lt <-  split(CO1_W1_COVID_2020_DS$argvals, CO1_W1_COVID_2020_DS$subj)
CO1_W1_COVID_2020_DS_FCPA <- FPCA(CO1_W1_COVID_2020_DS_Ly, 
                                  CO1_W1_COVID_2020_DS_Lt, 
                                  list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
CO1_W1_COVID_2020_DS_FCPA_RC <- CO1_W1_COVID_2020_DS_FCPA$xiEst %*%  t(CO1_W1_COVID_2020_DS_FCPA$phi)
plot(x=1:37, y  = CO1_W1_COVID_2020_DS_FCPA_RC[8,] + CO1_W1_COVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = CO1_W1_COVID_2020_DS_Ly[[8]],type="l",col="red")
CO1_W1_COVID_2020_DS_FCPA_RC_MEAN <- CO1_W1_COVID_2020_DS_FCPA_RC  + CO1_W1_COVID_2020_DS_AVG$V1
matplot(t(CO1_W1_COVID_2020_DS_FCPA_RC + CO1_W1_COVID_2020_DS_AVG$V1),type="l")



############## Wave 2 
####### NO COVID
CO1_W2_NOCOVID_2020_DS <- data.table(argvals = CO1_W2_NOCOVID_2020$Days,
                                     subj    = CO1_W2_NOCOVID_2020$Province,
                                     y       = CO1_W2_NOCOVID_2020$Real_Value *1000)
CO1_W2_NOCOVID_2020_DS_AVG <- CO1_W2_NOCOVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#CO1_W2_NOCOVID_2020_DS <- CO1_W2_NOCOVID_2020_DS[complete.cases(CO1_W2_NOCOVID_2020_DS),]
CO1_W2_NOCOVID_2020_DS_Ly <-  split(CO1_W2_NOCOVID_2020_DS$y     , CO1_W2_NOCOVID_2020_DS$subj)
CO1_W2_NOCOVID_2020_DS_Lt <-  split(CO1_W2_NOCOVID_2020_DS$argvals, CO1_W2_NOCOVID_2020_DS$subj)
CO1_W2_NOCOVID_2020_DS_FCPA <- FPCA(CO1_W2_NOCOVID_2020_DS_Ly, 
                                    CO1_W2_NOCOVID_2020_DS_Lt, 
                                    list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
CO1_W2_NOCOVID_2020_DS_FCPA_RC <- CO1_W2_NOCOVID_2020_DS_FCPA$xiEst %*%  t(CO1_W2_NOCOVID_2020_DS_FCPA$phi)
plot(x=1:12, y  = CO1_W2_NOCOVID_2020_DS_FCPA_RC[3,] + CO1_W2_NOCOVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = CO1_W2_NOCOVID_2020_DS_Ly[[3]],type="l",col="red")
CO1_W2_NOCOVID_2020_DS_FCPA_RC_MEAN <- CO1_W2_NOCOVID_2020_DS_FCPA_RC + CO1_W2_NOCOVID_2020_DS_AVG$V1


####### COVID
CO1_W2_COVID_2021_DS <- data.table(argvals = CO1_W2_COVID_2021$Days,
                                   subj    = CO1_W2_COVID_2021$Province,
                                   y       = CO1_W2_COVID_2021$Real_Value *1000)
CO1_W2_COVID_2021_DS_AVG <- CO1_W2_COVID_2021_DS[,mean(y, na.rm=T),by=argvals]
#CO1_W2_COVID_2021_DS <- CO1_W2_COVID_2021_DS[complete.cases(CO1_W2_COVID_2021_DS),]
CO1_W2_COVID_2021_DS_Ly <-  split(CO1_W2_COVID_2021_DS$y     , CO1_W2_COVID_2021_DS$subj)
CO1_W2_COVID_2021_DS_Lt <-  split(CO1_W2_COVID_2021_DS$argvals, CO1_W2_COVID_2021_DS$subj)
CO1_W2_COVID_2021_DS_FCPA <- FPCA(CO1_W2_COVID_2021_DS_Ly, 
                                  CO1_W2_COVID_2021_DS_Lt, 
                                  list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
CO1_W2_COVID_2021_DS_FCPA_RC <- CO1_W2_COVID_2021_DS_FCPA$xiEst %*%  t(CO1_W2_COVID_2021_DS_FCPA$phi)
plot(x=1:12, y  = CO1_W2_COVID_2021_DS_FCPA_RC[8,] + CO1_W2_COVID_2021_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = CO1_W2_COVID_2021_DS_Ly[[8]],type="l",col="red")
CO1_W2_COVID_2021_DS_FCPA_RC_MEAN <- CO1_W2_COVID_2021_DS_FCPA_RC + CO1_W2_COVID_2021_DS_AVG$V1




############################ CO2
############## Wave 1 
####### NO COVID
CO2_W1_NOCOVID_2019_DS <- data.table(argvals = CO2_W1_NOCOVID_2019$Days,
                                     subj    = CO2_W1_NOCOVID_2019$Province,
                                     y       = CO2_W1_NOCOVID_2019$Real_Value /10 )
CO2_W1_NOCOVID_2019_DS_AVG <- CO2_W1_NOCOVID_2019_DS[,mean(y, na.rm=T),by=argvals]
#CO2_W1_NOCOVID_2019_DS <- CO2_W1_NOCOVID_2019_DS[complete.cases(CO2_W1_NOCOVID_2019_DS),]
CO2_W1_NOCOVID_2019_DS_Ly <-  split(CO2_W1_NOCOVID_2019_DS$y     , CO2_W1_NOCOVID_2019_DS$subj)
CO2_W1_NOCOVID_2019_DS_Lt <-  split(CO2_W1_NOCOVID_2019_DS$argvals, CO2_W1_NOCOVID_2019_DS$subj)
CO2_W1_NOCOVID_2019_DS_FCPA <- FPCA(CO2_W1_NOCOVID_2019_DS_Ly, 
                                    CO2_W1_NOCOVID_2019_DS_Lt, 
                                    list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
CO2_W1_NOCOVID_2019_DS_FCPA_RC <- CO2_W1_NOCOVID_2019_DS_FCPA$xiEst %*%  t(CO2_W1_NOCOVID_2019_DS_FCPA$phi)
plot(x=1:37, y  = CO2_W1_NOCOVID_2019_DS_FCPA_RC[3,] + CO2_W1_NOCOVID_2019_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = CO2_W1_NOCOVID_2019_DS_Ly[[3]],type="l",col="red")
CO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN <- CO2_W1_NOCOVID_2019_DS_FCPA_RC + CO2_W1_NOCOVID_2019_DS_AVG$V1



####### COVID
CO2_W1_COVID_2020_DS <- data.table(argvals = CO2_W1_COVID_2020$Days,
                                   subj    = CO2_W1_COVID_2020$Province,
                                   y       = CO2_W1_COVID_2020$Real_Value /10)
CO2_W1_COVID_2020_DS_AVG <- CO2_W1_COVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#CO2_W1_COVID_2020_DS <- CO2_W1_COVID_2020_DS[complete.cases(CO2_W1_COVID_2020_DS),]
CO2_W1_COVID_2020_DS_Ly <-  split(CO2_W1_COVID_2020_DS$y     , CO2_W1_COVID_2020_DS$subj)
CO2_W1_COVID_2020_DS_Lt <-  split(CO2_W1_COVID_2020_DS$argvals, CO2_W1_COVID_2020_DS$subj)
CO2_W1_COVID_2020_DS_FCPA <- FPCA(CO2_W1_COVID_2020_DS_Ly, 
                                  CO2_W1_COVID_2020_DS_Lt, 
                                  list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
CO2_W1_COVID_2020_DS_FCPA_RC <- CO2_W1_COVID_2020_DS_FCPA$xiEst %*%  t(CO2_W1_COVID_2020_DS_FCPA$phi)
plot(x=1:37, y  = CO2_W1_COVID_2020_DS_FCPA_RC[8,] + CO2_W1_COVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = CO2_W1_COVID_2020_DS_Ly[[8]],type="l",col="red")
CO2_W1_COVID_2020_DS_FCPA_RC_MEAN <- CO2_W1_COVID_2020_DS_FCPA_RC + CO2_W1_COVID_2020_DS_AVG$V1



############## Wave 2 
####### NO COVID
CO2_W2_NOCOVID_2020_DS <- data.table(argvals = CO2_W2_NOCOVID_2020$Days,
                                     subj    = CO2_W2_NOCOVID_2020$Province,
                                     y       = CO2_W2_NOCOVID_2020$Real_Value /10 )
CO2_W2_NOCOVID_2020_DS_AVG <- CO2_W2_NOCOVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#CO2_W2_NOCOVID_2020_DS <- CO2_W2_NOCOVID_2020_DS[complete.cases(CO2_W2_NOCOVID_2020_DS),]
CO2_W2_NOCOVID_2020_DS_Ly <-  split(CO2_W2_NOCOVID_2020_DS$y     , CO2_W2_NOCOVID_2020_DS$subj)
CO2_W2_NOCOVID_2020_DS_Lt <-  split(CO2_W2_NOCOVID_2020_DS$argvals, CO2_W2_NOCOVID_2020_DS$subj)
CO2_W2_NOCOVID_2020_DS_FCPA <- FPCA(CO2_W2_NOCOVID_2020_DS_Ly, 
                                    CO2_W2_NOCOVID_2020_DS_Lt, 
                                    list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
CO2_W2_NOCOVID_2020_DS_FCPA_RC <- CO2_W2_NOCOVID_2020_DS_FCPA$xiEst %*%  t(CO2_W2_NOCOVID_2020_DS_FCPA$phi)
plot(x=1:12, y  = CO2_W2_NOCOVID_2020_DS_FCPA_RC[3,] + CO2_W2_NOCOVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = CO2_W2_NOCOVID_2020_DS_Ly[[3]],type="l",col="red")
CO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN <- CO2_W2_NOCOVID_2020_DS_FCPA_RC  + CO2_W2_NOCOVID_2020_DS_AVG$V1



####### COVID
CO2_W2_COVID_2021_DS <- data.table(argvals = CO2_W2_COVID_2021$Days,
                                   subj    = CO2_W2_COVID_2021$Province,
                                   y       = CO2_W2_COVID_2021$Real_Value /10)
CO2_W2_COVID_2021_DS_AVG <- CO2_W2_COVID_2021_DS[,mean(y, na.rm=T),by=argvals]
#CO2_W2_COVID_2021_DS <- CO2_W2_COVID_2021_DS[complete.cases(CO2_W2_COVID_2021_DS),]
CO2_W2_COVID_2021_DS_Ly <-  split(CO2_W2_COVID_2021_DS$y     , CO2_W2_COVID_2021_DS$subj)
CO2_W2_COVID_2021_DS_Lt <-  split(CO2_W2_COVID_2021_DS$argvals, CO2_W2_COVID_2021_DS$subj)
CO2_W2_COVID_2021_DS_FCPA <- FPCA(CO2_W2_COVID_2021_DS_Ly, 
                                  CO2_W2_COVID_2021_DS_Lt, 
                                  list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
CO2_W2_COVID_2021_DS_FCPA_RC <- CO2_W2_COVID_2021_DS_FCPA$xiEst %*%  t(CO2_W2_COVID_2021_DS_FCPA$phi)
plot(x=1:12, y  = CO2_W2_COVID_2021_DS_FCPA_RC[8,] + CO2_W2_COVID_2021_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = CO2_W2_COVID_2021_DS_Ly[[8]],type="l",col="red")
CO2_W2_COVID_2021_DS_FCPA_RC_MEAN <-  CO2_W2_COVID_2021_DS_FCPA_RC  + CO2_W2_COVID_2021_DS_AVG$V1






############################ NO2
############## Wave 1 
####### NO COVID
NO2_W1_NOCOVID_2019_DS <- data.table(argvals = NO2_W1_NOCOVID_2019$Days,
                                     subj    = NO2_W1_NOCOVID_2019$Province,
                                     y       = NO2_W1_NOCOVID_2019$Real_Value *  1000000 )
NO2_W1_NOCOVID_2019_DS_AVG <- NO2_W1_NOCOVID_2019_DS[,mean(y, na.rm=T),by=argvals]
#NO2_W1_NOCOVID_2019_DS <- NO2_W1_NOCOVID_2019_DS[complete.cases(NO2_W1_NOCOVID_2019_DS),]
NO2_W1_NOCOVID_2019_DS_Ly <-  split(NO2_W1_NOCOVID_2019_DS$y     , NO2_W1_NOCOVID_2019_DS$subj)
NO2_W1_NOCOVID_2019_DS_Lt <-  split(NO2_W1_NOCOVID_2019_DS$argvals, NO2_W1_NOCOVID_2019_DS$subj)
NO2_W1_NOCOVID_2019_DS_FCPA <- FPCA(NO2_W1_NOCOVID_2019_DS_Ly, 
                                    NO2_W1_NOCOVID_2019_DS_Lt, 
                                    list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
NO2_W1_NOCOVID_2019_DS_FCPA_RC <- NO2_W1_NOCOVID_2019_DS_FCPA$xiEst %*%  t(NO2_W1_NOCOVID_2019_DS_FCPA$phi)
plot(x=1:37, y  = NO2_W1_NOCOVID_2019_DS_FCPA_RC[3,] + NO2_W1_NOCOVID_2019_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = NO2_W1_NOCOVID_2019_DS_Ly[[3]],type="l",col="red")
NO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN <-  NO2_W1_NOCOVID_2019_DS_FCPA_RC  + NO2_W1_NOCOVID_2019_DS_AVG$V1



####### COVID -- ! Only Sparse with number of bin is 37!
NO2_W1_COVID_2020_DS <- data.table(argvals = NO2_W1_COVID_2020$Days,
                                   subj    = NO2_W1_COVID_2020$Province,
                                   y       = NO2_W1_COVID_2020$Real_Value *  1000000)
NO2_W1_COVID_2020_DS_AVG <- NO2_W1_COVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#NO2_W1_COVID_2020_DS <- NO2_W1_COVID_2020_DS[complete.cases(NO2_W1_COVID_2020_DS),]
NO2_W1_COVID_2020_DS_Ly <-  split(NO2_W1_COVID_2020_DS$y     , NO2_W1_COVID_2020_DS$subj)
NO2_W1_COVID_2020_DS_Lt <-  split(NO2_W1_COVID_2020_DS$argvals, NO2_W1_COVID_2020_DS$subj)
NO2_W1_COVID_2020_DS_DesignPlot <- CreateDesignPlot(NO2_W1_COVID_2020_DS_Lt,  sort(unique(unlist(NO2_W1_COVID_2020_DS_Lt))))
NO2_W1_COVID_2020_DS_FCPA <- FPCA(NO2_W1_COVID_2020_DS_Ly, 
                                  NO2_W1_COVID_2020_DS_Lt, 
                                  list(dataType='Sparse', error=FALSE, kernel='rect', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
NO2_W1_COVID_2020_DS_FCPA_RC <- NO2_W1_COVID_2020_DS_FCPA$xiEst %*%  t(NO2_W1_COVID_2020_DS_FCPA$phi)
plot(x=1:37, y  = NO2_W1_COVID_2020_DS_FCPA_RC[10,] + NO2_W1_COVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = NO2_W1_COVID_2020_DS_Ly[[10]],type="l",col="red")
NO2_W1_COVID_2020_DS_FCPA_RC_MEAN <- NO2_W1_COVID_2020_DS_FCPA_RC   + NO2_W1_COVID_2020_DS_AVG$V1



############## Wave 2 
####### NO COVID
NO2_W2_NOCOVID_2020_DS <- data.table(argvals = NO2_W2_NOCOVID_2020$Days,
                                     subj    = NO2_W2_NOCOVID_2020$Province,
                                     y       = NO2_W2_NOCOVID_2020$Real_Value *  1000000)
NO2_W2_NOCOVID_2020_DS_AVG <- NO2_W2_NOCOVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#NO2_W2_NOCOVID_2020_DS <- NO2_W2_NOCOVID_2020_DS[complete.cases(NO2_W2_NOCOVID_2020_DS),]
NO2_W2_NOCOVID_2020_DS_Ly <-  split(NO2_W2_NOCOVID_2020_DS$y     , NO2_W2_NOCOVID_2020_DS$subj)
NO2_W2_NOCOVID_2020_DS_Lt <-  split(NO2_W2_NOCOVID_2020_DS$argvals, NO2_W2_NOCOVID_2020_DS$subj)
NO2_W2_NOCOVID_2020_DS_FCPA <- FPCA(NO2_W2_NOCOVID_2020_DS_Ly, 
                                    NO2_W2_NOCOVID_2020_DS_Lt, 
                                    list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
NO2_W2_NOCOVID_2020_DS_FCPA_RC <- NO2_W2_NOCOVID_2020_DS_FCPA$xiEst %*%  t(NO2_W2_NOCOVID_2020_DS_FCPA$phi)
plot(x=1:12, y  = NO2_W2_NOCOVID_2020_DS_FCPA_RC[3,] + NO2_W2_NOCOVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = NO2_W2_NOCOVID_2020_DS_Ly[[3]],type="l",col="red")
NO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN <- NO2_W2_NOCOVID_2020_DS_FCPA_RC  + NO2_W2_NOCOVID_2020_DS_AVG$V1



####### COVID
NO2_W2_COVID_2021_DS <- data.table(argvals = NO2_W2_COVID_2021$Days,
                                   subj    = NO2_W2_COVID_2021$Province,
                                   y       = NO2_W2_COVID_2021$Real_Value *  1000000)
NO2_W2_COVID_2021_DS_AVG <- NO2_W2_COVID_2021_DS[,mean(y, na.rm=T),by=argvals]
#NO2_W2_COVID_2021_DS <- NO2_W2_COVID_2021_DS[complete.cases(NO2_W2_COVID_2021_DS),]
NO2_W2_COVID_2021_DS_Ly <-  split(NO2_W2_COVID_2021_DS$y     , NO2_W2_COVID_2021_DS$subj)
NO2_W2_COVID_2021_DS_Lt <-  split(NO2_W2_COVID_2021_DS$argvals, NO2_W2_COVID_2021_DS$subj)
NO2_W2_COVID_2021_DS_FCPA <- FPCA(NO2_W2_COVID_2021_DS_Ly, 
                                  NO2_W2_COVID_2021_DS_Lt, 
                                  list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
NO2_W2_COVID_2021_DS_FCPA_RC <- NO2_W2_COVID_2021_DS_FCPA$xiEst %*%  t(NO2_W2_COVID_2021_DS_FCPA$phi)
plot(x=1:12, y  = NO2_W2_COVID_2021_DS_FCPA_RC[8,] + NO2_W2_COVID_2021_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = NO2_W2_COVID_2021_DS_Ly[[8]],type="l",col="red")
NO2_W2_COVID_2021_DS_FCPA_RC_MEAN <-NO2_W2_COVID_2021_DS_FCPA_RC  + NO2_W2_COVID_2021_DS_AVG$V1





############################ O3
############## Wave 1 
####### NO COVID
O3_W1_NOCOVID_2019_DS <- data.table(argvals = O3_W1_NOCOVID_2019$Days,
                                    subj    = O3_W1_NOCOVID_2019$Province,
                                    y       = O3_W1_NOCOVID_2019$Real_Value *100)
O3_W1_NOCOVID_2019_DS_AVG <- O3_W1_NOCOVID_2019_DS[,mean(y, na.rm=T),by=argvals]
#O3_W1_NOCOVID_2019_DS <- O3_W1_NOCOVID_2019_DS[complete.cases(O3_W1_NOCOVID_2019_DS),]
O3_W1_NOCOVID_2019_DS_Ly <-  split(O3_W1_NOCOVID_2019_DS$y     , O3_W1_NOCOVID_2019_DS$subj)
O3_W1_NOCOVID_2019_DS_Lt <-  split(O3_W1_NOCOVID_2019_DS$argvals, O3_W1_NOCOVID_2019_DS$subj)
O3_W1_NOCOVID_2019_DS_FCPA <- FPCA(O3_W1_NOCOVID_2019_DS_Ly, 
                                   O3_W1_NOCOVID_2019_DS_Lt, 
                                   list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
O3_W1_NOCOVID_2019_DS_FCPA_RC <- O3_W1_NOCOVID_2019_DS_FCPA$xiEst %*%  t(O3_W1_NOCOVID_2019_DS_FCPA$phi)
plot(x=1:37, y  = O3_W1_NOCOVID_2019_DS_FCPA_RC[3,] + O3_W1_NOCOVID_2019_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = O3_W1_NOCOVID_2019_DS_Ly[[3]],type="l",col="red")
O3_W1_NOCOVID_2019_DS_FCPA_RC_MEAN <-O3_W1_NOCOVID_2019_DS_FCPA_RC + O3_W1_NOCOVID_2019_DS_AVG$V1 



####### COVID
O3_W1_COVID_2020_DS <- data.table(argvals = O3_W1_COVID_2020$Days,
                                  subj    = O3_W1_COVID_2020$Province,
                                  y       = O3_W1_COVID_2020$Real_Value  *100)
O3_W1_COVID_2020_DS_AVG <- O3_W1_COVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#O3_W1_COVID_2020_DS <- O3_W1_COVID_2020_DS[complete.cases(O3_W1_COVID_2020_DS),]
O3_W1_COVID_2020_DS_Ly <-  split(O3_W1_COVID_2020_DS$y     , O3_W1_COVID_2020_DS$subj)
O3_W1_COVID_2020_DS_Lt <-  split(O3_W1_COVID_2020_DS$argvals, O3_W1_COVID_2020_DS$subj)
O3_W1_COVID_2020_DS_FCPA <- FPCA(O3_W1_COVID_2020_DS_Ly, 
                                 O3_W1_COVID_2020_DS_Lt, 
                                 list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
O3_W1_COVID_2020_DS_FCPA_RC <- O3_W1_COVID_2020_DS_FCPA$xiEst %*%  t(O3_W1_COVID_2020_DS_FCPA$phi)
plot(x=1:37, y  = O3_W1_COVID_2020_DS_FCPA_RC[8,] + O3_W1_COVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = O3_W1_COVID_2020_DS_Ly[[8]],type="l",col="red")
O3_W1_COVID_2020_DS_FCPA_RC_MEAN <-O3_W1_COVID_2020_DS_FCPA_RC  + O3_W1_COVID_2020_DS_AVG$V1



############## Wave 2 
####### NO COVID
O3_W2_NOCOVID_2020_DS <- data.table(argvals = O3_W2_NOCOVID_2020$Days,
                                    subj    = O3_W2_NOCOVID_2020$Province,
                                    y       = O3_W2_NOCOVID_2020$Real_Value  *100)
O3_W2_NOCOVID_2020_DS_AVG <- O3_W2_NOCOVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#O3_W2_NOCOVID_2020_DS <- O3_W2_NOCOVID_2020_DS[complete.cases(O3_W2_NOCOVID_2020_DS),]
O3_W2_NOCOVID_2020_DS_Ly <-  split(O3_W2_NOCOVID_2020_DS$y     , O3_W2_NOCOVID_2020_DS$subj)
O3_W2_NOCOVID_2020_DS_Lt <-  split(O3_W2_NOCOVID_2020_DS$argvals, O3_W2_NOCOVID_2020_DS$subj)
O3_W2_NOCOVID_2020_DS_FCPA <- FPCA(O3_W2_NOCOVID_2020_DS_Ly, 
                                   O3_W2_NOCOVID_2020_DS_Lt, 
                                   list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
O3_W2_NOCOVID_2020_DS_FCPA_RC <- O3_W2_NOCOVID_2020_DS_FCPA$xiEst %*%  t(O3_W2_NOCOVID_2020_DS_FCPA$phi)
plot(x=1:12, y  = O3_W2_NOCOVID_2020_DS_FCPA_RC[3,] + O3_W2_NOCOVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = O3_W2_NOCOVID_2020_DS_Ly[[3]],type="l",col="red")
O3_W2_NOCOVID_2020_DS_FCPA_RC_MEAN <- O3_W2_NOCOVID_2020_DS_FCPA_RC  + O3_W2_NOCOVID_2020_DS_AVG$V1



####### COVID
O3_W2_COVID_2021_DS <- data.table(argvals = O3_W2_COVID_2021$Days,
                                  subj    = O3_W2_COVID_2021$Province,
                                  y       = O3_W2_COVID_2021$Real_Value  *100)
O3_W2_COVID_2021_DS_AVG <- O3_W2_COVID_2021_DS[,mean(y, na.rm=T),by=argvals]
#O3_W2_COVID_2021_DS <- O3_W2_COVID_2021_DS[complete.cases(O3_W2_COVID_2021_DS),]
O3_W2_COVID_2021_DS_Ly <-  split(O3_W2_COVID_2021_DS$y     , O3_W2_COVID_2021_DS$subj)
O3_W2_COVID_2021_DS_Lt <-  split(O3_W2_COVID_2021_DS$argvals, O3_W2_COVID_2021_DS$subj)
O3_W2_COVID_2021_DS_FCPA <- FPCA(O3_W2_COVID_2021_DS_Ly, 
                                 O3_W2_COVID_2021_DS_Lt, 
                                 list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
O3_W2_COVID_2021_DS_FCPA_RC <- O3_W2_COVID_2021_DS_FCPA$xiEst %*%  t(O3_W2_COVID_2021_DS_FCPA$phi)
plot(x=1:12, y  = O3_W2_COVID_2021_DS_FCPA_RC[8,] + O3_W2_COVID_2021_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = O3_W2_COVID_2021_DS_Ly[[8]],type="l",col="red")
O3_W2_COVID_2021_DS_FCPA_RC_MEAN <-  O3_W2_COVID_2021_DS_FCPA_RC  + O3_W2_COVID_2021_DS_AVG$V1







############################ SO2
############## Wave 1 
####### NO COVID
SO2_W1_NOCOVID_2019_DS <- data.table(argvals = SO2_W1_NOCOVID_2019$Days,
                                     subj    = SO2_W1_NOCOVID_2019$Province,
                                     y       = SO2_W1_NOCOVID_2019$Real_Value * 100000 )
SO2_W1_NOCOVID_2019_DS_AVG <- SO2_W1_NOCOVID_2019_DS[,mean(y, na.rm=T),by=argvals]
#SO2_W1_NOCOVID_2019_DS <- SO2_W1_NOCOVID_2019_DS[complete.cases(SO2_W1_NOCOVID_2019_DS),]
SO2_W1_NOCOVID_2019_DS_Ly <-  split(SO2_W1_NOCOVID_2019_DS$y     , SO2_W1_NOCOVID_2019_DS$subj)
SO2_W1_NOCOVID_2019_DS_Lt <-  split(SO2_W1_NOCOVID_2019_DS$argvals, SO2_W1_NOCOVID_2019_DS$subj)
SO2_W1_NOCOVID_2019_DS_FCPA <- FPCA(SO2_W1_NOCOVID_2019_DS_Ly, 
                                    SO2_W1_NOCOVID_2019_DS_Lt, 
                                    list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
SO2_W1_NOCOVID_2019_DS_FCPA_RC <- SO2_W1_NOCOVID_2019_DS_FCPA$xiEst %*%  t(SO2_W1_NOCOVID_2019_DS_FCPA$phi)
plot(x=1:37, y  = SO2_W1_NOCOVID_2019_DS_FCPA_RC[3,] + SO2_W1_NOCOVID_2019_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = SO2_W1_NOCOVID_2019_DS_Ly[[3]],type="l",col="red")
SO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN <-  SO2_W1_NOCOVID_2019_DS_FCPA_RC  + SO2_W1_NOCOVID_2019_DS_AVG$V1


####### COVID
SO2_W1_COVID_2020_DS <- data.table(argvals = SO2_W1_COVID_2020$Days,
                                   subj    = SO2_W1_COVID_2020$Province,
                                   y       = SO2_W1_COVID_2020$Real_Value * 100000 )
SO2_W1_COVID_2020_DS_AVG <- SO2_W1_COVID_2020_DS[,mean(y, na.rm=T),by=argvals]
SO2_W1_COVID_2020_DS_AVG[1,2] <- median( as.numeric(unlist(SO2_W1_COVID_2020_DS_AVG[,2])),na.rm=TRUE)
#SO2_W1_COVID_2020_DS <- SO2_W1_COVID_2020_DS[complete.cases(SO2_W1_COVID_2020_DS),]
SO2_W1_COVID_2020_DS_Ly <-  split(SO2_W1_COVID_2020_DS$y     , SO2_W1_COVID_2020_DS$subj)
SO2_W1_COVID_2020_DS_Lt <-  split(SO2_W1_COVID_2020_DS$argvals, SO2_W1_COVID_2020_DS$subj)
SO2_W1_COVID_2020_DS_FCPA <- FPCA(SO2_W1_COVID_2020_DS_Ly, 
                                  SO2_W1_COVID_2020_DS_Lt, 
                                  list(dataType='Sparse', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
SO2_W1_COVID_2020_DS_FCPA_RC <- SO2_W1_COVID_2020_DS_FCPA$xiEst %*%  t(SO2_W1_COVID_2020_DS_FCPA$phi)
plot(x=1:37, y  = SO2_W1_COVID_2020_DS_FCPA_RC[10,] + SO2_W1_COVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = SO2_W1_COVID_2020_DS_Ly[[10]],type="l",col="red")
SO2_W1_COVID_2020_DS_FCPA_RC_MEAN <-  SO2_W1_COVID_2020_DS_FCPA_RC + SO2_W1_COVID_2020_DS_AVG$V1


############## Wave 2 
####### NO COVID
SO2_W2_NOCOVID_2020_DS <- data.table(argvals = SO2_W2_NOCOVID_2020$Days,
                                     subj    = SO2_W2_NOCOVID_2020$Province,
                                     y       = SO2_W2_NOCOVID_2020$Real_Value * 100000 )
SO2_W2_NOCOVID_2020_DS_AVG <- SO2_W2_NOCOVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#SO2_W2_NOCOVID_2020_DS <- SO2_W2_NOCOVID_2020_DS[complete.cases(SO2_W2_NOCOVID_2020_DS),]
SO2_W2_NOCOVID_2020_DS_Ly <-  split(SO2_W2_NOCOVID_2020_DS$y     , SO2_W2_NOCOVID_2020_DS$subj)
SO2_W2_NOCOVID_2020_DS_Lt <-  split(SO2_W2_NOCOVID_2020_DS$argvals, SO2_W2_NOCOVID_2020_DS$subj)
SO2_W2_NOCOVID_2020_DS_FCPA <- FPCA(SO2_W2_NOCOVID_2020_DS_Ly, 
                                    SO2_W2_NOCOVID_2020_DS_Lt, 
                                    list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
SO2_W2_NOCOVID_2020_DS_FCPA_RC <- SO2_W2_NOCOVID_2020_DS_FCPA$xiEst %*%  t(SO2_W2_NOCOVID_2020_DS_FCPA$phi)
plot(x=1:12, y  = SO2_W2_NOCOVID_2020_DS_FCPA_RC[3,] + SO2_W2_NOCOVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = SO2_W2_NOCOVID_2020_DS_Ly[[3]],type="l",col="red")
SO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN <-  SO2_W2_NOCOVID_2020_DS_FCPA_RC + SO2_W2_NOCOVID_2020_DS_AVG$V1


####### COVID
SO2_W2_COVID_2021_DS <- data.table(argvals = SO2_W2_COVID_2021$Days,
                                   subj    = SO2_W2_COVID_2021$Province,
                                   y       = SO2_W2_COVID_2021$Real_Value * 100000)
SO2_W2_COVID_2021_DS_AVG <- SO2_W2_COVID_2021_DS[,mean(y, na.rm=T),by=argvals]
#SO2_W2_COVID_2021_DS <- SO2_W2_COVID_2021_DS[complete.cases(SO2_W2_COVID_2021_DS),]
SO2_W2_COVID_2021_DS_Ly <-  split(SO2_W2_COVID_2021_DS$y     , SO2_W2_COVID_2021_DS$subj)
SO2_W2_COVID_2021_DS_Lt <-  split(SO2_W2_COVID_2021_DS$argvals, SO2_W2_COVID_2021_DS$subj)
SO2_W2_COVID_2021_DS_FCPA <- FPCA(SO2_W2_COVID_2021_DS_Ly, 
                                  SO2_W2_COVID_2021_DS_Lt, 
                                  list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
SO2_W2_COVID_2021_DS_FCPA_RC <- SO2_W2_COVID_2021_DS_FCPA$xiEst %*%  t(SO2_W2_COVID_2021_DS_FCPA$phi)
plot(x=1:12, y  = SO2_W2_COVID_2021_DS_FCPA_RC[8,] + SO2_W2_COVID_2021_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = SO2_W2_COVID_2021_DS_Ly[[8]],type="l",col="red")
SO2_W2_COVID_2021_DS_FCPA_RC_MEAN <-  SO2_W2_COVID_2021_DS_FCPA_RC  + SO2_W2_COVID_2021_DS_AVG$V1







############################ AEI
############## Wave 1 
####### NO COVID
AEI_W1_NOCOVID_2019_DS <- data.table(argvals = AEI_W1_NOCOVID_2019$Days,
                                     subj    = AEI_W1_NOCOVID_2019$Province,
                                     y       = AEI_W1_NOCOVID_2019$Real_Value *10)
AEI_W1_NOCOVID_2019_DS_AVG <- AEI_W1_NOCOVID_2019_DS[,mean(y, na.rm=T),by=argvals]
#AEI_W1_NOCOVID_2019_DS <- AEI_W1_NOCOVID_2019_DS[complete.cases(AEI_W1_NOCOVID_2019_DS),]
AEI_W1_NOCOVID_2019_DS_Ly <-  split(AEI_W1_NOCOVID_2019_DS$y     , AEI_W1_NOCOVID_2019_DS$subj)
AEI_W1_NOCOVID_2019_DS_Lt <-  split(AEI_W1_NOCOVID_2019_DS$argvals, AEI_W1_NOCOVID_2019_DS$subj)
AEI_W1_NOCOVID_2019_DS_FCPA <- FPCA(AEI_W1_NOCOVID_2019_DS_Ly, 
                                    AEI_W1_NOCOVID_2019_DS_Lt, 
                                    list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
AEI_W1_NOCOVID_2019_DS_FCPA_RC <- AEI_W1_NOCOVID_2019_DS_FCPA$xiEst %*%  t(AEI_W1_NOCOVID_2019_DS_FCPA$phi)
plot(x=1:37, y  = AEI_W1_NOCOVID_2019_DS_FCPA_RC[3,] + AEI_W1_NOCOVID_2019_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = AEI_W1_NOCOVID_2019_DS_Ly[[3]],type="l",col="red")
AEI_W1_NOCOVID_2019_DS_FCPA_RC_MEAN <-  AEI_W1_NOCOVID_2019_DS_FCPA_RC + AEI_W1_NOCOVID_2019_DS_AVG$V1



####### COVID
AEI_W1_COVID_2020_DS <- data.table(argvals = AEI_W1_COVID_2020$Days,
                                   subj    = AEI_W1_COVID_2020$Province,
                                   y       = AEI_W1_COVID_2020$Real_Value *10)
AEI_W1_COVID_2020_DS_AVG <- AEI_W1_COVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#AEI_W1_COVID_2020_DS <- AEI_W1_COVID_2020_DS[complete.cases(AEI_W1_COVID_2020_DS),]
AEI_W1_COVID_2020_DS_Ly <-  split(AEI_W1_COVID_2020_DS$y     , AEI_W1_COVID_2020_DS$subj)
AEI_W1_COVID_2020_DS_Lt <-  split(AEI_W1_COVID_2020_DS$argvals, AEI_W1_COVID_2020_DS$subj)
AEI_W1_COVID_2020_DS_FCPA <- FPCA(AEI_W1_COVID_2020_DS_Ly, 
                                  AEI_W1_COVID_2020_DS_Lt, 
                                  list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
AEI_W1_COVID_2020_DS_FCPA_RC <- AEI_W1_COVID_2020_DS_FCPA$xiEst %*%  t(AEI_W1_COVID_2020_DS_FCPA$phi)
plot(x=1:37, y  = AEI_W1_COVID_2020_DS_FCPA_RC[8,] + AEI_W1_COVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = AEI_W1_COVID_2020_DS_Ly[[8]],type="l",col="red")
AEI_W1_COVID_2020_DS_FCPA_RC_MEAN <- AEI_W1_COVID_2020_DS_FCPA_RC  + AEI_W1_COVID_2020_DS_AVG$V1



############## Wave 2 
####### NO COVID
AEI_W2_NOCOVID_2020_DS <- data.table(argvals = AEI_W2_NOCOVID_2020$Days,
                                     subj    = AEI_W2_NOCOVID_2020$Province,
                                     y       = AEI_W2_NOCOVID_2020$Real_Value *10 )
AEI_W2_NOCOVID_2020_DS_AVG <- AEI_W2_NOCOVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#AEI_W2_NOCOVID_2020_DS <- AEI_W2_NOCOVID_2020_DS[complete.cases(AEI_W2_NOCOVID_2020_DS),]
AEI_W2_NOCOVID_2020_DS_Ly <-  split(AEI_W2_NOCOVID_2020_DS$y     , AEI_W2_NOCOVID_2020_DS$subj)
AEI_W2_NOCOVID_2020_DS_Lt <-  split(AEI_W2_NOCOVID_2020_DS$argvals, AEI_W2_NOCOVID_2020_DS$subj)
AEI_W2_NOCOVID_2020_DS_FCPA <- FPCA(AEI_W2_NOCOVID_2020_DS_Ly, 
                                    AEI_W2_NOCOVID_2020_DS_Lt, 
                                    list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
AEI_W2_NOCOVID_2020_DS_FCPA_RC <- AEI_W2_NOCOVID_2020_DS_FCPA$xiEst %*%  t(AEI_W2_NOCOVID_2020_DS_FCPA$phi)
plot(x=1:12, y  = AEI_W2_NOCOVID_2020_DS_FCPA_RC[3,] + AEI_W2_NOCOVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = AEI_W2_NOCOVID_2020_DS_Ly[[3]],type="l",col="red")
AEI_W2_NOCOVID_2020_DS_FCPA_RC_MEAN <- AEI_W2_NOCOVID_2020_DS_FCPA_RC  + AEI_W2_NOCOVID_2020_DS_AVG$V1



####### COVID
AEI_W2_COVID_2021_DS <- data.table(argvals = AEI_W2_COVID_2021$Days,
                                   subj    = AEI_W2_COVID_2021$Province,
                                   y       = AEI_W2_COVID_2021$Real_Value *10)
AEI_W2_COVID_2021_DS_AVG <- AEI_W2_COVID_2021_DS[,mean(y, na.rm=T),by=argvals]
#AEI_W2_COVID_2021_DS <- AEI_W2_COVID_2021_DS[complete.cases(AEI_W2_COVID_2021_DS),]
AEI_W2_COVID_2021_DS_Ly <-  split(AEI_W2_COVID_2021_DS$y     , AEI_W2_COVID_2021_DS$subj)
AEI_W2_COVID_2021_DS_Lt <-  split(AEI_W2_COVID_2021_DS$argvals, AEI_W2_COVID_2021_DS$subj)
AEI_W2_COVID_2021_DS_FCPA <- FPCA(AEI_W2_COVID_2021_DS_Ly, 
                                  AEI_W2_COVID_2021_DS_Lt, 
                                  list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
AEI_W2_COVID_2021_DS_FCPA_RC <- AEI_W2_COVID_2021_DS_FCPA$xiEst %*%  t(AEI_W2_COVID_2021_DS_FCPA$phi)
plot(x=1:12, y  = AEI_W2_COVID_2021_DS_FCPA_RC[8,] + AEI_W2_COVID_2021_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = AEI_W2_COVID_2021_DS_Ly[[8]],type="l",col="red")
AEI_W2_COVID_2021_DS_FCPA_RC_MEAN <- AEI_W2_COVID_2021_DS_FCPA_RC  + AEI_W2_COVID_2021_DS_AVG$V1






############################ HCHO
############## Wave 1 
####### NO COVID
HCHO_W1_NOCOVID_2019_DS <- data.table(argvals = HCHO_W1_NOCOVID_2019$Days,
                                      subj    = HCHO_W1_NOCOVID_2019$Province,
                                      y       = HCHO_W1_NOCOVID_2019$Real_Value * 1000000 )
HCHO_W1_NOCOVID_2019_DS_AVG <- HCHO_W1_NOCOVID_2019_DS[,mean(y, na.rm=T),by=argvals]
#HCHO_W1_NOCOVID_2019_DS <- HCHO_W1_NOCOVID_2019_DS[complete.cases(HCHO_W1_NOCOVID_2019_DS),]
HCHO_W1_NOCOVID_2019_DS_Ly <-  split(HCHO_W1_NOCOVID_2019_DS$y     , HCHO_W1_NOCOVID_2019_DS$subj)
HCHO_W1_NOCOVID_2019_DS_Lt <-  split(HCHO_W1_NOCOVID_2019_DS$argvals, HCHO_W1_NOCOVID_2019_DS$subj)
HCHO_W1_NOCOVID_2019_DS_FCPA <- FPCA(HCHO_W1_NOCOVID_2019_DS_Ly, 
                                     HCHO_W1_NOCOVID_2019_DS_Lt, 
                                     list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
HCHO_W1_NOCOVID_2019_DS_FCPA_RC <- HCHO_W1_NOCOVID_2019_DS_FCPA$xiEst %*%  t(HCHO_W1_NOCOVID_2019_DS_FCPA$phi)
plot(x=1:37, y  = HCHO_W1_NOCOVID_2019_DS_FCPA_RC[3,] + HCHO_W1_NOCOVID_2019_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = HCHO_W1_NOCOVID_2019_DS_Ly[[3]],type="l",col="red")
HCHO_W1_NOCOVID_2019_DS_FCPA_RC_MEAN <- HCHO_W1_NOCOVID_2019_DS_FCPA_RC  + HCHO_W1_NOCOVID_2019_DS_AVG$V1



####### COVID
HCHO_W1_COVID_2020_DS <- data.table(argvals = HCHO_W1_COVID_2020$Days,
                                    subj    = HCHO_W1_COVID_2020$Province,
                                    y       = HCHO_W1_COVID_2020$Real_Value * 1000000)
HCHO_W1_COVID_2020_DS_AVG <- HCHO_W1_COVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#HCHO_W1_COVID_2020_DS <- HCHO_W1_COVID_2020_DS[complete.cases(HCHO_W1_COVID_2020_DS),]
HCHO_W1_COVID_2020_DS_Ly <-  split(HCHO_W1_COVID_2020_DS$y     , HCHO_W1_COVID_2020_DS$subj)
HCHO_W1_COVID_2020_DS_Lt <-  split(HCHO_W1_COVID_2020_DS$argvals, HCHO_W1_COVID_2020_DS$subj)
HCHO_W1_COVID_2020_DS_FCPA <- FPCA(HCHO_W1_COVID_2020_DS_Ly, 
                                   HCHO_W1_COVID_2020_DS_Lt, 
                                   list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
HCHO_W1_COVID_2020_DS_FCPA_RC <- HCHO_W1_COVID_2020_DS_FCPA$xiEst %*%  t(HCHO_W1_COVID_2020_DS_FCPA$phi)
plot(x=1:37, y  = HCHO_W1_COVID_2020_DS_FCPA_RC[8,] + HCHO_W1_COVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = HCHO_W1_COVID_2020_DS_Ly[[8]],type="l",col="red")
HCHO_W1_COVID_2020_DS_FCPA_RC_MEAN <- HCHO_W1_COVID_2020_DS_FCPA_RC + HCHO_W1_COVID_2020_DS_AVG$V1


############## Wave 2 
####### NO COVID
HCHO_W2_NOCOVID_2020_DS <- data.table(argvals = HCHO_W2_NOCOVID_2020$Days,
                                      subj    = HCHO_W2_NOCOVID_2020$Province,
                                      y       = HCHO_W2_NOCOVID_2020$Real_Value * 1000000)
HCHO_W2_NOCOVID_2020_DS_AVG <- HCHO_W2_NOCOVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#HCHO_W2_NOCOVID_2020_DS <- HCHO_W2_NOCOVID_2020_DS[complete.cases(HCHO_W2_NOCOVID_2020_DS),]
HCHO_W2_NOCOVID_2020_DS_Ly <-  split(HCHO_W2_NOCOVID_2020_DS$y     , HCHO_W2_NOCOVID_2020_DS$subj)
HCHO_W2_NOCOVID_2020_DS_Lt <-  split(HCHO_W2_NOCOVID_2020_DS$argvals, HCHO_W2_NOCOVID_2020_DS$subj)
HCHO_W2_NOCOVID_2020_DS_FCPA <- FPCA(HCHO_W2_NOCOVID_2020_DS_Ly, 
                                     HCHO_W2_NOCOVID_2020_DS_Lt, 
                                     list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
HCHO_W2_NOCOVID_2020_DS_FCPA_RC <- HCHO_W2_NOCOVID_2020_DS_FCPA$xiEst %*%  t(HCHO_W2_NOCOVID_2020_DS_FCPA$phi)
plot(x=1:12, y  = HCHO_W2_NOCOVID_2020_DS_FCPA_RC[3,] + HCHO_W2_NOCOVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = HCHO_W2_NOCOVID_2020_DS_Ly[[3]],type="l",col="red")
HCHO_W2_NOCOVID_2020_DS_FCPA_RC_MEAN <- HCHO_W2_NOCOVID_2020_DS_FCPA_RC  + HCHO_W2_NOCOVID_2020_DS_AVG$V1


####### COVID
HCHO_W2_COVID_2021_DS <- data.table(argvals = HCHO_W2_COVID_2021$Days,
                                    subj    = HCHO_W2_COVID_2021$Province,
                                    y       = HCHO_W2_COVID_2021$Real_Value * 1000000)
HCHO_W2_COVID_2021_DS_AVG <- HCHO_W2_COVID_2021_DS[,mean(y, na.rm=T),by=argvals]
#HCHO_W2_COVID_2021_DS <- HCHO_W2_COVID_2021_DS[complete.cases(HCHO_W2_COVID_2021_DS),]
HCHO_W2_COVID_2021_DS_Ly <-  split(HCHO_W2_COVID_2021_DS$y     , HCHO_W2_COVID_2021_DS$subj)
HCHO_W2_COVID_2021_DS_Lt <-  split(HCHO_W2_COVID_2021_DS$argvals, HCHO_W2_COVID_2021_DS$subj)
HCHO_W2_COVID_2021_DS_FCPA <- FPCA(HCHO_W2_COVID_2021_DS_Ly, 
                                   HCHO_W2_COVID_2021_DS_Lt, 
                                   list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
HCHO_W2_COVID_2021_DS_FCPA_RC <- HCHO_W2_COVID_2021_DS_FCPA$xiEst %*%  t(HCHO_W2_COVID_2021_DS_FCPA$phi)
plot(x=1:12, y  = HCHO_W2_COVID_2021_DS_FCPA_RC[8,] + HCHO_W2_COVID_2021_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = HCHO_W2_COVID_2021_DS_Ly[[8]],type="l",col="red")
HCHO_W2_COVID_2021_DS_FCPA_RC_MEAN <- HCHO_W2_COVID_2021_DS_FCPA_RC  + HCHO_W2_COVID_2021_DS_AVG$V1





############################ Pressure
############## Wave 1 
####### NO COVID
Pressure_W1_NOCOVID_2019_DS <- data.table(argvals = Pressure_W1_NOCOVID_2019$Days,
                                          subj    = Pressure_W1_NOCOVID_2019$Province,
                                          y       = Pressure_W1_NOCOVID_2019$Real_Value /1000)
Pressure_W1_NOCOVID_2019_DS_AVG <- Pressure_W1_NOCOVID_2019_DS[,mean(y, na.rm=T),by=argvals]
#Pressure_W1_NOCOVID_2019_DS <- Pressure_W1_NOCOVID_2019_DS[complete.cases(Pressure_W1_NOCOVID_2019_DS),]
Pressure_W1_NOCOVID_2019_DS_Ly <-  split(Pressure_W1_NOCOVID_2019_DS$y     , Pressure_W1_NOCOVID_2019_DS$subj)
Pressure_W1_NOCOVID_2019_DS_Lt <-  split(Pressure_W1_NOCOVID_2019_DS$argvals, Pressure_W1_NOCOVID_2019_DS$subj)
Pressure_W1_NOCOVID_2019_DS_FCPA <- FPCA(Pressure_W1_NOCOVID_2019_DS_Ly, 
                                         Pressure_W1_NOCOVID_2019_DS_Lt, 
                                         list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.99))
Pressure_W1_NOCOVID_2019_DS_FCPA_RC <- Pressure_W1_NOCOVID_2019_DS_FCPA$xiEst %*%  t(Pressure_W1_NOCOVID_2019_DS_FCPA$phi)
plot(x=1:37, y  = Pressure_W1_NOCOVID_2019_DS_FCPA_RC[3,] + Pressure_W1_NOCOVID_2019_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = Pressure_W1_NOCOVID_2019_DS_Ly[[3]],type="l",col="red")
Pressure_W1_NOCOVID_2019_DS_FCPA_RC_MEAN <- Pressure_W1_NOCOVID_2019_DS_FCPA_RC  + Pressure_W1_NOCOVID_2019_DS_AVG$V1


####### COVID
Pressure_W1_COVID_2020_DS <- data.table(argvals = Pressure_W1_COVID_2020$Days,
                                        subj    = Pressure_W1_COVID_2020$Province,
                                        y       = Pressure_W1_COVID_2020$Real_Value /1000)
Pressure_W1_COVID_2020_DS_AVG <- Pressure_W1_COVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#Pressure_W1_COVID_2020_DS <- Pressure_W1_COVID_2020_DS[complete.cases(Pressure_W1_COVID_2020_DS),]
Pressure_W1_COVID_2020_DS_Ly <-  split(Pressure_W1_COVID_2020_DS$y     , Pressure_W1_COVID_2020_DS$subj)
Pressure_W1_COVID_2020_DS_Lt <-  split(Pressure_W1_COVID_2020_DS$argvals, Pressure_W1_COVID_2020_DS$subj)
Pressure_W1_COVID_2020_DS_FCPA <- FPCA(Pressure_W1_COVID_2020_DS_Ly, 
                                       Pressure_W1_COVID_2020_DS_Lt, 
                                       list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
Pressure_W1_COVID_2020_DS_FCPA_RC <- Pressure_W1_COVID_2020_DS_FCPA$xiEst %*%  t(Pressure_W1_COVID_2020_DS_FCPA$phi)
plot(x=1:37, y  = Pressure_W1_COVID_2020_DS_FCPA_RC[8,] + Pressure_W1_COVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = Pressure_W1_COVID_2020_DS_Ly[[8]],type="l",col="red")
Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN <- Pressure_W1_COVID_2020_DS_FCPA_RC + Pressure_W1_COVID_2020_DS_AVG$V1

############## Wave 2 
####### NO COVID
Pressure_W2_NOCOVID_2020_DS <- data.table(argvals = Pressure_W2_NOCOVID_2020$Days,
                                          subj    = Pressure_W2_NOCOVID_2020$Province,
                                          y       = Pressure_W2_NOCOVID_2020$Real_Value /1000)
Pressure_W2_NOCOVID_2020_DS_AVG <- Pressure_W2_NOCOVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#Pressure_W2_NOCOVID_2020_DS <- Pressure_W2_NOCOVID_2020_DS[complete.cases(Pressure_W2_NOCOVID_2020_DS),]
Pressure_W2_NOCOVID_2020_DS_Ly <-  split(Pressure_W2_NOCOVID_2020_DS$y     , Pressure_W2_NOCOVID_2020_DS$subj)
Pressure_W2_NOCOVID_2020_DS_Lt <-  split(Pressure_W2_NOCOVID_2020_DS$argvals, Pressure_W2_NOCOVID_2020_DS$subj)
Pressure_W2_NOCOVID_2020_DS_FCPA <- FPCA(Pressure_W2_NOCOVID_2020_DS_Ly, 
                                         Pressure_W2_NOCOVID_2020_DS_Lt, 
                                         list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
Pressure_W2_NOCOVID_2020_DS_FCPA_RC <- Pressure_W2_NOCOVID_2020_DS_FCPA$xiEst %*%  t(Pressure_W2_NOCOVID_2020_DS_FCPA$phi)
plot(x=1:12, y  = Pressure_W2_NOCOVID_2020_DS_FCPA_RC[3,] + Pressure_W2_NOCOVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = Pressure_W2_NOCOVID_2020_DS_Ly[[3]],type="l",col="red")
Pressure_W2_NOCOVID_2020_DS_FCPA_RC_MEAN <- Pressure_W2_NOCOVID_2020_DS_FCPA_RC  + Pressure_W2_NOCOVID_2020_DS_AVG$V1


####### COVID
Pressure_W2_COVID_2021_DS <- data.table(argvals = Pressure_W2_COVID_2021$Days,
                                        subj    = Pressure_W2_COVID_2021$Province,
                                        y       = Pressure_W2_COVID_2021$Real_Value /1000)
Pressure_W2_COVID_2021_DS_AVG <- Pressure_W2_COVID_2021_DS[,mean(y, na.rm=T),by=argvals]
#Pressure_W2_COVID_2021_DS <- Pressure_W2_COVID_2021_DS[complete.cases(Pressure_W2_COVID_2021_DS),]
Pressure_W2_COVID_2021_DS_Ly <-  split(Pressure_W2_COVID_2021_DS$y     , Pressure_W2_COVID_2021_DS$subj)
Pressure_W2_COVID_2021_DS_Lt <-  split(Pressure_W2_COVID_2021_DS$argvals, Pressure_W2_COVID_2021_DS$subj)
Pressure_W2_COVID_2021_DS_FCPA <- FPCA(Pressure_W2_COVID_2021_DS_Ly, 
                                       Pressure_W2_COVID_2021_DS_Lt, 
                                       list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
Pressure_W2_COVID_2021_DS_FCPA_RC <- Pressure_W2_COVID_2021_DS_FCPA$xiEst %*%  t(Pressure_W2_COVID_2021_DS_FCPA$phi)
plot(x=1:12, y  = Pressure_W2_COVID_2021_DS_FCPA_RC[8,] + Pressure_W2_COVID_2021_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = Pressure_W2_COVID_2021_DS_Ly[[8]],type="l",col="red")
Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN <- Pressure_W2_COVID_2021_DS_FCPA_RC  + Pressure_W2_COVID_2021_DS_AVG$V1




############################ TPrecipitation
############## Wave 1 
####### NO COVID
TPrecipitation_W1_NOCOVID_2019_DS <- data.table(argvals = TPrecipitation_W1_NOCOVID_2019$Days,
                                                subj    = TPrecipitation_W1_NOCOVID_2019$Province,
                                                y       = TPrecipitation_W1_NOCOVID_2019$Real_Value * 1000000)
TPrecipitation_W1_NOCOVID_2019_DS_AVG <- TPrecipitation_W1_NOCOVID_2019_DS[,mean(y, na.rm=T),by=argvals]
#TPrecipitation_W1_NOCOVID_2019_DS <- TPrecipitation_W1_NOCOVID_2019_DS[complete.cases(TPrecipitation_W1_NOCOVID_2019_DS),]
TPrecipitation_W1_NOCOVID_2019_DS_Ly <-  split(TPrecipitation_W1_NOCOVID_2019_DS$y     , TPrecipitation_W1_NOCOVID_2019_DS$subj)
TPrecipitation_W1_NOCOVID_2019_DS_Lt <-  split(TPrecipitation_W1_NOCOVID_2019_DS$argvals, TPrecipitation_W1_NOCOVID_2019_DS$subj)
TPrecipitation_W1_NOCOVID_2019_DS_FCPA <- FPCA(TPrecipitation_W1_NOCOVID_2019_DS_Ly, 
                                               TPrecipitation_W1_NOCOVID_2019_DS_Lt, 
                                               list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
TPrecipitation_W1_NOCOVID_2019_DS_FCPA_RC <- TPrecipitation_W1_NOCOVID_2019_DS_FCPA$xiEst %*%  t(TPrecipitation_W1_NOCOVID_2019_DS_FCPA$phi)
plot(x=1:37, y  = TPrecipitation_W1_NOCOVID_2019_DS_FCPA_RC[3,] + TPrecipitation_W1_NOCOVID_2019_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = TPrecipitation_W1_NOCOVID_2019_DS_Ly[[3]],type="l",col="red")
TPrecipitation_W1_NOCOVID_2019_DS_FCPA_RC_MEAN <- TPrecipitation_W1_NOCOVID_2019_DS_FCPA_RC  + TPrecipitation_W1_NOCOVID_2019_DS_AVG$V1 


####### COVID
TPrecipitation_W1_COVID_2020_DS <- data.table(argvals = TPrecipitation_W1_COVID_2020$Days,
                                              subj    = TPrecipitation_W1_COVID_2020$Province,
                                              y       = TPrecipitation_W1_COVID_2020$Real_Value * 1000000)
TPrecipitation_W1_COVID_2020_DS_AVG <- TPrecipitation_W1_COVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#TPrecipitation_W1_COVID_2020_DS <- TPrecipitation_W1_COVID_2020_DS[complete.cases(TPrecipitation_W1_COVID_2020_DS),]
TPrecipitation_W1_COVID_2020_DS_Ly <-  split(TPrecipitation_W1_COVID_2020_DS$y     , TPrecipitation_W1_COVID_2020_DS$subj)
TPrecipitation_W1_COVID_2020_DS_Lt <-  split(TPrecipitation_W1_COVID_2020_DS$argvals, TPrecipitation_W1_COVID_2020_DS$subj)
TPrecipitation_W1_COVID_2020_DS_FCPA <- FPCA(TPrecipitation_W1_COVID_2020_DS_Ly, 
                                             TPrecipitation_W1_COVID_2020_DS_Lt, 
                                             list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
TPrecipitation_W1_COVID_2020_DS_FCPA_RC <- TPrecipitation_W1_COVID_2020_DS_FCPA$xiEst %*%  t(TPrecipitation_W1_COVID_2020_DS_FCPA$phi)
plot(x=1:37, y  = TPrecipitation_W1_COVID_2020_DS_FCPA_RC[8,] + TPrecipitation_W1_COVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = TPrecipitation_W1_COVID_2020_DS_Ly[[8]],type="l",col="red")
TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN <- TPrecipitation_W1_COVID_2020_DS_FCPA_RC + TPrecipitation_W1_COVID_2020_DS_AVG$V1



############## Wave 2 
####### NO COVID
TPrecipitation_W2_NOCOVID_2020_DS <- data.table(argvals = TPrecipitation_W2_NOCOVID_2020$Days,
                                                subj    = TPrecipitation_W2_NOCOVID_2020$Province,
                                                y       = TPrecipitation_W2_NOCOVID_2020$Real_Value * 1000000)
TPrecipitation_W2_NOCOVID_2020_DS_AVG <- TPrecipitation_W2_NOCOVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#TPrecipitation_W2_NOCOVID_2020_DS <- TPrecipitation_W2_NOCOVID_2020_DS[complete.cases(TPrecipitation_W2_NOCOVID_2020_DS),]
TPrecipitation_W2_NOCOVID_2020_DS_Ly <-  split(TPrecipitation_W2_NOCOVID_2020_DS$y     , TPrecipitation_W2_NOCOVID_2020_DS$subj)
TPrecipitation_W2_NOCOVID_2020_DS_Lt <-  split(TPrecipitation_W2_NOCOVID_2020_DS$argvals, TPrecipitation_W2_NOCOVID_2020_DS$subj)
TPrecipitation_W2_NOCOVID_2020_DS_FCPA <- FPCA(TPrecipitation_W2_NOCOVID_2020_DS_Ly, 
                                               TPrecipitation_W2_NOCOVID_2020_DS_Lt, 
                                               list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
TPrecipitation_W2_NOCOVID_2020_DS_FCPA_RC <- TPrecipitation_W2_NOCOVID_2020_DS_FCPA$xiEst %*%  t(TPrecipitation_W2_NOCOVID_2020_DS_FCPA$phi)
plot(x=1:12, y  = TPrecipitation_W2_NOCOVID_2020_DS_FCPA_RC[3,] + TPrecipitation_W2_NOCOVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = TPrecipitation_W2_NOCOVID_2020_DS_Ly[[3]],type="l",col="red")
TPrecipitation_W2_NOCOVID_2020_DS_FCPA_RC_MEAN <- TPrecipitation_W2_NOCOVID_2020_DS_FCPA_RC  + TPrecipitation_W2_NOCOVID_2020_DS_AVG$V1 


####### COVID
TPrecipitation_W2_COVID_2021_DS <- data.table(argvals = TPrecipitation_W2_COVID_2021$Days,
                                              subj    = TPrecipitation_W2_COVID_2021$Province,
                                              y       = TPrecipitation_W2_COVID_2021$Real_Value * 1000000)
TPrecipitation_W2_COVID_2021_DS_AVG <- TPrecipitation_W2_COVID_2021_DS[,mean(y, na.rm=T),by=argvals]
#TPrecipitation_W2_COVID_2021_DS <- TPrecipitation_W2_COVID_2021_DS[complete.cases(TPrecipitation_W2_COVID_2021_DS),]
TPrecipitation_W2_COVID_2021_DS_Ly <-  split(TPrecipitation_W2_COVID_2021_DS$y     , TPrecipitation_W2_COVID_2021_DS$subj)
TPrecipitation_W2_COVID_2021_DS_Lt <-  split(TPrecipitation_W2_COVID_2021_DS$argvals, TPrecipitation_W2_COVID_2021_DS$subj)
TPrecipitation_W2_COVID_2021_DS_FCPA <- FPCA(TPrecipitation_W2_COVID_2021_DS_Ly, 
                                             TPrecipitation_W2_COVID_2021_DS_Lt, 
                                             list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
TPrecipitation_W2_COVID_2021_DS_FCPA_RC <- TPrecipitation_W2_COVID_2021_DS_FCPA$xiEst %*%  t(TPrecipitation_W2_COVID_2021_DS_FCPA$phi)
plot(x=1:12, y  = TPrecipitation_W2_COVID_2021_DS_FCPA_RC[8,] + TPrecipitation_W2_COVID_2021_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = TPrecipitation_W2_COVID_2021_DS_Ly[[8]],type="l",col="red")
TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN <-TPrecipitation_W2_COVID_2021_DS_FCPA_RC  + TPrecipitation_W2_COVID_2021_DS_AVG$V1







############################ MTemp
############## Wave 1 
####### NO COVID
MTemp_W1_NOCOVID_2019_DS <- data.table(argvals = MTemp_W1_NOCOVID_2019$Days,
                                       subj    = MTemp_W1_NOCOVID_2019$Province,
                                       y       = MTemp_W1_NOCOVID_2019$Real_Value /10 )
MTemp_W1_NOCOVID_2019_DS_AVG <- MTemp_W1_NOCOVID_2019_DS[,mean(y, na.rm=T),by=argvals]
#MTemp_W1_NOCOVID_2019_DS <- MTemp_W1_NOCOVID_2019_DS[complete.cases(MTemp_W1_NOCOVID_2019_DS),]
MTemp_W1_NOCOVID_2019_DS_Ly <-  split(MTemp_W1_NOCOVID_2019_DS$y     , MTemp_W1_NOCOVID_2019_DS$subj)
MTemp_W1_NOCOVID_2019_DS_Lt <-  split(MTemp_W1_NOCOVID_2019_DS$argvals, MTemp_W1_NOCOVID_2019_DS$subj)
MTemp_W1_NOCOVID_2019_DS_FCPA <- FPCA(MTemp_W1_NOCOVID_2019_DS_Ly, 
                                      MTemp_W1_NOCOVID_2019_DS_Lt, 
                                      list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
MTemp_W1_NOCOVID_2019_DS_FCPA_RC <- MTemp_W1_NOCOVID_2019_DS_FCPA$xiEst %*%  t(MTemp_W1_NOCOVID_2019_DS_FCPA$phi)
plot(x=1:37, y  = MTemp_W1_NOCOVID_2019_DS_FCPA_RC[3,] + MTemp_W1_NOCOVID_2019_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = MTemp_W1_NOCOVID_2019_DS_Ly[[3]],type="l",col="red")
MTemp_W1_NOCOVID_2019_DS_FCPA_RC_MEAN <-MTemp_W1_NOCOVID_2019_DS_FCPA_RC + MTemp_W1_NOCOVID_2019_DS_AVG$V1


####### COVID
MTemp_W1_COVID_2020_DS <- data.table(argvals = MTemp_W1_COVID_2020$Days,
                                     subj    = MTemp_W1_COVID_2020$Province,
                                     y       = MTemp_W1_COVID_2020$Real_Value /10 )
MTemp_W1_COVID_2020_DS_AVG <- MTemp_W1_COVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#MTemp_W1_COVID_2020_DS <- MTemp_W1_COVID_2020_DS[complete.cases(MTemp_W1_COVID_2020_DS),]
MTemp_W1_COVID_2020_DS_Ly <-  split(MTemp_W1_COVID_2020_DS$y     , MTemp_W1_COVID_2020_DS$subj)
MTemp_W1_COVID_2020_DS_Lt <-  split(MTemp_W1_COVID_2020_DS$argvals, MTemp_W1_COVID_2020_DS$subj)
MTemp_W1_COVID_2020_DS_FCPA <- FPCA(MTemp_W1_COVID_2020_DS_Ly, 
                                    MTemp_W1_COVID_2020_DS_Lt, 
                                    list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
MTemp_W1_COVID_2020_DS_FCPA_RC <- MTemp_W1_COVID_2020_DS_FCPA$xiEst %*%  t(MTemp_W1_COVID_2020_DS_FCPA$phi)
plot(x=1:37, y  = MTemp_W1_COVID_2020_DS_FCPA_RC[8,] + MTemp_W1_COVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = MTemp_W1_COVID_2020_DS_Ly[[8]],type="l",col="red")
MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN <-MTemp_W1_COVID_2020_DS_FCPA_RC + MTemp_W1_COVID_2020_DS_AVG$V1



############## Wave 2 
####### NO COVID
MTemp_W2_NOCOVID_2020_DS <- data.table(argvals = MTemp_W2_NOCOVID_2020$Days,
                                       subj    = MTemp_W2_NOCOVID_2020$Province,
                                       y       = MTemp_W2_NOCOVID_2020$Real_Value /10 )
MTemp_W2_NOCOVID_2020_DS_AVG <- MTemp_W2_NOCOVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#MTemp_W2_NOCOVID_2020_DS <- MTemp_W2_NOCOVID_2020_DS[complete.cases(MTemp_W2_NOCOVID_2020_DS),]
MTemp_W2_NOCOVID_2020_DS_Ly <-  split(MTemp_W2_NOCOVID_2020_DS$y     , MTemp_W2_NOCOVID_2020_DS$subj)
MTemp_W2_NOCOVID_2020_DS_Lt <-  split(MTemp_W2_NOCOVID_2020_DS$argvals, MTemp_W2_NOCOVID_2020_DS$subj)
MTemp_W2_NOCOVID_2020_DS_FCPA <- FPCA(MTemp_W2_NOCOVID_2020_DS_Ly, 
                                      MTemp_W2_NOCOVID_2020_DS_Lt, 
                                      list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
MTemp_W2_NOCOVID_2020_DS_FCPA_RC <- MTemp_W2_NOCOVID_2020_DS_FCPA$xiEst %*%  t(MTemp_W2_NOCOVID_2020_DS_FCPA$phi)
plot(x=1:12, y  = MTemp_W2_NOCOVID_2020_DS_FCPA_RC[3,] + MTemp_W2_NOCOVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = MTemp_W2_NOCOVID_2020_DS_Ly[[3]],type="l",col="red")
MTemp_W2_NOCOVID_2020_DS_FCPA_RC_MEAN <-MTemp_W2_NOCOVID_2020_DS_FCPA_RC + MTemp_W2_NOCOVID_2020_DS_AVG$V1


####### COVID
MTemp_W2_COVID_2021_DS <- data.table(argvals = MTemp_W2_COVID_2021$Days,
                                     subj    = MTemp_W2_COVID_2021$Province,
                                     y       = MTemp_W2_COVID_2021$Real_Value /10 )
MTemp_W2_COVID_2021_DS_AVG <- MTemp_W2_COVID_2021_DS[,mean(y, na.rm=T),by=argvals]
#MTemp_W2_COVID_2021_DS <- MTemp_W2_COVID_2021_DS[complete.cases(MTemp_W2_COVID_2021_DS),]
MTemp_W2_COVID_2021_DS_Ly <-  split(MTemp_W2_COVID_2021_DS$y     , MTemp_W2_COVID_2021_DS$subj)
MTemp_W2_COVID_2021_DS_Lt <-  split(MTemp_W2_COVID_2021_DS$argvals, MTemp_W2_COVID_2021_DS$subj)
MTemp_W2_COVID_2021_DS_FCPA <- FPCA(MTemp_W2_COVID_2021_DS_Ly, 
                                    MTemp_W2_COVID_2021_DS_Lt, 
                                    list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
MTemp_W2_COVID_2021_DS_FCPA_RC <- MTemp_W2_COVID_2021_DS_FCPA$xiEst %*%  t(MTemp_W2_COVID_2021_DS_FCPA$phi)
plot(x=1:12, y  = MTemp_W2_COVID_2021_DS_FCPA_RC[8,] + MTemp_W2_COVID_2021_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = MTemp_W2_COVID_2021_DS_Ly[[8]],type="l",col="red")
MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN <-MTemp_W2_COVID_2021_DS_FCPA_RC  + MTemp_W2_COVID_2021_DS_AVG$V1






############################ UWind
############## Wave 1 
####### NO COVID
UWind_W1_NOCOVID_2019_DS <- data.table(argvals = UWind_W1_NOCOVID_2019$Days,
                                       subj    = UWind_W1_NOCOVID_2019$Province,
                                       y       = UWind_W1_NOCOVID_2019$Real_Value )
UWind_W1_NOCOVID_2019_DS_AVG <- UWind_W1_NOCOVID_2019_DS[,mean(y, na.rm=T),by=argvals]
#UWind_W1_NOCOVID_2019_DS <- UWind_W1_NOCOVID_2019_DS[complete.cases(UWind_W1_NOCOVID_2019_DS),]
UWind_W1_NOCOVID_2019_DS_Ly <-  split(UWind_W1_NOCOVID_2019_DS$y     , UWind_W1_NOCOVID_2019_DS$subj)
UWind_W1_NOCOVID_2019_DS_Lt <-  split(UWind_W1_NOCOVID_2019_DS$argvals, UWind_W1_NOCOVID_2019_DS$subj)
UWind_W1_NOCOVID_2019_DS_FCPA <- FPCA(UWind_W1_NOCOVID_2019_DS_Ly, 
                                      UWind_W1_NOCOVID_2019_DS_Lt, 
                                      list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
UWind_W1_NOCOVID_2019_DS_FCPA_RC <- UWind_W1_NOCOVID_2019_DS_FCPA$xiEst %*%  t(UWind_W1_NOCOVID_2019_DS_FCPA$phi)
plot(x=1:37, y  = UWind_W1_NOCOVID_2019_DS_FCPA_RC[3,] + UWind_W1_NOCOVID_2019_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = UWind_W1_NOCOVID_2019_DS_Ly[[3]],type="l",col="red")
UWind_W1_NOCOVID_2019_DS_FCPA_RC_MEAN <-UWind_W1_NOCOVID_2019_DS_FCPA_RC  + UWind_W1_NOCOVID_2019_DS_AVG$V1



####### COVID
UWind_W1_COVID_2020_DS <- data.table(argvals = UWind_W1_COVID_2020$Days,
                                     subj    = UWind_W1_COVID_2020$Province,
                                     y       = UWind_W1_COVID_2020$Real_Value )
UWind_W1_COVID_2020_DS_AVG <- UWind_W1_COVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#UWind_W1_COVID_2020_DS <- UWind_W1_COVID_2020_DS[complete.cases(UWind_W1_COVID_2020_DS),]
UWind_W1_COVID_2020_DS_Ly <-  split(UWind_W1_COVID_2020_DS$y     , UWind_W1_COVID_2020_DS$subj)
UWind_W1_COVID_2020_DS_Lt <-  split(UWind_W1_COVID_2020_DS$argvals, UWind_W1_COVID_2020_DS$subj)
UWind_W1_COVID_2020_DS_FCPA <- FPCA(UWind_W1_COVID_2020_DS_Ly, 
                                    UWind_W1_COVID_2020_DS_Lt, 
                                    list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 37 ,maxK = 30))
UWind_W1_COVID_2020_DS_FCPA_RC <- UWind_W1_COVID_2020_DS_FCPA$xiEst %*%  t(UWind_W1_COVID_2020_DS_FCPA$phi)
plot(x=1:37, y  = UWind_W1_COVID_2020_DS_FCPA_RC[8,] + UWind_W1_COVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:37, y  = UWind_W1_COVID_2020_DS_Ly[[8]],type="l",col="red")
UWind_W1_COVID_2020_DS_FCPA_RC_MEAN <-UWind_W1_COVID_2020_DS_FCPA_RC  + UWind_W1_COVID_2020_DS_AVG$V1



############## Wave 2 
####### NO COVID
UWind_W2_NOCOVID_2020_DS <- data.table(argvals = UWind_W2_NOCOVID_2020$Days,
                                       subj    = UWind_W2_NOCOVID_2020$Province,
                                       y       = UWind_W2_NOCOVID_2020$Real_Value )
UWind_W2_NOCOVID_2020_DS_AVG <- UWind_W2_NOCOVID_2020_DS[,mean(y, na.rm=T),by=argvals]
#UWind_W2_NOCOVID_2020_DS <- UWind_W2_NOCOVID_2020_DS[complete.cases(UWind_W2_NOCOVID_2020_DS),]
UWind_W2_NOCOVID_2020_DS_Ly <-  split(UWind_W2_NOCOVID_2020_DS$y     , UWind_W2_NOCOVID_2020_DS$subj)
UWind_W2_NOCOVID_2020_DS_Lt <-  split(UWind_W2_NOCOVID_2020_DS$argvals, UWind_W2_NOCOVID_2020_DS$subj)
UWind_W2_NOCOVID_2020_DS_FCPA <- FPCA(UWind_W2_NOCOVID_2020_DS_Ly, 
                                      UWind_W2_NOCOVID_2020_DS_Lt, 
                                      list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
UWind_W2_NOCOVID_2020_DS_FCPA_RC <- UWind_W2_NOCOVID_2020_DS_FCPA$xiEst %*%  t(UWind_W2_NOCOVID_2020_DS_FCPA$phi)
plot(x=1:12, y  = UWind_W2_NOCOVID_2020_DS_FCPA_RC[3,] + UWind_W2_NOCOVID_2020_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = UWind_W2_NOCOVID_2020_DS_Ly[[3]],type="l",col="red")
UWind_W2_NOCOVID_2020_DS_FCPA_RC_MEAN <-UWind_W2_NOCOVID_2020_DS_FCPA_RC  + UWind_W2_NOCOVID_2020_DS_AVG$V1



####### COVID
UWind_W2_COVID_2021_DS <- data.table(argvals = UWind_W2_COVID_2021$Days,
                                     subj    = UWind_W2_COVID_2021$Province,
                                     y       = UWind_W2_COVID_2021$Real_Value )
UWind_W2_COVID_2021_DS_AVG <- UWind_W2_COVID_2021_DS[,mean(y, na.rm=T),by=argvals]
#UWind_W2_COVID_2021_DS <- UWind_W2_COVID_2021_DS[complete.cases(UWind_W2_COVID_2021_DS),]
UWind_W2_COVID_2021_DS_Ly <-  split(UWind_W2_COVID_2021_DS$y     , UWind_W2_COVID_2021_DS$subj)
UWind_W2_COVID_2021_DS_Lt <-  split(UWind_W2_COVID_2021_DS$argvals, UWind_W2_COVID_2021_DS$subj)
UWind_W2_COVID_2021_DS_FCPA <- FPCA(UWind_W2_COVID_2021_DS_Ly, 
                                    UWind_W2_COVID_2021_DS_Lt, 
                                    list(dataType='Dense', error=FALSE, kernel='epan', verbose=TRUE,FVEthreshold=0.999, numBins = 12 ,maxK = 30))
UWind_W2_COVID_2021_DS_FCPA_RC <- UWind_W2_COVID_2021_DS_FCPA$xiEst %*%  t(UWind_W2_COVID_2021_DS_FCPA$phi)
plot(x=1:12, y  = UWind_W2_COVID_2021_DS_FCPA_RC[8,] + UWind_W2_COVID_2021_DS_AVG$V1 ,type="l")
lines(x=1:12, y  = UWind_W2_COVID_2021_DS_Ly[[8]],type="l",col="red")
UWind_W2_COVID_2021_DS_FCPA_RC_MEAN <-UWind_W2_COVID_2021_DS_FCPA_RC  + UWind_W2_COVID_2021_DS_AVG$V1


#############################
############################# Descriptive 
#############################
#############################

D_CO1_W1_NOCOVID <- cbind(t(as.numeric(summary(unlist(CO1_W1_NOCOVID_2019_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(CO1_W1_NOCOVID_2019_DS_FCPA_RC_MEAN)))) ))
D_CO1_W1_COVID   <- cbind(t(as.numeric(summary(unlist(CO1_W1_COVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(CO1_W1_COVID_2020_DS_FCPA_RC_MEAN)))) ))
D_CO1_W2_NOCOVID <- cbind(t(as.numeric(summary(unlist(CO1_W2_NOCOVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(CO1_W2_NOCOVID_2020_DS_FCPA_RC_MEAN)))) ))
D_CO1_W2_COVID   <- cbind(t(as.numeric(summary(unlist(CO1_W2_COVID_2021_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(CO1_W2_COVID_2021_DS_FCPA_RC_MEAN)))) ))
D_CO1 <- rbind(D_CO1_W1_NOCOVID,D_CO1_W1_COVID,D_CO1_W2_NOCOVID,D_CO1_W2_COVID)

D_CO2_W1_NOCOVID <- cbind(t(as.numeric(summary(unlist(CO2_W1_NOCOVID_2019_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(CO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN)))) ))
D_CO2_W1_COVID   <- cbind(t(as.numeric(summary(unlist(CO2_W1_COVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(CO2_W1_COVID_2020_DS_FCPA_RC_MEAN)))) ))
D_CO2_W2_NOCOVID <- cbind(t(as.numeric(summary(unlist(CO2_W2_NOCOVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(CO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN)))) ))
D_CO2_W2_COVID   <- cbind(t(as.numeric(summary(unlist(CO2_W2_COVID_2021_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(CO2_W2_COVID_2021_DS_FCPA_RC_MEAN)))) ))
D_CO2 <- rbind(D_CO2_W1_NOCOVID,D_CO2_W1_COVID,D_CO2_W2_NOCOVID,D_CO2_W2_COVID)

D_NO2_W1_NOCOVID <- cbind(t(as.numeric(summary(unlist(NO2_W1_NOCOVID_2019_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(NO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN)))) ))
D_NO2_W1_COVID   <- cbind(t(as.numeric(summary(unlist(NO2_W1_COVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(NO2_W1_COVID_2020_DS_FCPA_RC_MEAN)))) ))
D_NO2_W2_NOCOVID <- cbind(t(as.numeric(summary(unlist(NO2_W2_NOCOVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(NO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN)))) ))
D_NO2_W2_COVID   <- cbind(t(as.numeric(summary(unlist(NO2_W2_COVID_2021_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(NO2_W2_COVID_2021_DS_FCPA_RC_MEAN)))) ))
D_NO2 <- rbind(D_NO2_W1_NOCOVID,D_NO2_W1_COVID,D_NO2_W2_NOCOVID,D_NO2_W2_COVID)

D_O3_W1_NOCOVID <- cbind(t(as.numeric(summary(unlist(O3_W1_NOCOVID_2019_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(O3_W1_NOCOVID_2019_DS_FCPA_RC_MEAN)))) ))
D_O3_W1_COVID   <- cbind(t(as.numeric(summary(unlist(O3_W1_COVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(O3_W1_COVID_2020_DS_FCPA_RC_MEAN)))) ))
D_O3_W2_NOCOVID <- cbind(t(as.numeric(summary(unlist(O3_W2_NOCOVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(O3_W2_NOCOVID_2020_DS_FCPA_RC_MEAN)))) ))
D_O3_W2_COVID   <- cbind(t(as.numeric(summary(unlist(O3_W2_COVID_2021_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(O3_W2_COVID_2021_DS_FCPA_RC_MEAN)))) ))
D_O3 <- rbind(D_O3_W1_NOCOVID,D_O3_W1_COVID,D_O3_W2_NOCOVID,D_O3_W2_COVID)

D_SO2_W1_NOCOVID <- cbind(t(as.numeric(summary(unlist(SO2_W1_NOCOVID_2019_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(SO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN)))) ))
D_SO2_W1_COVID   <- cbind(t(as.numeric(summary(unlist(SO2_W1_COVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(SO2_W1_COVID_2020_DS_FCPA_RC_MEAN)))) ))
D_SO2_W2_NOCOVID <- cbind(t(as.numeric(summary(unlist(SO2_W2_NOCOVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(SO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN)))) ))
D_SO2_W2_COVID   <- cbind(t(as.numeric(summary(unlist(SO2_W2_COVID_2021_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(SO2_W2_COVID_2021_DS_FCPA_RC_MEAN)))) ))
D_SO2 <- rbind(D_SO2_W1_NOCOVID,D_SO2_W1_COVID,D_SO2_W2_NOCOVID,D_SO2_W2_COVID)

D_AEI_W1_NOCOVID <- cbind(t(as.numeric(summary(unlist(AEI_W1_NOCOVID_2019_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(AEI_W1_NOCOVID_2019_DS_FCPA_RC_MEAN)))) ))
D_AEI_W1_COVID   <- cbind(t(as.numeric(summary(unlist(AEI_W1_COVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(AEI_W1_COVID_2020_DS_FCPA_RC_MEAN)))) ))
D_AEI_W2_NOCOVID <- cbind(t(as.numeric(summary(unlist(AEI_W2_NOCOVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(AEI_W2_NOCOVID_2020_DS_FCPA_RC_MEAN)))) ))
D_AEI_W2_COVID   <- cbind(t(as.numeric(summary(unlist(AEI_W2_COVID_2021_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(AEI_W2_COVID_2021_DS_FCPA_RC_MEAN)))) ))
D_AEI <- rbind(D_AEI_W1_NOCOVID,D_AEI_W1_COVID,D_AEI_W2_NOCOVID,D_AEI_W2_COVID)

D_HCHO_W1_NOCOVID <- cbind(t(as.numeric(summary(unlist(HCHO_W1_NOCOVID_2019_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(HCHO_W1_NOCOVID_2019_DS_FCPA_RC_MEAN)))) ))
D_HCHO_W1_COVID   <- cbind(t(as.numeric(summary(unlist(HCHO_W1_COVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(HCHO_W1_COVID_2020_DS_FCPA_RC_MEAN)))) ))
D_HCHO_W2_NOCOVID <- cbind(t(as.numeric(summary(unlist(HCHO_W2_NOCOVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(HCHO_W2_NOCOVID_2020_DS_FCPA_RC_MEAN)))) ))
D_HCHO_W2_COVID   <- cbind(t(as.numeric(summary(unlist(HCHO_W2_COVID_2021_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(HCHO_W2_COVID_2021_DS_FCPA_RC_MEAN)))) ))
D_HCHO <- rbind(D_HCHO_W1_NOCOVID,D_HCHO_W1_COVID,D_HCHO_W2_NOCOVID,D_HCHO_W2_COVID)

D_Pressure_W1_NOCOVID <- cbind(t(as.numeric(summary(unlist(Pressure_W1_NOCOVID_2019_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(Pressure_W1_NOCOVID_2019_DS_FCPA_RC_MEAN)))) ))
D_Pressure_W1_COVID   <- cbind(t(as.numeric(summary(unlist(Pressure_W1_COVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN)))) ))
D_Pressure_W2_NOCOVID <- cbind(t(as.numeric(summary(unlist(Pressure_W2_NOCOVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(Pressure_W2_NOCOVID_2020_DS_FCPA_RC_MEAN)))) ))
D_Pressure_W2_COVID   <- cbind(t(as.numeric(summary(unlist(Pressure_W2_COVID_2021_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN)))) ))
D_Pressure <- rbind(D_Pressure_W1_NOCOVID,D_Pressure_W1_COVID,D_Pressure_W2_NOCOVID,D_Pressure_W2_COVID)

D_TPrecipitation_W1_NOCOVID <- cbind(t(as.numeric(summary(unlist(TPrecipitation_W1_NOCOVID_2019_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(TPrecipitation_W1_NOCOVID_2019_DS_FCPA_RC_MEAN)))) ))
D_TPrecipitation_W1_COVID   <- cbind(t(as.numeric(summary(unlist(TPrecipitation_W1_COVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN)))) ))
D_TPrecipitation_W2_NOCOVID <- cbind(t(as.numeric(summary(unlist(TPrecipitation_W2_NOCOVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(TPrecipitation_W2_NOCOVID_2020_DS_FCPA_RC_MEAN)))) ))
D_TPrecipitation_W2_COVID   <- cbind(t(as.numeric(summary(unlist(TPrecipitation_W2_COVID_2021_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN)))) ))
D_TPrecipitation <- rbind(D_TPrecipitation_W1_NOCOVID,D_TPrecipitation_W1_COVID,D_TPrecipitation_W2_NOCOVID,D_TPrecipitation_W2_COVID)

D_MTemp_W1_NOCOVID <- cbind(t(as.numeric(summary(unlist(MTemp_W1_NOCOVID_2019_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(MTemp_W1_NOCOVID_2019_DS_FCPA_RC_MEAN)))) ))
D_MTemp_W1_COVID   <- cbind(t(as.numeric(summary(unlist(MTemp_W1_COVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN)))) ))
D_MTemp_W2_NOCOVID <- cbind(t(as.numeric(summary(unlist(MTemp_W2_NOCOVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(MTemp_W2_NOCOVID_2020_DS_FCPA_RC_MEAN)))) ))
D_MTemp_W2_COVID   <- cbind(t(as.numeric(summary(unlist(MTemp_W2_COVID_2021_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN)))) ))
D_MTemp <- rbind(D_MTemp_W1_NOCOVID,D_MTemp_W1_COVID,D_MTemp_W2_NOCOVID,D_MTemp_W2_COVID)

D_UWind_W1_NOCOVID <- cbind(t(as.numeric(summary(unlist(UWind_W1_NOCOVID_2019_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(UWind_W1_NOCOVID_2019_DS_FCPA_RC_MEAN)))) ))
D_UWind_W1_COVID   <- cbind(t(as.numeric(summary(unlist(UWind_W1_COVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN)))) ))
D_UWind_W2_NOCOVID <- cbind(t(as.numeric(summary(unlist(UWind_W2_NOCOVID_2020_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(UWind_W2_NOCOVID_2020_DS_FCPA_RC_MEAN)))) ))
D_UWind_W2_COVID   <- cbind(t(as.numeric(summary(unlist(UWind_W2_COVID_2021_DS_AVG[,2])))) , t(as.numeric(unlist(summary(colMeans(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN)))) ))
D_UWind <- rbind(D_UWind_W1_NOCOVID,D_UWind_W1_COVID,D_UWind_W2_NOCOVID,D_UWind_W2_COVID)


Descriptive_DS <- data.frame(rbind(D_CO1,D_CO2,D_NO2,D_O3,D_SO2,D_AEI,D_HCHO,D_Pressure,D_TPrecipitation,D_MTemp,D_UWind))
names(Descriptive_DS) <- c("Real_Min","Real_Q1","Real_Median","Real_Mean","Real_Q3","Real_Max",
                           "RC_Min","RC_Q1","RC_Median","RC_Mean","RC_Q3","RC_Max")
Descriptive_DS$GAS <- c(rep("CO1",4),rep("CO2",4),rep("NO2",4),rep("O3",4),rep("SO2",4),rep("AEI",4),rep("HCHO",4),rep("Pressure",4),rep("TPrec",4),rep("MTemp",4),rep("UWind",4)) 
Descriptive_DS$Waves <- rep(c( rep("Wave 1",2),rep("Wave 2",2) ) ,11)
Descriptive_DS$Status <-  rep(rep(c("No COVID","COVID"),2),11)

write.csv(Descriptive_DS,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_Tables\\Descriptive.csv")


#############################
############################# FOF Regressions
#############################
#############################

################ Wave 1
### CO1
FF_W1_CO1_m1 <- pffr(CO1_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(CO1_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37), yind = 1:37)
FF_W1_CO1_m2 <- pffr(CO1_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(CO1_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                       ff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                       ff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                       ff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                       ff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                     yind = 1:37)

FF_W1_CO1_m1_sum <- summary(FF_W1_CO1_m1)
FF_W1_CO1_m2_sum <- summary(FF_W1_CO1_m2)
FF_W1_CO1_AIC_Tables <- data.frame(m1_AIC = AIC(FF_W1_CO1_m1),m1_BIC = BIC(FF_W1_CO1_m1),m1_rsq = FF_W1_CO1_m1_sum$r.sq,
                                   m2_AIC = AIC(FF_W1_CO1_m2),m2_BIC = BIC(FF_W1_CO1_m2),m2_rsq = FF_W1_CO1_m2_sum$r.sq)

FF_W1_CO1_m1_OBS_Fitted <- rowMeans(CO1_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_CO1_m1))
FF_W1_CO1_m2_OBS_Fitted <- rowMeans(CO1_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_CO1_m2))
plot(x=FF_W1_CO1_m1_OBS_Fitted,y=FF_W1_CO1_m2_OBS_Fitted, xlab="Model 1",ylab="Model 2")


#Extract coefficients
FF_W1_CO1_m2_allcoef <- coef(FF_W1_CO1_m2)
## using seWithMean for  s(yind.vec) .

#Extract the smooth coefficients. They are stored in a vector, but they are then transformed into a matrix 
FF_W1_CO1_m2_smcoef <- FF_W1_CO1_m2_allcoef$smterms$`ff(CO1_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$value

#Extract the predictor functional arguments
FF_W1_CO1_m2_xsm <- FF_W1_CO1_m2_allcoef$smterms$`ff(CO1_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$x

#Extract the outcome functional arguments
FF_W1_CO1_m2_ysm <- FF_W1_CO1_m2_allcoef$smterms$`ff(CO1_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$y

#Transform the smooth coefficients into a matrix to prepare for plotting
FF_W1_CO1_m2_smcoef_plot <- matrix(FF_W1_CO1_m2_smcoef, nrow=length(FF_W1_CO1_m2_xsm))

#Use image.plot in the fields package to display the smooth coefficient
#par(mfrow = c(1, 2), mar = c(5, 5, 5, 5))

#Extract therelevanttermfromthefittedobject
beta_hat_CO1_W1_NOCOVID_2019 <-FF_W1_CO1_m2_allcoef$smterms$`ff(CO1_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$coef
#Create 95%CIandgetp-values
beta_hat_CO1_W1_NOCOVID_2019 <-
  beta_hat_CO1_W1_NOCOVID_2019 %>%
  mutate(LB =value - qnorm(0.975) * se,
         UB =value + qnorm(0.975) * se,
         p_val =2 * pnorm(abs(value / se),lower.tail= FALSE))

beta_hat_CO1_W1_NOCOVID_2019_pvalue_plot <- matrix(beta_hat_CO1_W1_NOCOVID_2019$p_val, nrow=length(FF_W1_CO1_m2_xsm))

# image.plot(FF_W1_CO1_m2_xsm, FF_W1_CO1_m2_ysm, beta_hat_CO1_W1_NOCOVID_2019_pvalue_plot,
#            xlab = "CO1 (Year 2019) ", 
#            ylab = "CO1 (Year 2020) ", 
#            main = "CO1 Lock down wave 1 2020 on CO1 2019",
#            # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
#            #  legend.shrink = 0.8,
#            #  legend.line = -1.5, 
#            legend.width = 0.5,
#            col =  viridis(3), 
#            breaks = c(0,0.05,0.10,1) )


image.plot(FF_W1_CO1_m2_xsm, FF_W1_CO1_m2_ysm, FF_W1_CO1_m2_smcoef_plot,
           xlab = "CO1 (Year 2019) ", 
           ylab = "CO1 (Year 2020) ", 
           main = "CO1 Lock down wave 1 2020 on CO1 2019",
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W1_CO1_m2_xsm, FF_W1_CO1_m2_ysm, beta_hat_CO1_W1_NOCOVID_2019_pvalue_plot,
        nlevels  = 5, add = TRUE, col = grey(0.1)) 



### CO2
FF_W1_CO2_m1 <- pffr(CO2_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(CO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37), yind = 1:37)
FF_W1_CO2_m2 <- pffr(CO2_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(CO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                       ff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                       ff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                       ff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                       ff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                     yind = 1:37)

FF_W1_CO2_m1_sum <- summary(FF_W1_CO2_m1)
FF_W1_CO2_m2_sum <- summary(FF_W1_CO2_m2)
FF_W1_CO2_AIC_Tables <- data.frame(m1_AIC = AIC(FF_W1_CO2_m1),m1_BIC = BIC(FF_W1_CO2_m1),m1_rsq = FF_W1_CO2_m1_sum$r.sq,
                                   m2_AIC = AIC(FF_W1_CO2_m2),m2_BIC = BIC(FF_W1_CO2_m2),m2_rsq = FF_W1_CO2_m2_sum$r.sq)

FF_W1_CO2_m1_OBS_Fitted <- rowMeans(CO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_CO2_m1))
FF_W1_CO2_m2_OBS_Fitted <- rowMeans(CO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_CO2_m2))
plot(x=FF_W1_CO2_m1_OBS_Fitted,y=FF_W1_CO2_m2_OBS_Fitted, xlab="Model 1",ylab="Model 2")


#Extract coefficients
FF_W1_CO2_m2_allcoef <- coef(FF_W1_CO2_m2)
## using seWithMean for  s(yind.vec) .

#Extract the smooth coefficients. They are stored in a vector, but they are then transformed into a matrix 
FF_W1_CO2_m2_smcoef <- FF_W1_CO2_m2_allcoef$smterms$`ff(CO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$value

#Extract the predictor functional arguments
FF_W1_CO2_m2_xsm <- FF_W1_CO2_m2_allcoef$smterms$`ff(CO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$x

#Extract the outcome functional arguments
FF_W1_CO2_m2_ysm <- FF_W1_CO2_m2_allcoef$smterms$`ff(CO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$y

#Transform the smooth coefficients into a matrix to prepare for plotting
FF_W1_CO2_m2_smcoef_plot <- matrix(FF_W1_CO2_m2_smcoef, nrow=length(FF_W1_CO2_m2_xsm))

#Use image.plot in the fields package to display the smooth coefficient
#par(mfrow = c(1, 2), mar = c(5, 5, 5, 5))

#Extract therelevanttermfromthefittedobject
beta_hat_CO2_W1_NOCOVID_2019 <-FF_W1_CO2_m2_allcoef$smterms$`ff(CO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$coef
#Create 95%CIandgetp-values
beta_hat_CO2_W1_NOCOVID_2019 <-
  beta_hat_CO2_W1_NOCOVID_2019 %>%
  mutate(LB =value - qnorm(0.975) * se,
         UB =value + qnorm(0.975) * se,
         p_val =2 * pnorm(abs(value / se),lower.tail= FALSE))

beta_hat_CO2_W1_NOCOVID_2019_pvalue_plot <- matrix(beta_hat_CO2_W1_NOCOVID_2019$p_val, nrow=length(FF_W1_CO2_m2_xsm))

# image.plot(FF_W1_CO2_m2_xsm, FF_W1_CO2_m2_ysm, beta_hat_CO2_W1_NOCOVID_2019_pvalue_plot,
#            xlab = "CO2 (Year 2019) ", 
#            ylab = "CO2 (Year 2020) ", 
#            main = "CO2 Lock down wave 1 2020 on CO1 2019",
#            # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
#            #  legend.shrink = 0.8,
#            #  legend.line = -1.5, 
#            legend.width = 0.5,
#            col =  viridis(3), 
#            breaks = c(0,0.05,0.10,1) )


image.plot(FF_W1_CO2_m2_xsm, FF_W1_CO2_m2_ysm, FF_W1_CO2_m2_smcoef_plot,
           xlab = "CO2 (Year 2019) ", 
           ylab = "CO2 (Year 2020) ", 
           main = "CO2 Lock down wave 1 2020 on CO2 2019",
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W1_CO2_m2_xsm, FF_W1_CO2_m2_ysm, beta_hat_CO2_W1_NOCOVID_2019_pvalue_plot,
        levels  = c(0.01,0.05,0.10,0.20), add = TRUE, col = grey(0.1)) 


###

### NO2
FF_W1_NO2_m1 <- pffr(NO2_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(NO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37), yind = 1:37)
FF_W1_NO2_m2 <- pffr(NO2_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(NO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                       ff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                       ff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                       ff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                       ff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                     yind = 1:37)

FF_W1_NO2_m1_sum <- summary(FF_W1_NO2_m1)
FF_W1_NO2_m2_sum <- summary(FF_W1_NO2_m2)
FF_W1_NO2_AIC_Tables <- data.frame(m1_AIC = AIC(FF_W1_NO2_m1),m1_BIC = BIC(FF_W1_NO2_m1),m1_rsq = FF_W1_NO2_m1_sum$r.sq,
                                   m2_AIC = AIC(FF_W1_NO2_m2),m2_BIC = BIC(FF_W1_NO2_m2),m2_rsq = FF_W1_NO2_m2_sum$r.sq)

FF_W1_NO2_m1_OBS_Fitted <- rowMeans(NO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_NO2_m1))
FF_W1_NO2_m2_OBS_Fitted <- rowMeans(NO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_NO2_m2))
plot(x=FF_W1_NO2_m1_OBS_Fitted,y=FF_W1_NO2_m2_OBS_Fitted, xlab="Model 1",ylab="Model 2")


#Extract coefficients
FF_W1_NO2_m2_allcoef <- coef(FF_W1_NO2_m2)
## using seWithMean for  s(yind.vec) .

#Extract the smooth coefficients. They are stored in a vector, but they are then transformed into a matrix 
FF_W1_NO2_m2_smcoef <- FF_W1_NO2_m2_allcoef$smterms$`ff(NO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$value

#Extract the predictor functional arguments
FF_W1_NO2_m2_xsm <- FF_W1_NO2_m2_allcoef$smterms$`ff(NO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$x

#Extract the outcome functional arguments
FF_W1_NO2_m2_ysm <- FF_W1_NO2_m2_allcoef$smterms$`ff(NO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$y

#Transform the smooth coefficients into a matrix to prepare for plotting
FF_W1_NO2_m2_smcoef_plot <- matrix(FF_W1_NO2_m2_smcoef, nrow=length(FF_W1_NO2_m2_xsm))

#Use image.plot in the fields package to display the smooth coefficient
#par(mfrow = c(1, 2), mar = c(5, 5, 5, 5))

#Extract therelevanttermfromthefittedobject
beta_hat_NO2_W1_NOCOVID_2019 <-FF_W1_NO2_m2_allcoef$smterms$`ff(NO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$coef
#Create 95%CIandgetp-values
beta_hat_NO2_W1_NOCOVID_2019 <-
  beta_hat_NO2_W1_NOCOVID_2019 %>%
  mutate(LB =value - qnorm(0.975) * se,
         UB =value + qnorm(0.975) * se,
         p_val =2 * pnorm(abs(value / se),lower.tail= FALSE))

beta_hat_NO2_W1_NOCOVID_2019_pvalue_plot <- matrix(beta_hat_NO2_W1_NOCOVID_2019$p_val, nrow=length(FF_W1_NO2_m2_xsm))

# image.plot(FF_W1_NO2_m2_xsm, FF_W1_NO2_m2_ysm, beta_hat_NO2_W1_NOCOVID_2019_pvalue_plot,
#            xlab = "NO2 (Year 2019) ", 
#            ylab = "NO2 (Year 2020) ", 
#            main = "NO2 Lock down wave 1 2020 on CO1 2019",
#            # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
#            #  legend.shrink = 0.8,
#            #  legend.line = -1.5, 
#            legend.width = 0.5,
#            col =  viridis(3), 
#            breaks = c(0,0.05,0.10,1) )


image.plot(FF_W1_NO2_m2_xsm, FF_W1_NO2_m2_ysm, FF_W1_NO2_m2_smcoef_plot,
           xlab = "NO2 (Year 2019) ", 
           ylab = "NO2 (Year 2020) ", 
           main = "NO2 Lock down wave 1 2020 on NO2 2019",
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W1_NO2_m2_xsm, FF_W1_NO2_m2_ysm, beta_hat_NO2_W1_NOCOVID_2019_pvalue_plot,
        levels  = c(0.01,0.05,0.10,0.20), add = TRUE, col = grey(0.1)) 



### O3
FF_W1_O3_m1 <- pffr(O3_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(O3_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37), yind = 1:37)
FF_W1_O3_m2 <- pffr(O3_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(O3_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                      ff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                      ff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                      ff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                      ff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                    yind = 1:37)

FF_W1_O3_m1_sum <- summary(FF_W1_O3_m1)
FF_W1_O3_m2_sum <- summary(FF_W1_O3_m2)
FF_W1_O3_AIC_Tables <- data.frame(m1_AIC = AIC(FF_W1_O3_m1),m1_BIC = BIC(FF_W1_O3_m1),m1_rsq = FF_W1_O3_m1_sum$r.sq,
                                  m2_AIC = AIC(FF_W1_O3_m2),m2_BIC = BIC(FF_W1_O3_m2),m2_rsq = FF_W1_O3_m2_sum$r.sq)

FF_W1_O3_m1_OBS_Fitted <- rowMeans(O3_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_O3_m1))
FF_W1_O3_m2_OBS_Fitted <- rowMeans(O3_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_O3_m2))
plot(x=FF_W1_O3_m1_OBS_Fitted,y=FF_W1_O3_m2_OBS_Fitted, xlab="Model 1",ylab="Model 2")


#Extract coefficients
FF_W1_O3_m2_allcoef <- coef(FF_W1_O3_m2)
## using seWithMean for  s(yind.vec) .

#Extract the smooth coefficients. They are stored in a vector, but they are then transformed into a matrix 
FF_W1_O3_m2_smcoef <- FF_W1_O3_m2_allcoef$smterms$`ff(O3_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$value

#Extract the predictor functional arguments
FF_W1_O3_m2_xsm <- FF_W1_O3_m2_allcoef$smterms$`ff(O3_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$x

#Extract the outcome functional arguments
FF_W1_O3_m2_ysm <- FF_W1_O3_m2_allcoef$smterms$`ff(O3_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$y

#Transform the smooth coefficients into a matrix to prepare for plotting
FF_W1_O3_m2_smcoef_plot <- matrix(FF_W1_O3_m2_smcoef, nrow=length(FF_W1_O3_m2_xsm))

#Use image.plot in the fields package to display the smooth coefficient
#par(mfrow = c(1, 2), mar = c(5, 5, 5, 5))

#Extract therelevanttermfromthefittedobject
beta_hat_O3_W1_NOCOVID_2019 <-FF_W1_O3_m2_allcoef$smterms$`ff(O3_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$coef
#Create 95%CIandgetp-values
beta_hat_O3_W1_NOCOVID_2019 <-
  beta_hat_O3_W1_NOCOVID_2019 %>%
  mutate(LB =value - qnorm(0.975) * se,
         UB =value + qnorm(0.975) * se,
         p_val =2 * pnorm(abs(value / se),lower.tail= FALSE))

beta_hat_O3_W1_NOCOVID_2019_pvalue_plot <- matrix(beta_hat_O3_W1_NOCOVID_2019$p_val, nrow=length(FF_W1_O3_m2_xsm))

# image.plot(FF_W1_O3_m2_xsm, FF_W1_O3_m2_ysm, beta_hat_O3_W1_NOCOVID_2019_pvalue_plot,
#            xlab = "O3 (Year 2019) ", 
#            ylab = "O3 (Year 2020) ", 
#            main = "O3 Lock down wave 1 2020 on CO1 2019",
#            # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
#            #  legend.shrink = 0.8,
#            #  legend.line = -1.5, 
#            legend.width = 0.5,
#            col =  viridis(3), 
#            breaks = c(0,0.05,0.10,1) )


image.plot(FF_W1_O3_m2_xsm, FF_W1_O3_m2_ysm, FF_W1_O3_m2_smcoef_plot,
           xlab = "O3 (Year 2019) ", 
           ylab = "O3 (Year 2020) ", 
           main = "O3 Lock down wave 1 2020 on O3 2019",
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W1_O3_m2_xsm, FF_W1_O3_m2_ysm, beta_hat_O3_W1_NOCOVID_2019_pvalue_plot,
        nlevels  = 5, add = TRUE, col = grey(0.1)) 







### SO2
FF_W1_SO2_m1 <- pffr(SO2_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(SO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37), yind = 1:37)
FF_W1_SO2_m2 <- pffr(SO2_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(SO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                       ff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                       ff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                       ff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                       ff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                     yind = 1:37)

FF_W1_SO2_m1_sum <- summary(FF_W1_SO2_m1)
FF_W1_SO2_m2_sum <- summary(FF_W1_SO2_m2)
FF_W1_SO2_AIC_Tables <- data.frame(m1_AIC = AIC(FF_W1_SO2_m1),m1_BIC = BIC(FF_W1_SO2_m1),m1_rsq = FF_W1_SO2_m1_sum$r.sq,
                                   m2_AIC = AIC(FF_W1_SO2_m2),m2_BIC = BIC(FF_W1_SO2_m2),m2_rsq = FF_W1_SO2_m2_sum$r.sq)

FF_W1_SO2_m1_OBS_Fitted <- rowMeans(SO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_SO2_m1))
FF_W1_SO2_m2_OBS_Fitted <- rowMeans(SO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_SO2_m2))
plot(x=FF_W1_SO2_m1_OBS_Fitted,y=FF_W1_SO2_m2_OBS_Fitted, xlab="Model 1",ylab="Model 2")


#Extract coefficients
FF_W1_SO2_m2_allcoef <- coef(FF_W1_SO2_m2)
## using seWithMean for  s(yind.vec) .

#Extract the smooth coefficients. They are stored in a vector, but they are then transformed into a matrix 
FF_W1_SO2_m2_smcoef <- FF_W1_SO2_m2_allcoef$smterms$`ff(SO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$value

#Extract the predictor functional arguments
FF_W1_SO2_m2_xsm <- FF_W1_SO2_m2_allcoef$smterms$`ff(SO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$x

#Extract the outcome functional arguments
FF_W1_SO2_m2_ysm <- FF_W1_SO2_m2_allcoef$smterms$`ff(SO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$y

#Transform the smooth coefficients into a matrix to prepare for plotting
FF_W1_SO2_m2_smcoef_plot <- matrix(FF_W1_SO2_m2_smcoef, nrow=length(FF_W1_SO2_m2_xsm))

#Use image.plot in the fields package to display the smooth coefficient
#par(mfrow = c(1, 2), mar = c(5, 5, 5, 5))

#Extract therelevanttermfromthefittedobject
beta_hat_SO2_W1_NOCOVID_2019 <-FF_W1_SO2_m2_allcoef$smterms$`ff(SO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$coef
#Create 95%CIandgetp-values
beta_hat_SO2_W1_NOCOVID_2019 <-
  beta_hat_SO2_W1_NOCOVID_2019 %>%
  mutate(LB =value - qnorm(0.975) * se,
         UB =value + qnorm(0.975) * se,
         p_val =2 * pnorm(abs(value / se),lower.tail= FALSE))

beta_hat_SO2_W1_NOCOVID_2019_pvalue_plot <- matrix(beta_hat_SO2_W1_NOCOVID_2019$p_val, nrow=length(FF_W1_SO2_m2_xsm))

# image.plot(FF_W1_SO2_m2_xsm, FF_W1_SO2_m2_ysm, beta_hat_SO2_W1_NOCOVID_2019_pvalue_plot,
#            xlab = "SO2 (Year 2019) ", 
#            ylab = "SO2 (Year 2020) ", 
#            main = "SO2 Lock down wave 1 2020 on CO1 2019",
#            # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
#            #  legend.shrink = 0.8,
#            #  legend.line = -1.5, 
#            legend.width = 0.5,
#            col =  viridis(3), 
#            breaks = c(0,0.05,0.10,1) )


image.plot(FF_W1_SO2_m2_xsm, FF_W1_SO2_m2_ysm, FF_W1_SO2_m2_smcoef_plot,
           xlab = "SO2 (Year 2019) ", 
           ylab = "SO2 (Year 2020) ", 
           main = "SO2 Lock down wave 1 2020 on SO2 2019",
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W1_SO2_m2_xsm, FF_W1_SO2_m2_ysm, beta_hat_SO2_W1_NOCOVID_2019_pvalue_plot,
        nlevels  = 5, add = TRUE, col = grey(0.1)) 





### AEI
FF_W1_AEI_m1 <- pffr(AEI_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(AEI_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37), yind = 1:37)
FF_W1_AEI_m2 <- pffr(AEI_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(AEI_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                       ff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                       ff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                       ff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                       ff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                     yind = 1:37)

FF_W1_AEI_m1_sum <- summary(FF_W1_AEI_m1)
FF_W1_AEI_m2_sum <- summary(FF_W1_AEI_m2)
FF_W1_AEI_AIC_Tables <- data.frame(m1_AIC = AIC(FF_W1_AEI_m1),m1_BIC = BIC(FF_W1_AEI_m1),m1_rsq = FF_W1_AEI_m1_sum$r.sq,
                                   m2_AIC = AIC(FF_W1_AEI_m2),m2_BIC = BIC(FF_W1_AEI_m2),m2_rsq = FF_W1_AEI_m2_sum$r.sq)

FF_W1_AEI_m1_OBS_Fitted <- rowMeans(AEI_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_AEI_m1))
FF_W1_AEI_m2_OBS_Fitted <- rowMeans(AEI_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_AEI_m2))
plot(x=FF_W1_AEI_m1_OBS_Fitted,y=FF_W1_AEI_m2_OBS_Fitted, xlab="Model 1",ylab="Model 2")


#Extract coefficients
FF_W1_AEI_m2_allcoef <- coef(FF_W1_AEI_m2)
## using seWithMean for  s(yind.vec) .

#Extract the smooth coefficients. They are stored in a vector, but they are then transformed into a matrix 
FF_W1_AEI_m2_smcoef <- FF_W1_AEI_m2_allcoef$smterms$`ff(AEI_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$value

#Extract the predictor functional arguments
FF_W1_AEI_m2_xsm <- FF_W1_AEI_m2_allcoef$smterms$`ff(AEI_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$x

#Extract the outcome functional arguments
FF_W1_AEI_m2_ysm <- FF_W1_AEI_m2_allcoef$smterms$`ff(AEI_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$y

#Transform the smooth coefficients into a matrix to prepare for plotting
FF_W1_AEI_m2_smcoef_plot <- matrix(FF_W1_AEI_m2_smcoef, nrow=length(FF_W1_AEI_m2_xsm))

#Use image.plot in the fields package to display the smooth coefficient
#par(mfrow = c(1, 2), mar = c(5, 5, 5, 5))

#Extract therelevanttermfromthefittedobject
beta_hat_AEI_W1_NOCOVID_2019 <-FF_W1_AEI_m2_allcoef$smterms$`ff(AEI_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$coef
#Create 95%CIandgetp-values
beta_hat_AEI_W1_NOCOVID_2019 <-
  beta_hat_AEI_W1_NOCOVID_2019 %>%
  mutate(LB =value - qnorm(0.975) * se,
         UB =value + qnorm(0.975) * se,
         p_val =2 * pnorm(abs(value / se),lower.tail= FALSE))

beta_hat_AEI_W1_NOCOVID_2019_pvalue_plot <- matrix(beta_hat_AEI_W1_NOCOVID_2019$p_val, nrow=length(FF_W1_AEI_m2_xsm))

# image.plot(FF_W1_AEI_m2_xsm, FF_W1_AEI_m2_ysm, beta_hat_AEI_W1_NOCOVID_2019_pvalue_plot,
#            xlab = "AEI (Year 2019) ", 
#            ylab = "AEI (Year 2020) ", 
#            main = "AEI Lock down wave 1 2020 on CO1 2019",
#            # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
#            #  legend.shrink = 0.8,
#            #  legend.line = -1.5, 
#            legend.width = 0.5,
#            col =  viridis(3), 
#            breaks = c(0,0.05,0.10,1) )


image.plot(FF_W1_AEI_m2_xsm, FF_W1_AEI_m2_ysm, FF_W1_AEI_m2_smcoef_plot,
           xlab = "AEI (Year 2019) ", 
           ylab = "AEI (Year 2020) ", 
           main = "AEI Lock down wave 1 2020 on AEI 2019",
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W1_AEI_m2_xsm, FF_W1_AEI_m2_ysm, beta_hat_AEI_W1_NOCOVID_2019_pvalue_plot,
        nlevels  = 5, add = TRUE, col = grey(0.1)) 



### HCHO
FF_W1_HCHO_m1 <- pffr(HCHO_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(HCHO_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37), yind = 1:37)
FF_W1_HCHO_m2 <- pffr(HCHO_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(HCHO_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                        ff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                        ff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                        ff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                        ff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                      yind = 1:37)

FF_W1_HCHO_m1_sum <- summary(FF_W1_HCHO_m1)
FF_W1_HCHO_m2_sum <- summary(FF_W1_HCHO_m2)
FF_W1_HCHO_AIC_Tables <- data.frame(m1_AIC = AIC(FF_W1_HCHO_m1),m1_BIC = BIC(FF_W1_HCHO_m1),m1_rsq = FF_W1_HCHO_m1_sum$r.sq,
                                    m2_AIC = AIC(FF_W1_HCHO_m2),m2_BIC = BIC(FF_W1_HCHO_m2),m2_rsq = FF_W1_HCHO_m2_sum$r.sq)

FF_W1_HCHO_m1_OBS_Fitted <- rowMeans(HCHO_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_HCHO_m1))
FF_W1_HCHO_m2_OBS_Fitted <- rowMeans(HCHO_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_HCHO_m2))
plot(x=FF_W1_HCHO_m1_OBS_Fitted,y=FF_W1_HCHO_m2_OBS_Fitted, xlab="Model 1",ylab="Model 2")


#Extract coefficients
FF_W1_HCHO_m2_allcoef <- coef(FF_W1_HCHO_m2)
## using seWithMean for  s(yind.vec) .

#Extract the smooth coefficients. They are stored in a vector, but they are then transformed into a matrix 
FF_W1_HCHO_m2_smcoef <- FF_W1_HCHO_m2_allcoef$smterms$`ff(HCHO_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$value

#Extract the predictor functional arguments
FF_W1_HCHO_m2_xsm <- FF_W1_HCHO_m2_allcoef$smterms$`ff(HCHO_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$x

#Extract the outcome functional arguments
FF_W1_HCHO_m2_ysm <- FF_W1_HCHO_m2_allcoef$smterms$`ff(HCHO_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$y

#Transform the smooth coefficients into a matrix to prepare for plotting
FF_W1_HCHO_m2_smcoef_plot <- matrix(FF_W1_HCHO_m2_smcoef, nrow=length(FF_W1_HCHO_m2_xsm))

#Use image.plot in the fields package to display the smooth coefficient
#par(mfrow = c(1, 2), mar = c(5, 5, 5, 5))

#Extract therelevanttermfromthefittedobject
beta_hat_HCHO_W1_NOCOVID_2019 <-FF_W1_HCHO_m2_allcoef$smterms$`ff(HCHO_W1_NOCOVID_2019_DS_FCPA_RC_MEAN,1:37)`$coef
#Create 95%CIandgetp-values
beta_hat_HCHO_W1_NOCOVID_2019 <-
  beta_hat_HCHO_W1_NOCOVID_2019 %>%
  mutate(LB =value - qnorm(0.975) * se,
         UB =value + qnorm(0.975) * se,
         p_val =2 * pnorm(abs(value / se),lower.tail= FALSE))

beta_hat_HCHO_W1_NOCOVID_2019_pvalue_plot <- matrix(beta_hat_HCHO_W1_NOCOVID_2019$p_val, nrow=length(FF_W1_HCHO_m2_xsm))

# image.plot(FF_W1_HCHO_m2_xsm, FF_W1_HCHO_m2_ysm, beta_hat_HCHO_W1_NOCOVID_2019_pvalue_plot,
#            xlab = "HCHO (Year 2019) ", 
#            ylab = "HCHO (Year 2020) ", 
#            main = "HCHO Lock down wave 1 2020 on CO1 2019",
#            # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
#            #  legend.shrink = 0.8,
#            #  legend.line = -1.5, 
#            legend.width = 0.5,
#            col =  viridis(3), 
#            breaks = c(0,0.05,0.10,1) )


image.plot(FF_W1_HCHO_m2_xsm, FF_W1_HCHO_m2_ysm, FF_W1_HCHO_m2_smcoef_plot,
           xlab = "HCHO (Year 2019) ", 
           ylab = "HCHO (Year 2020) ", 
           main = "HCHO Lock down wave 1 2020 on HCHO 2019",
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W1_HCHO_m2_xsm, FF_W1_HCHO_m2_ysm, beta_hat_HCHO_W1_NOCOVID_2019_pvalue_plot,
        nlevels  = 5, add = TRUE, col = grey(0.1)) 






################# Wave 2
### CO1
FF_W2_CO1_m1 <- pffr(CO1_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(CO1_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12), yind = 1:12)
FF_W2_CO1_m2 <- pffr(CO1_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(CO1_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                       ff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                       ff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                       ff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                       ff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                     yind = 1:12)

FF_W2_CO1_m1_sum <- summary(FF_W2_CO1_m1)
FF_W2_CO1_m2_sum <- summary(FF_W2_CO1_m2)
FF_W2_CO1_AIC_Tables <- data.frame(m1_AIC = AIC(FF_W2_CO1_m1),m1_BIC = BIC(FF_W2_CO1_m1),m1_rsq = FF_W2_CO1_m1_sum$r.sq,
                                   m2_AIC = AIC(FF_W2_CO1_m2),m2_BIC = BIC(FF_W2_CO1_m2),m2_rsq = FF_W2_CO1_m2_sum$r.sq)

FF_W2_CO1_m1_OBS_Fitted <- rowMeans(CO1_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_CO1_m1))
FF_W2_CO1_m2_OBS_Fitted <- rowMeans(CO1_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_CO1_m2))
plot(x=FF_W2_CO1_m1_OBS_Fitted,y=FF_W2_CO1_m2_OBS_Fitted, xlab="Model 1",ylab="Model 2")


#Extract coefficients
FF_W2_CO1_m2_allcoef <- coef(FF_W2_CO1_m2)
## using seWithMean for  s(yind.vec) .

#Extract the smooth coefficients. They are stored in a vector, but they are then transformed into a matrix 
FF_W2_CO1_m2_smcoef <- FF_W2_CO1_m2_allcoef$smterms$`ff(CO1_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$value

#Extract the predictor functional arguments
FF_W2_CO1_m2_xsm <- FF_W2_CO1_m2_allcoef$smterms$`ff(CO1_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$x

#Extract the outcome functional arguments
FF_W2_CO1_m2_ysm <- FF_W2_CO1_m2_allcoef$smterms$`ff(CO1_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$y

#Transform the smooth coefficients into a matrix to prepare for plotting
FF_W2_CO1_m2_smcoef_plot <- matrix(FF_W2_CO1_m2_smcoef, nrow=length(FF_W2_CO1_m2_xsm))

#Use image.plot in the fields package to display the smooth coefficient
#par(mfrow = c(1, 2), mar = c(5, 5, 5, 5))

#Extract therelevanttermfromthefittedobject
beta_hat_CO1_W2_NOCOVID_2020 <-FF_W2_CO1_m2_allcoef$smterms$`ff(CO1_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$coef
#Create 95%CIandgetp-values
beta_hat_CO1_W2_NOCOVID_2020 <-
  beta_hat_CO1_W2_NOCOVID_2020 %>%
  mutate(LB =value - qnorm(0.975) * se,
         UB =value + qnorm(0.975) * se,
         p_val =2 * pnorm(abs(value / se),lower.tail= FALSE))

beta_hat_CO1_W2_NOCOVID_2020_pvalue_plot <- matrix(beta_hat_CO1_W2_NOCOVID_2020$p_val, nrow=length(FF_W2_CO1_m2_xsm))

# image.plot(FF_W2_CO1_m2_xsm, FF_W2_CO1_m2_ysm, beta_hat_CO1_W2_NOCOVID_2020_pvalue_plot,
#            xlab = "CO1 (Year 2019) ", 
#            ylab = "CO1 (Year 2020) ", 
#            main = "CO1 Lock down wave 1 2020 on CO1 2019",
#            # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
#            #  legend.shrink = 0.8,
#            #  legend.line = -1.5, 
#            legend.width = 0.5,
#            col =  viridis(3), 
#            breaks = c(0,0.05,0.10,1) )


image.plot(FF_W2_CO1_m2_xsm, FF_W2_CO1_m2_ysm, FF_W2_CO1_m2_smcoef_plot,
           xlab = "CO1 (Year 2020) ", 
           ylab = "CO1 (Year 2021) ", 
           main = "CO1 Lock down wave 2 2021 on 2020",
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W2_CO1_m2_xsm, FF_W2_CO1_m2_ysm, beta_hat_CO1_W2_NOCOVID_2020_pvalue_plot,
        nlevels  = 5, add = TRUE, col = grey(0.1)) 



### CO2
FF_W2_CO2_m1 <- pffr(CO2_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(CO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12), yind = 1:12)
FF_W2_CO2_m2 <- pffr(CO2_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(CO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                       ff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                       ff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                       ff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                       ff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                     yind = 1:12)

FF_W2_CO2_m1_sum <- summary(FF_W2_CO2_m1)
FF_W2_CO2_m2_sum <- summary(FF_W2_CO2_m2)
FF_W2_CO2_AIC_Tables <- data.frame(m1_AIC = AIC(FF_W2_CO2_m1),m1_BIC = BIC(FF_W2_CO2_m1),m1_rsq = FF_W2_CO2_m1_sum$r.sq,
                                   m2_AIC = AIC(FF_W2_CO2_m2),m2_BIC = BIC(FF_W2_CO2_m2),m2_rsq = FF_W2_CO2_m2_sum$r.sq)

FF_W2_CO2_m1_OBS_Fitted <- rowMeans(CO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_CO2_m1))
FF_W2_CO2_m2_OBS_Fitted <- rowMeans(CO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_CO2_m2))
plot(x=FF_W2_CO2_m1_OBS_Fitted,y=FF_W2_CO2_m2_OBS_Fitted, xlab="Model 1",ylab="Model 2")


#Extract coefficients
FF_W2_CO2_m2_allcoef <- coef(FF_W2_CO2_m2)
## using seWithMean for  s(yind.vec) .

#Extract the smooth coefficients. They are stored in a vector, but they are then transformed into a matrix 
FF_W2_CO2_m2_smcoef <- FF_W2_CO2_m2_allcoef$smterms$`ff(CO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$value

#Extract the predictor functional arguments
FF_W2_CO2_m2_xsm <- FF_W2_CO2_m2_allcoef$smterms$`ff(CO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$x

#Extract the outcome functional arguments
FF_W2_CO2_m2_ysm <- FF_W2_CO2_m2_allcoef$smterms$`ff(CO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$y

#Transform the smooth coefficients into a matrix to prepare for plotting
FF_W2_CO2_m2_smcoef_plot <- matrix(FF_W2_CO2_m2_smcoef, nrow=length(FF_W2_CO2_m2_xsm))

#Use image.plot in the fields package to display the smooth coefficient
#par(mfrow = c(1, 2), mar = c(5, 5, 5, 5))

#Extract therelevanttermfromthefittedobject
beta_hat_CO2_W2_NOCOVID_2020 <-FF_W2_CO2_m2_allcoef$smterms$`ff(CO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$coef
#Create 95%CIandgetp-values
beta_hat_CO2_W2_NOCOVID_2020 <-
  beta_hat_CO2_W2_NOCOVID_2020 %>%
  mutate(LB =value - qnorm(0.975) * se,
         UB =value + qnorm(0.975) * se,
         p_val =2 * pnorm(abs(value / se),lower.tail= FALSE))

beta_hat_CO2_W2_NOCOVID_2020_pvalue_plot <- matrix(beta_hat_CO2_W2_NOCOVID_2020$p_val, nrow=length(FF_W2_CO2_m2_xsm))

# image.plot(FF_W2_CO2_m2_xsm, FF_W2_CO2_m2_ysm, beta_hat_CO2_W2_NOCOVID_2020_pvalue_plot,
#            xlab = "CO2 (Year 2019) ", 
#            ylab = "CO2 (Year 2020) ", 
#            main = "CO2 Lock down wave 1 2020 on CO1 2019",
#            # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
#            #  legend.shrink = 0.8,
#            #  legend.line = -1.5, 
#            legend.width = 0.5,
#            col =  viridis(3), 
#            breaks = c(0,0.05,0.10,1) )


image.plot(FF_W2_CO2_m2_xsm, FF_W2_CO2_m2_ysm, FF_W2_CO2_m2_smcoef_plot,
           xlab = "H2O (Year 2020) ", 
           ylab = "H2O (Year 2021) ", 
           main = "H2O Lock down wave 2 2021 on CO2 2020",
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W2_CO2_m2_xsm, FF_W2_CO2_m2_ysm, beta_hat_CO2_W2_NOCOVID_2020_pvalue_plot,
        levels  = c(0.0001,0.001,0.01,0.05,0.10,0.20), add = TRUE, col = grey(0.1)) 


###

### NO2
FF_W2_NO2_m1 <- pffr(NO2_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(NO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12), yind = 1:12)
FF_W2_NO2_m2 <- pffr(NO2_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(NO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                       ff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                       ff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                       ff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                       ff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                     yind = 1:12)

FF_W2_NO2_m1_sum <- summary(FF_W2_NO2_m1)
FF_W2_NO2_m2_sum <- summary(FF_W2_NO2_m2)
FF_W2_NO2_AIC_Tables <- data.frame(m1_AIC = AIC(FF_W2_NO2_m1),m1_BIC = BIC(FF_W2_NO2_m1),m1_rsq = FF_W2_NO2_m1_sum$r.sq,
                                   m2_AIC = AIC(FF_W2_NO2_m2),m2_BIC = BIC(FF_W2_NO2_m2),m2_rsq = FF_W2_NO2_m2_sum$r.sq)

FF_W2_NO2_m1_OBS_Fitted <- rowMeans(NO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_NO2_m1))
FF_W2_NO2_m2_OBS_Fitted <- rowMeans(NO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_NO2_m2))
plot(x=FF_W2_NO2_m1_OBS_Fitted,y=FF_W2_NO2_m2_OBS_Fitted, xlab="Model 1",ylab="Model 2")


#Extract coefficients
FF_W2_NO2_m2_allcoef <- coef(FF_W2_NO2_m2)
## using seWithMean for  s(yind.vec) .

#Extract the smooth coefficients. They are stored in a vector, but they are then transformed into a matrix 
FF_W2_NO2_m2_smcoef <- FF_W2_NO2_m2_allcoef$smterms$`ff(NO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$value

#Extract the predictor functional arguments
FF_W2_NO2_m2_xsm <- FF_W2_NO2_m2_allcoef$smterms$`ff(NO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$x

#Extract the outcome functional arguments
FF_W2_NO2_m2_ysm <- FF_W2_NO2_m2_allcoef$smterms$`ff(NO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$y

#Transform the smooth coefficients into a matrix to prepare for plotting
FF_W2_NO2_m2_smcoef_plot <- matrix(FF_W2_NO2_m2_smcoef, nrow=length(FF_W2_NO2_m2_xsm))

#Use image.plot in the fields package to display the smooth coefficient
#par(mfrow = c(1, 2), mar = c(5, 5, 5, 5))

#Extract therelevanttermfromthefittedobject
beta_hat_NO2_W2_NOCOVID_2020 <-FF_W2_NO2_m2_allcoef$smterms$`ff(NO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$coef
#Create 95%CIandgetp-values
beta_hat_NO2_W2_NOCOVID_2020 <-
  beta_hat_NO2_W2_NOCOVID_2020 %>%
  mutate(LB =value - qnorm(0.975) * se,
         UB =value + qnorm(0.975) * se,
         p_val =2 * pnorm(abs(value / se),lower.tail= FALSE))

beta_hat_NO2_W2_NOCOVID_2020_pvalue_plot <- matrix(beta_hat_NO2_W2_NOCOVID_2020$p_val, nrow=length(FF_W2_NO2_m2_xsm))

# image.plot(FF_W2_NO2_m2_xsm, FF_W2_NO2_m2_ysm, beta_hat_NO2_W2_NOCOVID_2020_pvalue_plot,
#            xlab = "NO2 (Year 2019) ", 
#            ylab = "NO2 (Year 2020) ", 
#            main = "NO2 Lock down wave 1 2020 on CO1 2019",
#            # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
#            #  legend.shrink = 0.8,
#            #  legend.line = -1.5, 
#            legend.width = 0.5,
#            col =  viridis(3), 
#            breaks = c(0,0.05,0.10,1) )


image.plot(FF_W2_NO2_m2_xsm, FF_W2_NO2_m2_ysm, FF_W2_NO2_m2_smcoef_plot,
           xlab = "NO2 (Year 2020) ", 
           ylab = "NO2 (Year 2021) ", 
           main = "NO2 Lock down wave 2 2021 on NO2 2020",
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W2_NO2_m2_xsm, FF_W2_NO2_m2_ysm, beta_hat_NO2_W2_NOCOVID_2020_pvalue_plot,
        levels  = c(0.01,0.05,0.10,0.20), add = TRUE, col = grey(0.1)) 



### O3
FF_W2_O3_m1 <- pffr(O3_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(O3_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12), yind = 1:12)
FF_W2_O3_m2 <- pffr(O3_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(O3_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                      ff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                      ff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                      ff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                      ff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                    yind = 1:12)

FF_W2_O3_m1_sum <- summary(FF_W2_O3_m1)
FF_W2_O3_m2_sum <- summary(FF_W2_O3_m2)
FF_W2_O3_AIC_Tables <- data.frame(m1_AIC = AIC(FF_W2_O3_m1),m1_BIC = BIC(FF_W2_O3_m1),m1_rsq = FF_W2_O3_m1_sum$r.sq,
                                  m2_AIC = AIC(FF_W2_O3_m2),m2_BIC = BIC(FF_W2_O3_m2),m2_rsq = FF_W2_O3_m2_sum$r.sq)

FF_W2_O3_m1_OBS_Fitted <- rowMeans(O3_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_O3_m1))
FF_W2_O3_m2_OBS_Fitted <- rowMeans(O3_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_O3_m2))
plot(x=FF_W2_O3_m1_OBS_Fitted,y=FF_W2_O3_m2_OBS_Fitted, xlab="Model 1",ylab="Model 2")


#Extract coefficients
FF_W2_O3_m2_allcoef <- coef(FF_W2_O3_m2)
## using seWithMean for  s(yind.vec) .

#Extract the smooth coefficients. They are stored in a vector, but they are then transformed into a matrix 
FF_W2_O3_m2_smcoef <- FF_W2_O3_m2_allcoef$smterms$`ff(O3_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$value

#Extract the predictor functional arguments
FF_W2_O3_m2_xsm <- FF_W2_O3_m2_allcoef$smterms$`ff(O3_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$x

#Extract the outcome functional arguments
FF_W2_O3_m2_ysm <- FF_W2_O3_m2_allcoef$smterms$`ff(O3_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$y

#Transform the smooth coefficients into a matrix to prepare for plotting
FF_W2_O3_m2_smcoef_plot <- matrix(FF_W2_O3_m2_smcoef, nrow=length(FF_W2_O3_m2_xsm))

#Use image.plot in the fields package to display the smooth coefficient
#par(mfrow = c(1, 2), mar = c(5, 5, 5, 5))

#Extract therelevanttermfromthefittedobject
beta_hat_O3_W2_NOCOVID_2020 <-FF_W2_O3_m2_allcoef$smterms$`ff(O3_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$coef
#Create 95%CIandgetp-values
beta_hat_O3_W2_NOCOVID_2020 <-
  beta_hat_O3_W2_NOCOVID_2020 %>%
  mutate(LB =value - qnorm(0.975) * se,
         UB =value + qnorm(0.975) * se,
         p_val =2 * pnorm(abs(value / se),lower.tail= FALSE))

beta_hat_O3_W2_NOCOVID_2020_pvalue_plot <- matrix(beta_hat_O3_W2_NOCOVID_2020$p_val, nrow=length(FF_W2_O3_m2_xsm))

# image.plot(FF_W2_O3_m2_xsm, FF_W2_O3_m2_ysm, beta_hat_O3_W2_NOCOVID_2020_pvalue_plot,
#            xlab = "O3 (Year 2019) ", 
#            ylab = "O3 (Year 2020) ", 
#            main = "O3 Lock down wave 1 2020 on CO1 2019",
#            # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
#            #  legend.shrink = 0.8,
#            #  legend.line = -1.5, 
#            legend.width = 0.5,
#            col =  viridis(3), 
#            breaks = c(0,0.05,0.10,1) )


image.plot(FF_W2_O3_m2_xsm, FF_W2_O3_m2_ysm, FF_W2_O3_m2_smcoef_plot,
           xlab = "O3 (Year 2020) ", 
           ylab = "O3 (Year 2021) ", 
           main = "O3 Lock down wave 2 2021 on O3 2020",
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W2_O3_m2_xsm, FF_W2_O3_m2_ysm, beta_hat_O3_W2_NOCOVID_2020_pvalue_plot,
        nlevels  = 5, add = TRUE, col = grey(0.1)) 







### SO2
FF_W2_SO2_m1 <- pffr(SO2_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(SO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12), yind = 1:12)
FF_W2_SO2_m2 <- pffr(SO2_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(SO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                       ff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                       ff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                       ff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                       ff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                     yind = 1:12)

FF_W2_SO2_m1_sum <- summary(FF_W2_SO2_m1)
FF_W2_SO2_m2_sum <- summary(FF_W2_SO2_m2)
FF_W2_SO2_AIC_Tables <- data.frame(m1_AIC = AIC(FF_W2_SO2_m1),m1_BIC = BIC(FF_W2_SO2_m1),m1_rsq = FF_W2_SO2_m1_sum$r.sq,
                                   m2_AIC = AIC(FF_W2_SO2_m2),m2_BIC = BIC(FF_W2_SO2_m2),m2_rsq = FF_W2_SO2_m2_sum$r.sq)

FF_W2_SO2_m1_OBS_Fitted <- rowMeans(SO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_SO2_m1))
FF_W2_SO2_m2_OBS_Fitted <- rowMeans(SO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_SO2_m2))
plot(x=FF_W2_SO2_m1_OBS_Fitted,y=FF_W2_SO2_m2_OBS_Fitted, xlab="Model 1",ylab="Model 2")


#Extract coefficients
FF_W2_SO2_m2_allcoef <- coef(FF_W2_SO2_m2)
## using seWithMean for  s(yind.vec) .

#Extract the smooth coefficients. They are stored in a vector, but they are then transformed into a matrix 
FF_W2_SO2_m2_smcoef <- FF_W2_SO2_m2_allcoef$smterms$`ff(SO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$value

#Extract the predictor functional arguments
FF_W2_SO2_m2_xsm <- FF_W2_SO2_m2_allcoef$smterms$`ff(SO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$x

#Extract the outcome functional arguments
FF_W2_SO2_m2_ysm <- FF_W2_SO2_m2_allcoef$smterms$`ff(SO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$y

#Transform the smooth coefficients into a matrix to prepare for plotting
FF_W2_SO2_m2_smcoef_plot <- matrix(FF_W2_SO2_m2_smcoef, nrow=length(FF_W2_SO2_m2_xsm))

#Use image.plot in the fields package to display the smooth coefficient
#par(mfrow = c(1, 2), mar = c(5, 5, 5, 5))

#Extract therelevanttermfromthefittedobject
beta_hat_SO2_W2_NOCOVID_2020 <-FF_W2_SO2_m2_allcoef$smterms$`ff(SO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$coef
#Create 95%CIandgetp-values
beta_hat_SO2_W2_NOCOVID_2020 <-
  beta_hat_SO2_W2_NOCOVID_2020 %>%
  mutate(LB =value - qnorm(0.975) * se,
         UB =value + qnorm(0.975) * se,
         p_val =2 * pnorm(abs(value / se),lower.tail= FALSE))

beta_hat_SO2_W2_NOCOVID_2020_pvalue_plot <- matrix(beta_hat_SO2_W2_NOCOVID_2020$p_val, nrow=length(FF_W2_SO2_m2_xsm))

# image.plot(FF_W2_SO2_m2_xsm, FF_W2_SO2_m2_ysm, beta_hat_SO2_W2_NOCOVID_2020_pvalue_plot,
#            xlab = "SO2 (Year 2019) ", 
#            ylab = "SO2 (Year 2020) ", 
#            main = "SO2 Lock down wave 1 2020 on CO1 2019",
#            # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
#            #  legend.shrink = 0.8,
#            #  legend.line = -1.5, 
#            legend.width = 0.5,
#            col =  viridis(3), 
#            breaks = c(0,0.05,0.10,1) )


image.plot(FF_W2_SO2_m2_xsm, FF_W2_SO2_m2_ysm, FF_W2_SO2_m2_smcoef_plot,
           xlab = "SO2 (Year 2020) ", 
           ylab = "SO2 (Year 2021) ", 
           main = "SO2 Lock down wave 2 2021 on SO2 2020",
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W2_SO2_m2_xsm, FF_W2_SO2_m2_ysm, beta_hat_SO2_W2_NOCOVID_2020_pvalue_plot,
        nlevels  = 5, add = TRUE, col = grey(0.1)) 





### AEI
FF_W2_AEI_m1 <- pffr(AEI_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(AEI_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12), yind = 1:12)
FF_W2_AEI_m2 <- pffr(AEI_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(AEI_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                       ff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                       ff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                       ff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                       ff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                     yind = 1:12)

FF_W2_AEI_m1_sum <- summary(FF_W2_AEI_m1)
FF_W2_AEI_m2_sum <- summary(FF_W2_AEI_m2)
FF_W2_AEI_AIC_Tables <- data.frame(m1_AIC = AIC(FF_W2_AEI_m1),m1_BIC = BIC(FF_W2_AEI_m1),m1_rsq = FF_W2_AEI_m1_sum$r.sq,
                                   m2_AIC = AIC(FF_W2_AEI_m2),m2_BIC = BIC(FF_W2_AEI_m2),m2_rsq = FF_W2_AEI_m2_sum$r.sq)

FF_W2_AEI_m1_OBS_Fitted <- rowMeans(AEI_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_AEI_m1))
FF_W2_AEI_m2_OBS_Fitted <- rowMeans(AEI_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_AEI_m2))
plot(x=FF_W2_AEI_m1_OBS_Fitted,y=FF_W2_AEI_m2_OBS_Fitted, xlab="Model 1",ylab="Model 2")


#Extract coefficients
FF_W2_AEI_m2_allcoef <- coef(FF_W2_AEI_m2)
## using seWithMean for  s(yind.vec) .

#Extract the smooth coefficients. They are stored in a vector, but they are then transformed into a matrix 
FF_W2_AEI_m2_smcoef <- FF_W2_AEI_m2_allcoef$smterms$`ff(AEI_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$value

#Extract the predictor functional arguments
FF_W2_AEI_m2_xsm <- FF_W2_AEI_m2_allcoef$smterms$`ff(AEI_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$x

#Extract the outcome functional arguments
FF_W2_AEI_m2_ysm <- FF_W2_AEI_m2_allcoef$smterms$`ff(AEI_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$y

#Transform the smooth coefficients into a matrix to prepare for plotting
FF_W2_AEI_m2_smcoef_plot <- matrix(FF_W2_AEI_m2_smcoef, nrow=length(FF_W2_AEI_m2_xsm))

#Use image.plot in the fields package to display the smooth coefficient
#par(mfrow = c(1, 2), mar = c(5, 5, 5, 5))

#Extract therelevanttermfromthefittedobject
beta_hat_AEI_W2_NOCOVID_2020 <-FF_W2_AEI_m2_allcoef$smterms$`ff(AEI_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$coef
#Create 95%CIandgetp-values
beta_hat_AEI_W2_NOCOVID_2020 <-
  beta_hat_AEI_W2_NOCOVID_2020 %>%
  mutate(LB =value - qnorm(0.975) * se,
         UB =value + qnorm(0.975) * se,
         p_val =2 * pnorm(abs(value / se),lower.tail= FALSE))

beta_hat_AEI_W2_NOCOVID_2020_pvalue_plot <- matrix(beta_hat_AEI_W2_NOCOVID_2020$p_val, nrow=length(FF_W2_AEI_m2_xsm))

# image.plot(FF_W2_AEI_m2_xsm, FF_W2_AEI_m2_ysm, beta_hat_AEI_W2_NOCOVID_2020_pvalue_plot,
#            xlab = "AEI (Year 2019) ", 
#            ylab = "AEI (Year 2020) ", 
#            main = "AEI Lock down wave 1 2020 on CO1 2019",
#            # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
#            #  legend.shrink = 0.8,
#            #  legend.line = -1.5, 
#            legend.width = 0.5,
#            col =  viridis(3), 
#            breaks = c(0,0.05,0.10,1) )


image.plot(FF_W2_AEI_m2_xsm, FF_W2_AEI_m2_ysm, FF_W2_AEI_m2_smcoef_plot,
           xlab = "AEI (Year 2020) ", 
           ylab = "AEI (Year 2021) ", 
           main = "AEI Lock down wave 2 2021 on AEI 2020",
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W2_AEI_m2_xsm, FF_W2_AEI_m2_ysm, beta_hat_AEI_W2_NOCOVID_2020_pvalue_plot,
        nlevels  = 5, add = TRUE, col = grey(0.1)) 



### HCHO
FF_W2_HCHO_m1 <- pffr(HCHO_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(HCHO_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12), yind = 1:12)
FF_W2_HCHO_m2 <- pffr(HCHO_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(HCHO_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                        ff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                        ff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                        ff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                        ff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                      yind = 1:12)

FF_W2_HCHO_m1_sum <- summary(FF_W2_HCHO_m1)
FF_W2_HCHO_m2_sum <- summary(FF_W2_HCHO_m2)
FF_W2_HCHO_AIC_Tables <- data.frame(m1_AIC = AIC(FF_W2_HCHO_m1),m1_BIC = BIC(FF_W2_HCHO_m1),m1_rsq = FF_W2_HCHO_m1_sum$r.sq,
                                    m2_AIC = AIC(FF_W2_HCHO_m2),m2_BIC = BIC(FF_W2_HCHO_m2),m2_rsq = FF_W2_HCHO_m2_sum$r.sq)

FF_W2_HCHO_m1_OBS_Fitted <- rowMeans(HCHO_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_HCHO_m1))
FF_W2_HCHO_m2_OBS_Fitted <- rowMeans(HCHO_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_HCHO_m2))
plot(x=FF_W2_HCHO_m1_OBS_Fitted,y=FF_W2_HCHO_m2_OBS_Fitted, xlab="Model 1",ylab="Model 2")


#Extract coefficients
FF_W2_HCHO_m2_allcoef <- coef(FF_W2_HCHO_m2)
## using seWithMean for  s(yind.vec) .

#Extract the smooth coefficients. They are stored in a vector, but they are then transformed into a matrix 
FF_W2_HCHO_m2_smcoef <- FF_W2_HCHO_m2_allcoef$smterms$`ff(HCHO_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$value

#Extract the predictor functional arguments
FF_W2_HCHO_m2_xsm <- FF_W2_HCHO_m2_allcoef$smterms$`ff(HCHO_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$x

#Extract the outcome functional arguments
FF_W2_HCHO_m2_ysm <- FF_W2_HCHO_m2_allcoef$smterms$`ff(HCHO_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$y

#Transform the smooth coefficients into a matrix to prepare for plotting
FF_W2_HCHO_m2_smcoef_plot <- matrix(FF_W2_HCHO_m2_smcoef, nrow=length(FF_W2_HCHO_m2_xsm))

#Use image.plot in the fields package to display the smooth coefficient
#par(mfrow = c(1, 2), mar = c(5, 5, 5, 5))

#Extract therelevanttermfromthefittedobject
beta_hat_HCHO_W2_NOCOVID_2020 <-FF_W2_HCHO_m2_allcoef$smterms$`ff(HCHO_W2_NOCOVID_2020_DS_FCPA_RC_MEAN,1:12)`$coef
#Create 95%CIandgetp-values
beta_hat_HCHO_W2_NOCOVID_2020 <-
  beta_hat_HCHO_W2_NOCOVID_2020 %>%
  mutate(LB =value - qnorm(0.975) * se,
         UB =value + qnorm(0.975) * se,
         p_val =2 * pnorm(abs(value / se),lower.tail= FALSE))

beta_hat_HCHO_W2_NOCOVID_2020_pvalue_plot <- matrix(beta_hat_HCHO_W2_NOCOVID_2020$p_val, nrow=length(FF_W2_HCHO_m2_xsm))

# image.plot(FF_W2_HCHO_m2_xsm, FF_W2_HCHO_m2_ysm, beta_hat_HCHO_W2_NOCOVID_2020_pvalue_plot,
#            xlab = "HCHO (Year 2019) ", 
#            ylab = "HCHO (Year 2020) ", 
#            main = "HCHO Lock down wave 1 2020 on CO1 2019",
#            # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
#            #  legend.shrink = 0.8,
#            #  legend.line = -1.5, 
#            legend.width = 0.5,
#            col =  viridis(3), 
#            breaks = c(0,0.05,0.10,1) )


image.plot(FF_W2_HCHO_m2_xsm, FF_W2_HCHO_m2_ysm, FF_W2_HCHO_m2_smcoef_plot,
           xlab = "HCHO (Year 2020) ", 
           ylab = "HCHO (Year 2021) ", 
           main = "HCHO Lock down wave 2 2021 on HCHO 2020",
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W2_HCHO_m2_xsm, FF_W2_HCHO_m2_ysm, beta_hat_HCHO_W2_NOCOVID_2020_pvalue_plot,
        levels  = c(0.0001, 0.001,0.01,0.05,0.10,0.20,0.3,0.4), add = TRUE, col = grey(0.1)) 




############################
############################
############################
############## Maps
############################
############################
############################


FF_W1 <- data.frame(PRV  = unique(CO1_W1_NOCOVID_2019_DS$subj),
                    W1_CO1  = FF_W1_CO1_m2_OBS_Fitted,
                    W1_CO2  = FF_W1_CO2_m2_OBS_Fitted,
                    W1_NO2  = FF_W1_NO2_m2_OBS_Fitted,
                    W1_O3   = FF_W1_O3_m2_OBS_Fitted,
                    W1_SO2  = FF_W1_SO2_m2_OBS_Fitted,
                    W1_AEI  = FF_W1_AEI_m2_OBS_Fitted,
                    W1_HCHO = FF_W1_HCHO_m2_OBS_Fitted)



FF_W2 <- data.frame(PRV  = unique(CO1_W1_NOCOVID_2019_DS$subj),
                    W2_CO1  = FF_W2_CO1_m2_OBS_Fitted,
                    W2_CO2  = FF_W2_CO2_m2_OBS_Fitted,
                    W2_NO2  = FF_W2_NO2_m2_OBS_Fitted,
                    W2_O3   = FF_W2_O3_m2_OBS_Fitted,
                    W2_SO2  = FF_W2_SO2_m2_OBS_Fitted,
                    W2_AEI  = FF_W2_AEI_m2_OBS_Fitted,
                    W2_HCHO = FF_W2_HCHO_m2_OBS_Fitted)

Iran_names <- c("Tehran", "Zanjan","Yazd" ,"West Azerbaijan","Markazi","Sistan and Baluchestan" ,
                "Semnan","Qom" ,"Qazvin","Mazandaran","Kurdistan" ,"Lorestan"  ,"Kohgiluyeh and Boyer-Ahmad",
                "Khuzestan" ,"South Khorasan" ,"Razavi Khorasan", "North Khorasan","Kermanshah" ,"Kerman",
                "Ilam","Hormozgan","Hamadan" ,"Golestan" ,"Gilan" ,"Fars" ,"Isfahan"  ,"East Azerbaijan"  ,
                "Chaharmahal and Bakhtiari", "Bushehr", "Ardabil" ,"Alborz")
                

FF_Fitted_Values <- cbind(ADM1_EN=Iran_names, FF_W1,FF_W2)
names(FF_Fitted_Values)

library(sf)
library(ggplot2)

iran <- st_read("D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Maps\\irn_adm_unhcr_20190514_shp\\irn_admbnda_adm1_unhcr_20190514.shp")
iran$W1_CO1  <- FF_Fitted_Values$W1_CO1[match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W1_CO2  <- FF_Fitted_Values$W1_CO2[match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W1_NO2  <- FF_Fitted_Values$W1_NO2[match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W1_O3   <- FF_Fitted_Values$W1_O3[match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W1_SO2  <- FF_Fitted_Values$W1_SO2[match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W1_AEI  <- FF_Fitted_Values$W1_AEI[match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W1_HCHO <- FF_Fitted_Values$W1_HCHO[match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]

iran$W2_CO1  <- FF_Fitted_Values$W2_CO1 [match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W2_CO2  <- FF_Fitted_Values$W2_CO2 [match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W2_NO2  <- FF_Fitted_Values$W2_NO2 [match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W2_O3   <- FF_Fitted_Values$W2_O3  [match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W2_SO2  <- FF_Fitted_Values$W2_SO2 [match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W2_AEI  <- FF_Fitted_Values$W2_AEI [match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W2_HCHO <- FF_Fitted_Values$W2_HCHO[match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]

library(scales) # for muted
library(ggpubr)

########## CO1 
W1_CO1 <- ggplot(iran) + 
             geom_sf(aes(fill = W1_CO1)) +
             geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
                  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(0,2), 
                       na.value = "orange",
                       name = "") +
  theme_void() 



W2_CO1 <- ggplot(iran) + 
  geom_sf(aes(fill = W2_CO1)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
    scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(0,2), 
                       na.value = "orange",
                       name = "") +
  theme_void() +
  guides(fill=guide_legend(title=""))

ggarrange_CO1 <- ggarrange(W1_CO1 , W2_CO1,
          labels = c("A", "B"),
          ncol = 2, nrow = 1,
          common.legend = TRUE, legend="bottom")

ggsave("D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_Maps\\ggarrange_CO1.png", 
       ggarrange_CO1,limitsize = FALSE, dpi = "print")



########## CO2 
W1_CO2 <- ggplot(iran) + 
  geom_sf(aes(fill = W1_CO2)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-26,100), 
                       na.value = "orange",
                       name = "") +
  theme_void() 



W2_CO2 <- ggplot(iran) + 
  geom_sf(aes(fill = W2_CO2)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-26,100), 
                       na.value = "orange",
                       name = "") +
  theme_void() +
  guides(fill=guide_legend(title=""))

ggarrange_CO2 <- ggarrange(W1_CO2 , W2_CO2,
                           labels = c("A", "B"),
                           ncol = 2, nrow = 1,
                           common.legend = TRUE, legend="bottom")

ggsave("D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_Maps\\ggarrange_CO2.png", 
       ggarrange_CO2,limitsize = FALSE, dpi = "print")




########## NO2 
W1_NO2 <- ggplot(iran) + 
  geom_sf(aes(fill = W1_NO2)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-30,110), 
                       na.value = "orange",
                       name = "") +
  theme_void() 



W2_NO2 <- ggplot(iran) + 
  geom_sf(aes(fill = W2_NO2)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-30,110), 
                       na.value = "orange",
                       name = "") +
  theme_void() +
  guides(fill=guide_legend(title=""))

ggarrange_NO2 <- ggarrange(W1_NO2 , W2_NO2,
                           labels = c("A", "B"),
                           ncol = 2, nrow = 1,
                           common.legend = TRUE, legend="bottom")

ggsave("D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_Maps\\ggarrange_NO2.png", 
       ggarrange_NO2,limitsize = FALSE, dpi = "print")




########## O3 
W1_O3 <- ggplot(iran) + 
  geom_sf(aes(fill = W1_O3)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-2,2), 
                       na.value = "orange",
                       name = "") +
  theme_void() 



W2_O3 <- ggplot(iran) + 
  geom_sf(aes(fill = W2_O3)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-2,2), 
                       na.value = "orange",
                       name = "") +
  theme_void() +
  guides(fill=guide_legend(title=""))

ggarrange_O3 <- ggarrange(W1_O3 , W2_O3,
                          labels = c("A", "B"),
                          ncol = 2, nrow = 1,
                          common.legend = TRUE, legend="bottom")

ggsave("D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_Maps\\ggarrange_O3.png", 
       ggarrange_O3,limitsize = FALSE, dpi = "print")




########## SO2 ()
W1_SO2 <- ggplot(iran) + 
  geom_sf(aes(fill = W1_SO2)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-30,30), 
                       na.value = "orange",
                       name = "") +
  theme_void() 



W2_SO2 <- ggplot(iran) + 
  geom_sf(aes(fill = W2_SO2)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-30,30), 
                       na.value = "orange",
                       name = "") +
  theme_void() +
  guides(fill=guide_legend(title=""))

ggarrange_SO2 <- ggarrange(W1_SO2 , W2_SO2,
                           labels = c("A", "B"),
                           ncol = 2, nrow = 1,
                           common.legend = TRUE, legend="bottom")

ggsave("D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_Maps\\ggarrange_SO2.png", 
       ggarrange_SO2,limitsize = FALSE, dpi = "print")




########## AEI 
W1_AEI <- ggplot(iran) + 
  geom_sf(aes(fill = W1_AEI)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-4,4), 
                       na.value = "orange",
                       name = "") +
  theme_void() 



W2_AEI <- ggplot(iran) + 
  geom_sf(aes(fill = W2_AEI)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-4,4), 
                       na.value = "orange",
                       name = "") +
  theme_void() +
  guides(fill=guide_legend(title=""))

ggarrange_AEI <- ggarrange(W1_AEI , W2_AEI,
                           labels = c("A", "B"),
                           ncol = 2, nrow = 1,
                           common.legend = TRUE, legend="bottom")

ggsave("D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_Maps\\ggarrange_AEI.png", 
       ggarrange_AEI,limitsize = FALSE, dpi = "print")



########## HCHO ()
W1_HCHO <- ggplot(iran) + 
  geom_sf(aes(fill = W1_HCHO)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-40,20), 
                       na.value = "orange",
                       name = "") +
  theme_void() 



W2_HCHO <- ggplot(iran) + 
  geom_sf(aes(fill = W2_HCHO)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-40,20), 
                       na.value = "orange",
                       name = "") +
  theme_void() +
  guides(fill=guide_legend(title=""))

ggarrange_HCHO <- ggarrange(W1_HCHO , W2_HCHO,
                            labels = c("A", "B"),
                            ncol = 2, nrow = 1,
                            common.legend = TRUE, legend="bottom")

ggsave("D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_Maps\\ggarrange_HCHO.png", 
       ggarrange_HCHO,limitsize = FALSE, dpi = "print")


###############################
###############################
#### Summary Statistics 
Tables_M1 <- rbind(FF_W1_CO1_m1_sum$s.table,
                   FF_W1_CO2_m1_sum$s.table,
                   FF_W1_NO2_m1_sum$s.table,
                   FF_W1_O3_m1_sum$s.table,
                   FF_W1_SO2_m1_sum$s.table,
                   FF_W1_AEI_m1_sum$s.table,
                   FF_W1_HCHO_m1_sum$s.table,
                   FF_W2_CO1_m1_sum$s.table,
                   FF_W2_CO2_m1_sum$s.table,
                   FF_W2_NO2_m1_sum$s.table,
                   FF_W2_O3_m1_sum$s.table,
                   FF_W2_SO2_m1_sum$s.table,
                   FF_W2_AEI_m1_sum$s.table,
                   FF_W2_HCHO_m1_sum$s.table)

write.csv(Tables_M1,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_Tables\\ANOVA_M1.csv")




Tables_M2 <- rbind(FF_W1_CO1_m2_sum$s.table,
                FF_W1_CO2_m2_sum$s.table,
                FF_W1_NO2_m2_sum$s.table,
                FF_W1_O3_m2_sum$s.table,
                FF_W1_SO2_m2_sum$s.table,
                FF_W1_AEI_m2_sum$s.table,
                FF_W1_HCHO_m2_sum$s.table,
                FF_W2_CO1_m2_sum$s.table,
                FF_W2_CO2_m2_sum$s.table,
                FF_W2_NO2_m2_sum$s.table,
                FF_W2_O3_m2_sum$s.table,
                FF_W2_SO2_m2_sum$s.table,
                FF_W2_AEI_m2_sum$s.table,
                FF_W2_HCHO_m2_sum$s.table)

write.csv(Tables_M2,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_Tables\\ANOVA_M2.csv")


AIC_Tables <-rbind( FF_W1_CO1_AIC_Tables,
                    FF_W1_CO2_AIC_Tables,
                    FF_W1_NO2_AIC_Tables,
                    FF_W1_O3_AIC_Tables,
                    FF_W1_SO2_AIC_Tables,
                    FF_W1_AEI_AIC_Tables,
                    FF_W1_HCHO_AIC_Tables,
                    FF_W2_CO1_AIC_Tables,
                    FF_W2_CO2_AIC_Tables,
                    FF_W2_NO2_AIC_Tables,
                    FF_W2_O3_AIC_Tables,
                    FF_W2_SO2_AIC_Tables,
                    FF_W2_AEI_AIC_Tables,
                    FF_W2_HCHO_AIC_Tables)
AIC_Tables$Waves <- c(rep("wave 1",7),rep("wave 2",7))
AIC_Tables$GASes <- c(rep(c("CO1","CO2","NO2","O3","SO2","AEI","CHO"),2))

write.csv(AIC_Tables,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_Tables\\AIC_Tables.csv")


############# Plot Beta hat
############ Wave 1
######## CO1
par(mfrow=c(2,4))

image.plot(FF_W1_CO1_m2_xsm, FF_W1_CO1_m2_ysm, FF_W1_CO1_m2_smcoef_plot,
           xlab = "CO from 14-03-2019 to 20-04-2019 ", 
           ylab = "CO from 14-03-2020 to 20-04-2020 ", 
           main = "CO COVID Year on Previous Year",
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W1_CO1_m2_xsm, FF_W1_CO1_m2_ysm, beta_hat_CO1_W1_NOCOVID_2019_pvalue_plot,
        nlevels  = 5, add = TRUE, col = grey(0.1)) 

######## CO2

image.plot(FF_W1_CO2_m2_xsm, FF_W1_CO2_m2_ysm, FF_W1_CO2_m2_smcoef_plot,
           xlab = expression(H[2]*O ~ "from 14-03-2019 to 20-04-2019"), 
           ylab = expression(H[2]*O ~ "from 14-03-2020 to 20-04-2020"), 
           main = expression(H[2]*O ~ "COVID Year on Previous Year"),
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W1_CO2_m2_xsm, FF_W1_CO2_m2_ysm, beta_hat_CO2_W1_NOCOVID_2019_pvalue_plot,
        levels  = c(0.01,0.05,0.10,0.20), add = TRUE, col = grey(0.1)) 

######## NO2

image.plot(FF_W1_NO2_m2_xsm, FF_W1_NO2_m2_ysm, FF_W1_NO2_m2_smcoef_plot,
           xlab = expression(NO[2] ~ "from 14-03-2019 to 20-04-2019"), 
           ylab = expression(NO[2] ~ "from 14-03-2020 to 20-04-2020"), 
           main = expression(NO[2] ~ "COVID Year on Previous Year"),
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W1_NO2_m2_xsm, FF_W1_NO2_m2_ysm, beta_hat_NO2_W1_NOCOVID_2019_pvalue_plot,
        levels  = c(0.001,0.01,0.05,0.10,0.20), add = TRUE, col = grey(0.1)) 

######## O3
image.plot(FF_W1_O3_m2_xsm, FF_W1_O3_m2_ysm, FF_W1_O3_m2_smcoef_plot,
           xlab = expression(O[3] ~ "from 14-03-2019 to 20-04-2019"), 
           ylab = expression(O[3] ~ "from 14-03-2020 to 20-04-2020"), 
           main = expression(O[3] ~ "COVID Year on Previous Year"),
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W1_O3_m2_xsm, FF_W1_O3_m2_ysm, beta_hat_O3_W1_NOCOVID_2019_pvalue_plot,
        nlevels  = 5, add = TRUE, col = grey(0.1)) 


######## SO2
image.plot(FF_W1_SO2_m2_xsm, FF_W1_SO2_m2_ysm, FF_W1_SO2_m2_smcoef_plot,
           xlab = expression(SO[2] ~ "from 14-03-2019 to 20-04-2019"), 
           ylab = expression(SO[2] ~ "from 14-03-2020 to 20-04-2020"), 
           main = expression(SO[2] ~ "COVID Year on Previous Year"),           
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W1_SO2_m2_xsm, FF_W1_SO2_m2_ysm, beta_hat_SO2_W1_NOCOVID_2019_pvalue_plot,
        nlevels  = 5, add = TRUE, col = grey(0.1)) 



######## AEI


image.plot(FF_W1_AEI_m2_xsm, FF_W1_AEI_m2_ysm, FF_W1_AEI_m2_smcoef_plot,
           xlab = "AEI from 14-03-2019 to 20-04-2019", 
           ylab = "AEI from 14-03-2020 to 20-04-2020", 
           main = "AEI COVID Year on Previous Year",           
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W1_AEI_m2_xsm, FF_W1_AEI_m2_ysm, beta_hat_AEI_W1_NOCOVID_2019_pvalue_plot,
        nlevels  = 5, add = TRUE, col = grey(0.1)) 



######## HCHO

image.plot(FF_W1_HCHO_m2_xsm, FF_W1_HCHO_m2_ysm, FF_W1_HCHO_m2_smcoef_plot,2,
           xlab = "HCHO from 14-03-2019 to 20-04-2019", 
           ylab = "HCHO from 14-03-2020 to 20-04-2020", 
           main = "HCHO COVID Year on Previous Year",           
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
            legend.shrink = 0.8,
           #  legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W1_HCHO_m2_xsm, FF_W1_HCHO_m2_ysm, beta_hat_HCHO_W1_NOCOVID_2019_pvalue_plot,
        nlevels  = 5, add = TRUE, col = grey(0.1)) 





############ Wave 2
######## CO1
#png("D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_Betas\\02_wave2.png",width = 2500, height = 2000)
par(mfrow=c(2,4))

image.plot(FF_W2_CO1_m2_xsm, FF_W2_CO1_m2_ysm, FF_W2_CO1_m2_smcoef_plot,
           xlab = "CO from 13-04-2020 to 25-04-2020 ", 
           ylab = "CO from 13-04-2021 to 25-04-2021 ", 
           main = "CO COVID Year on Previous Year",
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W2_CO1_m2_xsm, FF_W2_CO1_m2_ysm, beta_hat_CO1_W2_NOCOVID_2020_pvalue_plot,
        nlevels  = 5, add = TRUE, col = grey(0.1)) 

######## CO2

image.plot(FF_W2_CO2_m2_xsm, FF_W2_CO2_m2_ysm, FF_W2_CO2_m2_smcoef_plot,
           xlab = expression(H[2]*O ~ "from 13-04-2020 to 25-04-2020"), 
           ylab = expression(H[2]*O ~ "from 13-04-2020 to 25-04-2020"), 
           main = expression(H[2]*O ~ "COVID Year on Previous Year"),
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W2_CO2_m2_xsm, FF_W2_CO2_m2_ysm, beta_hat_CO2_W2_NOCOVID_2020_pvalue_plot,
        levels  = c(0.01,0.05,0.10,0.20), add = TRUE, col = grey(0.1)) 

######## NO2

image.plot(FF_W2_NO2_m2_xsm, FF_W2_NO2_m2_ysm, FF_W2_NO2_m2_smcoef_plot,
           xlab = expression(NO[2] ~ "from 13-04-2020 to 25-04-2020"), 
           ylab = expression(NO[2] ~ "from 13-04-2020 to 25-04-2020"), 
           main = expression(NO[2] ~ "COVID Year on Previous Year"),
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W2_NO2_m2_xsm, FF_W2_NO2_m2_ysm, beta_hat_NO2_W2_NOCOVID_2020_pvalue_plot,
        levels  = c(0.001,0.01,0.05,0.10,0.20), add = TRUE, col = grey(0.1)) 

######## O3
image.plot(FF_W2_O3_m2_xsm, FF_W2_O3_m2_ysm, FF_W2_O3_m2_smcoef_plot,
           xlab = expression(O[3] ~ "from 13-04-2020 to 25-04-2020"), 
           ylab = expression(O[3] ~ "from 13-04-2020 to 25-04-2020"), 
           main = expression(O[3] ~ "COVID Year on Previous Year"),
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W2_O3_m2_xsm, FF_W2_O3_m2_ysm, beta_hat_O3_W2_NOCOVID_2020_pvalue_plot,
        nlevels  = 5, add = TRUE, col = grey(0.1)) 


######## SO2
image.plot(FF_W2_SO2_m2_xsm, FF_W2_SO2_m2_ysm, FF_W2_SO2_m2_smcoef_plot,
           xlab = expression(SO[2] ~ "from 13-04-2020 to 25-04-2020"), 
           ylab = expression(SO[2] ~ "from 13-04-2020 to 25-04-2020"), 
           main = expression(SO[2] ~ "COVID Year on Previous Year"),           
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W2_SO2_m2_xsm, FF_W2_SO2_m2_ysm, beta_hat_SO2_W2_NOCOVID_2020_pvalue_plot,
        nlevels  = 5, add = TRUE, col = grey(0.1)) 



######## AEI


image.plot(FF_W2_AEI_m2_xsm, FF_W2_AEI_m2_ysm, FF_W2_AEI_m2_smcoef_plot,
           xlab = "AEI from 13-04-2020 to 25-04-2020", 
           ylab = "AEI from 13-04-2020 to 25-04-2020", 
           main = "AEI COVID Year on Previous Year",           
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           # legend.shrink = 0.8,
           # legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W2_AEI_m2_xsm, FF_W2_AEI_m2_ysm, beta_hat_AEI_W2_NOCOVID_2020_pvalue_plot,
        nlevels  = 5, add = TRUE, col = grey(0.1)) 



######## HCHO

image.plot(FF_W2_HCHO_m2_xsm, FF_W2_HCHO_m2_ysm, FF_W2_HCHO_m2_smcoef_plot,2,
           xlab = "HCHO from 13-04-2020 to 25-04-2020", 
           ylab = "HCHO from 13-04-2020 to 25-04-2020", 
           main = "HCHO COVID Year on Previous Year",           
           # axis.args = list(at = c(-0.1,0.0,0.1,0.2,0.3)),
           legend.shrink = 0.8,
           #  legend.line = -1.5, 
           legend.width = 0.5)

contour(FF_W2_HCHO_m2_xsm, FF_W2_HCHO_m2_ysm, beta_hat_HCHO_W2_NOCOVID_2020_pvalue_plot,
        nlevels  = 5, add = TRUE, col = grey(0.1)) 

#dev.off()





################### FF Comparions
################# Wave 1
######## CO1
FF_W1_CO1_m2_t1 <- pffr(CO1_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(CO1_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                          ff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                          ff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                        yind = 1:37)


FF_W1_CO1_m2_t2 <- pffr(CO1_W1_COVID_2020_DS_FCPA_RC_MEAN ~ sff(CO1_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          sff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                          sff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                          sff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          sff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                        yind = 1:37)

FF_W1_CO1_m2_t3 <- pffr(CO1_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ffpc(CO1_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ffpc(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                          ffpc(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                          ffpc(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ffpc(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                        yind = 1:37)



FF_W1_CO1_m2_t1_sum <- summary(FF_W1_CO1_m2_t1)
FF_W1_CO1_m2_t2_sum <- summary(FF_W1_CO1_m2_t2)
FF_W1_CO1_m2_t3_sum <- summary(FF_W1_CO1_m2_t3)

FF_W1_CO1_AIC_Tables <- data.frame(m1_t1_rsq = FF_W1_CO1_m2_t1_sum$r.sq, m1_t1_AIC = AIC(FF_W1_CO1_m2_t1),m1_t1_BIC = BIC(FF_W1_CO1_m2_t1),
                                   m1_t2_rsq = FF_W1_CO1_m2_t2_sum$r.sq, m1_t2_AIC = AIC(FF_W1_CO1_m2_t2),m1_t2_BIC = BIC(FF_W1_CO1_m2_t2),
                                   m1_t3_rsq = FF_W1_CO1_m2_t3_sum$r.sq, m1_t3_AIC = AIC(FF_W1_CO1_m2_t3),m1_t3_BIC = BIC(FF_W1_CO1_m2_t3))


FF_W1_CO1_m2_t1_ANOVA <- data.frame(method = "ff"   , FF_W1_CO1_m2_t1_sum$s.table)
FF_W1_CO1_m2_t2_ANOVA <- data.frame(method = "sff"  , FF_W1_CO1_m2_t2_sum$s.table)
FF_W1_CO1_m2_t3_ANOVA <- data.frame(method = "ffpc" , FF_W1_CO1_m2_t3_sum$s.table)
FF_W1_CO1_m2_ANOVA    <- rbind(FF_W1_CO1_m2_t1_ANOVA,FF_W1_CO1_m2_t2_ANOVA,FF_W1_CO1_m2_t3_ANOVA)


write.csv(FF_W1_CO1_AIC_Tables,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\CO1\\FF_W1_CO1_AIC_Tables.csv")

write.csv(FF_W1_CO1_m2_ANOVA,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\CO1\\FF_W1_CO1_m2_ANOVA.csv")


######## CO2
FF_W1_CO2_m2_t1 <- pffr(CO2_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(CO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                          ff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                          ff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                        yind = 1:37)


FF_W1_CO2_m2_t2 <- pffr(CO2_W1_COVID_2020_DS_FCPA_RC_MEAN ~ sff(CO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          sff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                          sff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                          sff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          sff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                        yind = 1:37)

FF_W1_CO2_m2_t3 <- pffr(CO2_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ffpc(CO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ffpc(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                          ffpc(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                          ffpc(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ffpc(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                        yind = 1:37)



FF_W1_CO2_m2_t1_sum <- summary(FF_W1_CO2_m2_t1)
FF_W1_CO2_m2_t2_sum <- summary(FF_W1_CO2_m2_t2)
FF_W1_CO2_m2_t3_sum <- summary(FF_W1_CO2_m2_t3)

FF_W1_CO2_AIC_Tables <- data.frame(m1_t1_rsq = FF_W1_CO2_m2_t1_sum$r.sq, m1_t1_AIC = AIC(FF_W1_CO2_m2_t1),m1_t1_BIC = BIC(FF_W1_CO2_m2_t1),
                                   m1_t2_rsq = FF_W1_CO2_m2_t2_sum$r.sq, m1_t2_AIC = AIC(FF_W1_CO2_m2_t2),m1_t2_BIC = BIC(FF_W1_CO2_m2_t2),
                                   m1_t3_rsq = FF_W1_CO2_m2_t3_sum$r.sq, m1_t3_AIC = AIC(FF_W1_CO2_m2_t3),m1_t3_BIC = BIC(FF_W1_CO2_m2_t3))


FF_W1_CO2_m2_t1_ANOVA <- data.frame(method = "ff"   , FF_W1_CO2_m2_t1_sum$s.table)
FF_W1_CO2_m2_t2_ANOVA <- data.frame(method = "sff"  , FF_W1_CO2_m2_t2_sum$s.table)
FF_W1_CO2_m2_t3_ANOVA <- data.frame(method = "ffpc" , FF_W1_CO2_m2_t3_sum$s.table)
FF_W1_CO2_m2_ANOVA    <- rbind(FF_W1_CO2_m2_t1_ANOVA,FF_W1_CO2_m2_t2_ANOVA,FF_W1_CO2_m2_t3_ANOVA)


write.csv(FF_W1_CO2_AIC_Tables,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\CO2\\FF_W1_CO2_AIC_Tables.csv")

write.csv(FF_W1_CO2_m2_ANOVA,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\CO2\\FF_W1_CO2_m2_ANOVA.csv")

######## NO2
FF_W1_NO2_m2_t1 <- pffr(NO2_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(NO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                          ff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                          ff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                        yind = 1:37)


FF_W1_NO2_m2_t2 <- pffr(NO2_W1_COVID_2020_DS_FCPA_RC_MEAN ~ sff(NO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          sff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                          sff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                          sff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          sff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                        yind = 1:37)

FF_W1_NO2_m2_t3 <- pffr(NO2_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ffpc(NO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ffpc(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                          ffpc(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                          ffpc(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ffpc(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                        yind = 1:37)



FF_W1_NO2_m2_t1_sum <- summary(FF_W1_NO2_m2_t1)
FF_W1_NO2_m2_t2_sum <- summary(FF_W1_NO2_m2_t2)
FF_W1_NO2_m2_t3_sum <- summary(FF_W1_NO2_m2_t3)

FF_W1_NO2_AIC_Tables <- data.frame(m1_t1_rsq = FF_W1_NO2_m2_t1_sum$r.sq, m1_t1_AIC = AIC(FF_W1_NO2_m2_t1),m1_t1_BIC = BIC(FF_W1_NO2_m2_t1),
                                   m1_t2_rsq = FF_W1_NO2_m2_t2_sum$r.sq, m1_t2_AIC = AIC(FF_W1_NO2_m2_t2),m1_t2_BIC = BIC(FF_W1_NO2_m2_t2),
                                   m1_t3_rsq = FF_W1_NO2_m2_t3_sum$r.sq, m1_t3_AIC = AIC(FF_W1_NO2_m2_t3),m1_t3_BIC = BIC(FF_W1_NO2_m2_t3))


FF_W1_NO2_m2_t1_ANOVA <- data.frame(method = "ff"   , FF_W1_NO2_m2_t1_sum$s.table)
FF_W1_NO2_m2_t2_ANOVA <- data.frame(method = "sff"  , FF_W1_NO2_m2_t2_sum$s.table)
FF_W1_NO2_m2_t3_ANOVA <- data.frame(method = "ffpc" , FF_W1_NO2_m2_t3_sum$s.table)
FF_W1_NO2_m2_ANOVA    <- rbind(FF_W1_NO2_m2_t1_ANOVA,FF_W1_NO2_m2_t2_ANOVA,FF_W1_NO2_m2_t3_ANOVA)


write.csv(FF_W1_NO2_AIC_Tables,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\NO2\\FF_W1_NO2_AIC_Tables.csv")

write.csv(FF_W1_NO2_m2_ANOVA,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\NO2\\FF_W1_NO2_m2_ANOVA.csv")

######## O3
FF_W1_O3_m2_t1 <- pffr(O3_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(O3_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                         ff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                         ff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                         ff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                         ff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                       yind = 1:37)


FF_W1_O3_m2_t2 <- pffr(O3_W1_COVID_2020_DS_FCPA_RC_MEAN ~ sff(O3_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                         sff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                         sff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                         sff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                         sff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                       yind = 1:37)

FF_W1_O3_m2_t3 <- pffr(O3_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ffpc(O3_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                         ffpc(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                         ffpc(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                         ffpc(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                         ffpc(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                       yind = 1:37)



FF_W1_O3_m2_t1_sum <- summary(FF_W1_O3_m2_t1)
FF_W1_O3_m2_t2_sum <- summary(FF_W1_O3_m2_t2)
FF_W1_O3_m2_t3_sum <- summary(FF_W1_O3_m2_t3)

FF_W1_O3_AIC_Tables <- data.frame(m1_t1_rsq = FF_W1_O3_m2_t1_sum$r.sq, m1_t1_AIC = AIC(FF_W1_O3_m2_t1),m1_t1_BIC = BIC(FF_W1_O3_m2_t1),
                                  m1_t2_rsq = FF_W1_O3_m2_t2_sum$r.sq, m1_t2_AIC = AIC(FF_W1_O3_m2_t2),m1_t2_BIC = BIC(FF_W1_O3_m2_t2),
                                  m1_t3_rsq = FF_W1_O3_m2_t3_sum$r.sq, m1_t3_AIC = AIC(FF_W1_O3_m2_t3),m1_t3_BIC = BIC(FF_W1_O3_m2_t3))


FF_W1_O3_m2_t1_ANOVA <- data.frame(method = "ff"   , FF_W1_O3_m2_t1_sum$s.table)
FF_W1_O3_m2_t2_ANOVA <- data.frame(method = "sff"  , FF_W1_O3_m2_t2_sum$s.table)
FF_W1_O3_m2_t3_ANOVA <- data.frame(method = "ffpc" , FF_W1_O3_m2_t3_sum$s.table)
FF_W1_O3_m2_ANOVA    <- rbind(FF_W1_O3_m2_t1_ANOVA,FF_W1_O3_m2_t2_ANOVA,FF_W1_O3_m2_t3_ANOVA)


write.csv(FF_W1_O3_AIC_Tables,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\O3\\FF_W1_O3_AIC_Tables.csv")

write.csv(FF_W1_O3_m2_ANOVA,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\O3\\FF_W1_O3_m2_ANOVA.csv")

######## SO2
FF_W1_SO2_m2_t1 <- pffr(SO2_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(SO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                          ff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                          ff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                        yind = 1:37)


FF_W1_SO2_m2_t2 <- pffr(SO2_W1_COVID_2020_DS_FCPA_RC_MEAN ~ sff(SO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          sff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                          sff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                          sff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          sff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                        yind = 1:37)

FF_W1_SO2_m2_t3 <- pffr(SO2_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ffpc(SO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ffpc(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                          ffpc(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                          ffpc(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ffpc(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                        yind = 1:37)



FF_W1_SO2_m2_t1_sum <- summary(FF_W1_SO2_m2_t1)
FF_W1_SO2_m2_t2_sum <- summary(FF_W1_SO2_m2_t2)
FF_W1_SO2_m2_t3_sum <- summary(FF_W1_SO2_m2_t3)

FF_W1_SO2_AIC_Tables <- data.frame(m1_t1_rsq = FF_W1_SO2_m2_t1_sum$r.sq, m1_t1_AIC = AIC(FF_W1_SO2_m2_t1),m1_t1_BIC = BIC(FF_W1_SO2_m2_t1),
                                   m1_t2_rsq = FF_W1_SO2_m2_t2_sum$r.sq, m1_t2_AIC = AIC(FF_W1_SO2_m2_t2),m1_t2_BIC = BIC(FF_W1_SO2_m2_t2),
                                   m1_t3_rsq = FF_W1_SO2_m2_t3_sum$r.sq, m1_t3_AIC = AIC(FF_W1_SO2_m2_t3),m1_t3_BIC = BIC(FF_W1_SO2_m2_t3))


FF_W1_SO2_m2_t1_ANOVA <- data.frame(method = "ff"   , FF_W1_SO2_m2_t1_sum$s.table)
FF_W1_SO2_m2_t2_ANOVA <- data.frame(method = "sff"  , FF_W1_SO2_m2_t2_sum$s.table)
FF_W1_SO2_m2_t3_ANOVA <- data.frame(method = "ffpc" , FF_W1_SO2_m2_t3_sum$s.table)
FF_W1_SO2_m2_ANOVA    <- rbind(FF_W1_SO2_m2_t1_ANOVA,FF_W1_SO2_m2_t2_ANOVA,FF_W1_SO2_m2_t3_ANOVA)


write.csv(FF_W1_SO2_AIC_Tables,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\SO2\\FF_W1_SO2_AIC_Tables.csv")

write.csv(FF_W1_SO2_m2_ANOVA,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\SO2\\FF_W1_SO2_m2_ANOVA.csv")

######## AEI
FF_W1_AEI_m2_t1 <- pffr(AEI_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(AEI_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                          ff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                          ff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                        yind = 1:37)


FF_W1_AEI_m2_t2 <- pffr(AEI_W1_COVID_2020_DS_FCPA_RC_MEAN ~ sff(AEI_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          sff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                          sff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                          sff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          sff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                        yind = 1:37)

FF_W1_AEI_m2_t3 <- pffr(AEI_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ffpc(AEI_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ffpc(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                          ffpc(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                          ffpc(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                          ffpc(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                        yind = 1:37)



FF_W1_AEI_m2_t1_sum <- summary(FF_W1_AEI_m2_t1)
FF_W1_AEI_m2_t2_sum <- summary(FF_W1_AEI_m2_t2)
FF_W1_AEI_m2_t3_sum <- summary(FF_W1_AEI_m2_t3)

FF_W1_AEI_AIC_Tables <- data.frame(m1_t1_rsq = FF_W1_AEI_m2_t1_sum$r.sq, m1_t1_AIC = AIC(FF_W1_AEI_m2_t1),m1_t1_BIC = BIC(FF_W1_AEI_m2_t1),
                                   m1_t2_rsq = FF_W1_AEI_m2_t2_sum$r.sq, m1_t2_AIC = AIC(FF_W1_AEI_m2_t2),m1_t2_BIC = BIC(FF_W1_AEI_m2_t2),
                                   m1_t3_rsq = FF_W1_AEI_m2_t3_sum$r.sq, m1_t3_AIC = AIC(FF_W1_AEI_m2_t3),m1_t3_BIC = BIC(FF_W1_AEI_m2_t3))


FF_W1_AEI_m2_t1_ANOVA <- data.frame(method = "ff"   , FF_W1_AEI_m2_t1_sum$s.table)
FF_W1_AEI_m2_t2_ANOVA <- data.frame(method = "sff"  , FF_W1_AEI_m2_t2_sum$s.table)
FF_W1_AEI_m2_t3_ANOVA <- data.frame(method = "ffpc" , FF_W1_AEI_m2_t3_sum$s.table)
FF_W1_AEI_m2_ANOVA    <- rbind(FF_W1_AEI_m2_t1_ANOVA,FF_W1_AEI_m2_t2_ANOVA,FF_W1_AEI_m2_t3_ANOVA)


write.csv(FF_W1_AEI_AIC_Tables,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\AEI\\FF_W1_AEI_AIC_Tables.csv")

write.csv(FF_W1_AEI_m2_ANOVA,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\AEI\\FF_W1_AEI_m2_ANOVA.csv")

######## HCHO
FF_W1_HCHO_m2_t1 <- pffr(HCHO_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ff(HCHO_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                           ff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                           ff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                           ff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                           ff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                         yind = 1:37)


FF_W1_HCHO_m2_t2 <- pffr(HCHO_W1_COVID_2020_DS_FCPA_RC_MEAN ~ sff(HCHO_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                           sff(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                           sff(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                           sff(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                           sff(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                         yind = 1:37)

FF_W1_HCHO_m2_t3 <- pffr(HCHO_W1_COVID_2020_DS_FCPA_RC_MEAN ~ ffpc(HCHO_W1_NOCOVID_2019_DS_FCPA_RC_MEAN            , xind = 1:37) +
                           ffpc(Pressure_W1_COVID_2020_DS_FCPA_RC_MEAN         , xind = 1:37) +
                           ffpc(TPrecipitation_W1_COVID_2020_DS_FCPA_RC_MEAN   , xind = 1:37) +
                           ffpc(MTemp_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) +
                           ffpc(UWind_W1_COVID_2020_DS_FCPA_RC_MEAN            , xind = 1:37) ,
                         yind = 1:37)



FF_W1_HCHO_m2_t1_sum <- summary(FF_W1_HCHO_m2_t1)
FF_W1_HCHO_m2_t2_sum <- summary(FF_W1_HCHO_m2_t2)
FF_W1_HCHO_m2_t3_sum <- summary(FF_W1_HCHO_m2_t3)

FF_W1_HCHO_AIC_Tables <- data.frame(m1_t1_rsq = FF_W1_HCHO_m2_t1_sum$r.sq, m1_t1_AIC = AIC(FF_W1_HCHO_m2_t1),m1_t1_BIC = BIC(FF_W1_HCHO_m2_t1),
                                    m1_t2_rsq = FF_W1_HCHO_m2_t2_sum$r.sq, m1_t2_AIC = AIC(FF_W1_HCHO_m2_t2),m1_t2_BIC = BIC(FF_W1_HCHO_m2_t2),
                                    m1_t3_rsq = FF_W1_HCHO_m2_t3_sum$r.sq, m1_t3_AIC = AIC(FF_W1_HCHO_m2_t3),m1_t3_BIC = BIC(FF_W1_HCHO_m2_t3))


FF_W1_HCHO_m2_t1_ANOVA <- data.frame(method = "ff"   , FF_W1_HCHO_m2_t1_sum$s.table)
FF_W1_HCHO_m2_t2_ANOVA <- data.frame(method = "sff"  , FF_W1_HCHO_m2_t2_sum$s.table)
FF_W1_HCHO_m2_t3_ANOVA <- data.frame(method = "ffpc" , FF_W1_HCHO_m2_t3_sum$s.table)
FF_W1_HCHO_m2_ANOVA    <- rbind(FF_W1_HCHO_m2_t1_ANOVA,FF_W1_HCHO_m2_t2_ANOVA,FF_W1_HCHO_m2_t3_ANOVA)


write.csv(FF_W1_HCHO_AIC_Tables,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\HCHO\\FF_W1_HCHO_AIC_Tables.csv")

write.csv(FF_W1_HCHO_m2_ANOVA,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\HCHO\\FF_W1_HCHO_m2_ANOVA.csv")



################# Wave 2
######## CO1
FF_W2_CO1_m2_t1 <- pffr(CO1_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(CO1_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                          ff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                          ff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                        yind = 1:12)


FF_W2_CO1_m2_t2 <- pffr(CO1_W2_COVID_2021_DS_FCPA_RC_MEAN ~ sff(CO1_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          sff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                          sff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                          sff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          sff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                        yind = 1:12)

FF_W2_CO1_m2_t3 <- pffr(CO1_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ffpc(CO1_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ffpc(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                          ffpc(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                          ffpc(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ffpc(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                        yind = 1:12)



FF_W2_CO1_m2_t1_sum <- summary(FF_W2_CO1_m2_t1)
FF_W2_CO1_m2_t2_sum <- summary(FF_W2_CO1_m2_t2)
FF_W2_CO1_m2_t3_sum <- summary(FF_W2_CO1_m2_t3)

FF_W2_CO1_AIC_Tables <- data.frame(m1_t1_rsq = FF_W2_CO1_m2_t1_sum$r.sq, m1_t1_AIC = AIC(FF_W2_CO1_m2_t1),m1_t1_BIC = BIC(FF_W2_CO1_m2_t1),
                                   m1_t2_rsq = FF_W2_CO1_m2_t2_sum$r.sq, m1_t2_AIC = AIC(FF_W2_CO1_m2_t2),m1_t2_BIC = BIC(FF_W2_CO1_m2_t2),
                                   m1_t3_rsq = FF_W2_CO1_m2_t3_sum$r.sq, m1_t3_AIC = AIC(FF_W2_CO1_m2_t3),m1_t3_BIC = BIC(FF_W2_CO1_m2_t3))


FF_W2_CO1_m2_t1_ANOVA <- data.frame(method = "ff"   , FF_W2_CO1_m2_t1_sum$s.table)
FF_W2_CO1_m2_t2_ANOVA <- data.frame(method = "sff"  , FF_W2_CO1_m2_t2_sum$s.table)
FF_W2_CO1_m2_t3_ANOVA <- data.frame(method = "ffpc" , FF_W2_CO1_m2_t3_sum$s.table)
FF_W2_CO1_m2_ANOVA    <- rbind(FF_W2_CO1_m2_t1_ANOVA,FF_W2_CO1_m2_t2_ANOVA,FF_W2_CO1_m2_t3_ANOVA)


write.csv(FF_W2_CO1_AIC_Tables,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\CO1\\FF_W2_CO1_AIC_Tables.csv")

write.csv(FF_W2_CO1_m2_ANOVA,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\CO1\\FF_W2_CO1_m2_ANOVA.csv")


######## CO2
FF_W2_CO2_m2_t1 <- pffr(CO2_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(CO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                          ff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                          ff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                        yind = 1:12)


FF_W2_CO2_m2_t2 <- pffr(CO2_W2_COVID_2021_DS_FCPA_RC_MEAN ~ sff(CO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          sff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                          sff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                          sff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          sff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                        yind = 1:12)

FF_W2_CO2_m2_t3 <- pffr(CO2_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ffpc(CO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ffpc(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                          ffpc(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                          ffpc(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ffpc(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                        yind = 1:12)



FF_W2_CO2_m2_t1_sum <- summary(FF_W2_CO2_m2_t1)
FF_W2_CO2_m2_t2_sum <- summary(FF_W2_CO2_m2_t2)
FF_W2_CO2_m2_t3_sum <- summary(FF_W2_CO2_m2_t3)

FF_W2_CO2_AIC_Tables <- data.frame(m1_t1_rsq = FF_W2_CO2_m2_t1_sum$r.sq, m1_t1_AIC = AIC(FF_W2_CO2_m2_t1),m1_t1_BIC = BIC(FF_W2_CO2_m2_t1),
                                   m1_t2_rsq = FF_W2_CO2_m2_t2_sum$r.sq, m1_t2_AIC = AIC(FF_W2_CO2_m2_t2),m1_t2_BIC = BIC(FF_W2_CO2_m2_t2),
                                   m1_t3_rsq = FF_W2_CO2_m2_t3_sum$r.sq, m1_t3_AIC = AIC(FF_W2_CO2_m2_t3),m1_t3_BIC = BIC(FF_W2_CO2_m2_t3))


FF_W2_CO2_m2_t1_ANOVA <- data.frame(method = "ff"   , FF_W2_CO2_m2_t1_sum$s.table)
FF_W2_CO2_m2_t2_ANOVA <- data.frame(method = "sff"  , FF_W2_CO2_m2_t2_sum$s.table)
FF_W2_CO2_m2_t3_ANOVA <- data.frame(method = "ffpc" , FF_W2_CO2_m2_t3_sum$s.table)
FF_W2_CO2_m2_ANOVA    <- rbind(FF_W2_CO2_m2_t1_ANOVA,FF_W2_CO2_m2_t2_ANOVA,FF_W2_CO2_m2_t3_ANOVA)


write.csv(FF_W2_CO2_AIC_Tables,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\CO2\\FF_W2_CO2_AIC_Tables.csv")

write.csv(FF_W2_CO2_m2_ANOVA,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\CO2\\FF_W2_CO2_m2_ANOVA.csv")

######## NO2
FF_W2_NO2_m2_t1 <- pffr(NO2_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(NO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                          ff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                          ff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                        yind = 1:12)


FF_W2_NO2_m2_t2 <- pffr(NO2_W2_COVID_2021_DS_FCPA_RC_MEAN ~ sff(NO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          sff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                          sff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                          sff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          sff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                        yind = 1:12)

FF_W2_NO2_m2_t3 <- pffr(NO2_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ffpc(NO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ffpc(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                          ffpc(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                          ffpc(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ffpc(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                        yind = 1:12)



FF_W2_NO2_m2_t1_sum <- summary(FF_W2_NO2_m2_t1)
FF_W2_NO2_m2_t2_sum <- summary(FF_W2_NO2_m2_t2)
FF_W2_NO2_m2_t3_sum <- summary(FF_W2_NO2_m2_t3)

FF_W2_NO2_AIC_Tables <- data.frame(m1_t1_rsq = FF_W2_NO2_m2_t1_sum$r.sq, m1_t1_AIC = AIC(FF_W2_NO2_m2_t1),m1_t1_BIC = BIC(FF_W2_NO2_m2_t1),
                                   m1_t2_rsq = FF_W2_NO2_m2_t2_sum$r.sq, m1_t2_AIC = AIC(FF_W2_NO2_m2_t2),m1_t2_BIC = BIC(FF_W2_NO2_m2_t2),
                                   m1_t3_rsq = FF_W2_NO2_m2_t3_sum$r.sq, m1_t3_AIC = AIC(FF_W2_NO2_m2_t3),m1_t3_BIC = BIC(FF_W2_NO2_m2_t3))


FF_W2_NO2_m2_t1_ANOVA <- data.frame(method = "ff"   , FF_W2_NO2_m2_t1_sum$s.table)
FF_W2_NO2_m2_t2_ANOVA <- data.frame(method = "sff"  , FF_W2_NO2_m2_t2_sum$s.table)
FF_W2_NO2_m2_t3_ANOVA <- data.frame(method = "ffpc" , FF_W2_NO2_m2_t3_sum$s.table)
FF_W2_NO2_m2_ANOVA    <- rbind(FF_W2_NO2_m2_t1_ANOVA,FF_W2_NO2_m2_t2_ANOVA,FF_W2_NO2_m2_t3_ANOVA)


write.csv(FF_W2_NO2_AIC_Tables,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\NO2\\FF_W2_NO2_AIC_Tables.csv")

write.csv(FF_W2_NO2_m2_ANOVA,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\NO2\\FF_W2_NO2_m2_ANOVA.csv")

######## O3
FF_W2_O3_m2_t1 <- pffr(O3_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(O3_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                         ff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                         ff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                         ff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                         ff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                       yind = 1:12)


FF_W2_O3_m2_t2 <- pffr(O3_W2_COVID_2021_DS_FCPA_RC_MEAN ~ sff(O3_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                         sff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                         sff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                         sff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                         sff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                       yind = 1:12)

FF_W2_O3_m2_t3 <- pffr(O3_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ffpc(O3_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                         ffpc(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                         ffpc(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                         ffpc(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                         ffpc(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                       yind = 1:12)



FF_W2_O3_m2_t1_sum <- summary(FF_W2_O3_m2_t1)
FF_W2_O3_m2_t2_sum <- summary(FF_W2_O3_m2_t2)
FF_W2_O3_m2_t3_sum <- summary(FF_W2_O3_m2_t3)

FF_W2_O3_AIC_Tables <- data.frame(m1_t1_rsq = FF_W2_O3_m2_t1_sum$r.sq, m1_t1_AIC = AIC(FF_W2_O3_m2_t1),m1_t1_BIC = BIC(FF_W2_O3_m2_t1),
                                  m1_t2_rsq = FF_W2_O3_m2_t2_sum$r.sq, m1_t2_AIC = AIC(FF_W2_O3_m2_t2),m1_t2_BIC = BIC(FF_W2_O3_m2_t2),
                                  m1_t3_rsq = FF_W2_O3_m2_t3_sum$r.sq, m1_t3_AIC = AIC(FF_W2_O3_m2_t3),m1_t3_BIC = BIC(FF_W2_O3_m2_t3))


FF_W2_O3_m2_t1_ANOVA <- data.frame(method = "ff"   , FF_W2_O3_m2_t1_sum$s.table)
FF_W2_O3_m2_t2_ANOVA <- data.frame(method = "sff"  , FF_W2_O3_m2_t2_sum$s.table)
FF_W2_O3_m2_t3_ANOVA <- data.frame(method = "ffpc" , FF_W2_O3_m2_t3_sum$s.table)
FF_W2_O3_m2_ANOVA    <- rbind(FF_W2_O3_m2_t1_ANOVA,FF_W2_O3_m2_t2_ANOVA,FF_W2_O3_m2_t3_ANOVA)


write.csv(FF_W2_O3_AIC_Tables,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\O3\\FF_W2_O3_AIC_Tables.csv")

write.csv(FF_W2_O3_m2_ANOVA,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\O3\\FF_W2_O3_m2_ANOVA.csv")

######## SO2
FF_W2_SO2_m2_t1 <- pffr(SO2_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(SO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                          ff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                          ff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                        yind = 1:12)


FF_W2_SO2_m2_t2 <- pffr(SO2_W2_COVID_2021_DS_FCPA_RC_MEAN ~ sff(SO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          sff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                          sff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                          sff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          sff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                        yind = 1:12)

FF_W2_SO2_m2_t3 <- pffr(SO2_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ffpc(SO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ffpc(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                          ffpc(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                          ffpc(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ffpc(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                        yind = 1:12)



FF_W2_SO2_m2_t1_sum <- summary(FF_W2_SO2_m2_t1)
FF_W2_SO2_m2_t2_sum <- summary(FF_W2_SO2_m2_t2)
FF_W2_SO2_m2_t3_sum <- summary(FF_W2_SO2_m2_t3)

FF_W2_SO2_AIC_Tables <- data.frame(m1_t1_rsq = FF_W2_SO2_m2_t1_sum$r.sq, m1_t1_AIC = AIC(FF_W2_SO2_m2_t1),m1_t1_BIC = BIC(FF_W2_SO2_m2_t1),
                                   m1_t2_rsq = FF_W2_SO2_m2_t2_sum$r.sq, m1_t2_AIC = AIC(FF_W2_SO2_m2_t2),m1_t2_BIC = BIC(FF_W2_SO2_m2_t2),
                                   m1_t3_rsq = FF_W2_SO2_m2_t3_sum$r.sq, m1_t3_AIC = AIC(FF_W2_SO2_m2_t3),m1_t3_BIC = BIC(FF_W2_SO2_m2_t3))


FF_W2_SO2_m2_t1_ANOVA <- data.frame(method = "ff"   , FF_W2_SO2_m2_t1_sum$s.table)
FF_W2_SO2_m2_t2_ANOVA <- data.frame(method = "sff"  , FF_W2_SO2_m2_t2_sum$s.table)
FF_W2_SO2_m2_t3_ANOVA <- data.frame(method = "ffpc" , FF_W2_SO2_m2_t3_sum$s.table)
FF_W2_SO2_m2_ANOVA    <- rbind(FF_W2_SO2_m2_t1_ANOVA,FF_W2_SO2_m2_t2_ANOVA,FF_W2_SO2_m2_t3_ANOVA)


write.csv(FF_W2_SO2_AIC_Tables,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\SO2\\FF_W2_SO2_AIC_Tables.csv")

write.csv(FF_W2_SO2_m2_ANOVA,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\SO2\\FF_W2_SO2_m2_ANOVA.csv")

######## AEI
FF_W2_AEI_m2_t1 <- pffr(AEI_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(AEI_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                          ff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                          ff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                        yind = 1:12)


FF_W2_AEI_m2_t2 <- pffr(AEI_W2_COVID_2021_DS_FCPA_RC_MEAN ~ sff(AEI_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          sff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                          sff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                          sff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          sff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                        yind = 1:12)

FF_W2_AEI_m2_t3 <- pffr(AEI_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ffpc(AEI_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ffpc(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                          ffpc(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                          ffpc(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                          ffpc(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                        yind = 1:12)



FF_W2_AEI_m2_t1_sum <- summary(FF_W2_AEI_m2_t1)
FF_W2_AEI_m2_t2_sum <- summary(FF_W2_AEI_m2_t2)
FF_W2_AEI_m2_t3_sum <- summary(FF_W2_AEI_m2_t3)

FF_W2_AEI_AIC_Tables <- data.frame(m1_t1_rsq = FF_W2_AEI_m2_t1_sum$r.sq, m1_t1_AIC = AIC(FF_W2_AEI_m2_t1),m1_t1_BIC = BIC(FF_W2_AEI_m2_t1),
                                   m1_t2_rsq = FF_W2_AEI_m2_t2_sum$r.sq, m1_t2_AIC = AIC(FF_W2_AEI_m2_t2),m1_t2_BIC = BIC(FF_W2_AEI_m2_t2),
                                   m1_t3_rsq = FF_W2_AEI_m2_t3_sum$r.sq, m1_t3_AIC = AIC(FF_W2_AEI_m2_t3),m1_t3_BIC = BIC(FF_W2_AEI_m2_t3))


FF_W2_AEI_m2_t1_ANOVA <- data.frame(method = "ff"   , FF_W2_AEI_m2_t1_sum$s.table)
FF_W2_AEI_m2_t2_ANOVA <- data.frame(method = "sff"  , FF_W2_AEI_m2_t2_sum$s.table)
FF_W2_AEI_m2_t3_ANOVA <- data.frame(method = "ffpc" , FF_W2_AEI_m2_t3_sum$s.table)
FF_W2_AEI_m2_ANOVA    <- rbind(FF_W2_AEI_m2_t1_ANOVA,FF_W2_AEI_m2_t2_ANOVA,FF_W2_AEI_m2_t3_ANOVA)


write.csv(FF_W2_AEI_AIC_Tables,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\AEI\\FF_W2_AEI_AIC_Tables.csv")

write.csv(FF_W2_AEI_m2_ANOVA,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\AEI\\FF_W2_AEI_m2_ANOVA.csv")

######## HCHO
FF_W2_HCHO_m2_t1 <- pffr(HCHO_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ff(HCHO_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                           ff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                           ff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                           ff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                           ff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                         yind = 1:12)


FF_W2_HCHO_m2_t2 <- pffr(HCHO_W2_COVID_2021_DS_FCPA_RC_MEAN ~ sff(HCHO_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                           sff(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                           sff(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                           sff(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                           sff(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                         yind = 1:12)

FF_W2_HCHO_m2_t3 <- pffr(HCHO_W2_COVID_2021_DS_FCPA_RC_MEAN ~ ffpc(HCHO_W2_NOCOVID_2020_DS_FCPA_RC_MEAN            , xind = 1:12) +
                           ffpc(Pressure_W2_COVID_2021_DS_FCPA_RC_MEAN         , xind = 1:12) +
                           ffpc(TPrecipitation_W2_COVID_2021_DS_FCPA_RC_MEAN   , xind = 1:12) +
                           ffpc(MTemp_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) +
                           ffpc(UWind_W2_COVID_2021_DS_FCPA_RC_MEAN            , xind = 1:12) ,
                         yind = 1:12)



FF_W2_HCHO_m2_t1_sum <- summary(FF_W2_HCHO_m2_t1)
FF_W2_HCHO_m2_t2_sum <- summary(FF_W2_HCHO_m2_t2)
FF_W2_HCHO_m2_t3_sum <- summary(FF_W2_HCHO_m2_t3)

FF_W2_HCHO_AIC_Tables <- data.frame(m1_t1_rsq = FF_W2_HCHO_m2_t1_sum$r.sq, m1_t1_AIC = AIC(FF_W2_HCHO_m2_t1),m1_t1_BIC = BIC(FF_W2_HCHO_m2_t1),
                                    m1_t2_rsq = FF_W2_HCHO_m2_t2_sum$r.sq, m1_t2_AIC = AIC(FF_W2_HCHO_m2_t2),m1_t2_BIC = BIC(FF_W2_HCHO_m2_t2),
                                    m1_t3_rsq = FF_W2_HCHO_m2_t3_sum$r.sq, m1_t3_AIC = AIC(FF_W2_HCHO_m2_t3),m1_t3_BIC = BIC(FF_W2_HCHO_m2_t3))


FF_W2_HCHO_m2_t1_ANOVA <- data.frame(method = "ff"   , FF_W2_HCHO_m2_t1_sum$s.table)
FF_W2_HCHO_m2_t2_ANOVA <- data.frame(method = "sff"  , FF_W2_HCHO_m2_t2_sum$s.table)
FF_W2_HCHO_m2_t3_ANOVA <- data.frame(method = "ffpc" , FF_W2_HCHO_m2_t3_sum$s.table)
FF_W2_HCHO_m2_ANOVA    <- rbind(FF_W2_HCHO_m2_t1_ANOVA,FF_W2_HCHO_m2_t2_ANOVA,FF_W2_HCHO_m2_t3_ANOVA)


write.csv(FF_W2_HCHO_AIC_Tables,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\HCHO\\FF_W2_HCHO_AIC_Tables.csv")

write.csv(FF_W2_HCHO_m2_ANOVA,
          "D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_ff_Comparisons\\HCHO\\FF_W2_HCHO_m2_ANOVA.csv")



#############  Fitted vs Observed 
######## CO1
FF_W1_CO1_m2_t1_OBS_Fitted <- rowMeans(CO1_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_CO1_m2_t1))
FF_W1_CO1_m2_t2_OBS_Fitted <- rowMeans(CO1_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_CO1_m2_t2))
FF_W1_CO1_m2_t3_OBS_Fitted <- rowMeans(CO1_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_CO1_m2_t3))

FF_W2_CO1_m2_t1_OBS_Fitted <- rowMeans(CO1_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_CO1_m2_t1))
FF_W2_CO1_m2_t2_OBS_Fitted <- rowMeans(CO1_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_CO1_m2_t2))
FF_W2_CO1_m2_t3_OBS_Fitted <- rowMeans(CO1_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_CO1_m2_t3))
######## CO2
FF_W1_CO2_m2_t1_OBS_Fitted <- rowMeans(CO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_CO2_m2_t1))
FF_W1_CO2_m2_t2_OBS_Fitted <- rowMeans(CO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_CO2_m2_t2))
FF_W1_CO2_m2_t3_OBS_Fitted <- rowMeans(CO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_CO2_m2_t3))

FF_W2_CO2_m2_t1_OBS_Fitted <- rowMeans(CO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_CO2_m2_t1))
FF_W2_CO2_m2_t2_OBS_Fitted <- rowMeans(CO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_CO2_m2_t2))
FF_W2_CO2_m2_t3_OBS_Fitted <- rowMeans(CO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_CO2_m2_t3))
######## NO2
FF_W1_NO2_m2_t1_OBS_Fitted <- rowMeans(NO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_NO2_m2_t1))
FF_W1_NO2_m2_t2_OBS_Fitted <- rowMeans(NO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_NO2_m2_t2))
FF_W1_NO2_m2_t3_OBS_Fitted <- rowMeans(NO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_NO2_m2_t3))

FF_W2_NO2_m2_t1_OBS_Fitted <- rowMeans(NO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_NO2_m2_t1))
FF_W2_NO2_m2_t2_OBS_Fitted <- rowMeans(NO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_NO2_m2_t2))
FF_W2_NO2_m2_t3_OBS_Fitted <- rowMeans(NO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_NO2_m2_t3))
######## O3
FF_W1_O3_m2_t1_OBS_Fitted <- rowMeans(O3_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_O3_m2_t1))
FF_W1_O3_m2_t2_OBS_Fitted <- rowMeans(O3_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_O3_m2_t2))
FF_W1_O3_m2_t3_OBS_Fitted <- rowMeans(O3_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_O3_m2_t3))

FF_W2_O3_m2_t1_OBS_Fitted <- rowMeans(O3_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_O3_m2_t1))
FF_W2_O3_m2_t2_OBS_Fitted <- rowMeans(O3_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_O3_m2_t2))
FF_W2_O3_m2_t3_OBS_Fitted <- rowMeans(O3_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_O3_m2_t3))
######## SO2
FF_W1_SO2_m2_t1_OBS_Fitted <- rowMeans(SO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_SO2_m2_t1))
FF_W1_SO2_m2_t2_OBS_Fitted <- rowMeans(SO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_SO2_m2_t2))
FF_W1_SO2_m2_t3_OBS_Fitted <- rowMeans(SO2_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_SO2_m2_t3))

FF_W2_SO2_m2_t1_OBS_Fitted <- rowMeans(SO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_SO2_m2_t1))
FF_W2_SO2_m2_t2_OBS_Fitted <- rowMeans(SO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_SO2_m2_t2))
FF_W2_SO2_m2_t3_OBS_Fitted <- rowMeans(SO2_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_SO2_m2_t3))
######## AEI
FF_W1_AEI_m2_t1_OBS_Fitted <- rowMeans(AEI_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_AEI_m2_t1))
FF_W1_AEI_m2_t2_OBS_Fitted <- rowMeans(AEI_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_AEI_m2_t2))
FF_W1_AEI_m2_t3_OBS_Fitted <- rowMeans(AEI_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_AEI_m2_t3))

FF_W2_AEI_m2_t1_OBS_Fitted <- rowMeans(AEI_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_AEI_m2_t1))
FF_W2_AEI_m2_t2_OBS_Fitted <- rowMeans(AEI_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_AEI_m2_t2))
FF_W2_AEI_m2_t3_OBS_Fitted <- rowMeans(AEI_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_AEI_m2_t3))
######## HCHO
FF_W1_HCHO_m2_t1_OBS_Fitted <- rowMeans(HCHO_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_HCHO_m2_t1))
FF_W1_HCHO_m2_t2_OBS_Fitted <- rowMeans(HCHO_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_HCHO_m2_t2))
FF_W1_HCHO_m2_t3_OBS_Fitted <- rowMeans(HCHO_W1_NOCOVID_2019_DS_FCPA_RC_MEAN - fitted(FF_W1_HCHO_m2_t3))

FF_W2_HCHO_m2_t1_OBS_Fitted <- rowMeans(HCHO_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_HCHO_m2_t1))
FF_W2_HCHO_m2_t2_OBS_Fitted <- rowMeans(HCHO_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_HCHO_m2_t2))
FF_W2_HCHO_m2_t3_OBS_Fitted <- rowMeans(HCHO_W2_NOCOVID_2020_DS_FCPA_RC_MEAN - fitted(FF_W2_HCHO_m2_t3))




############################
############################
############################
############## Maps -- pffr with ffpc
############################
############################
############################


FF_W1_t3 <- data.frame(PRV  = unique(CO1_W1_NOCOVID_2019_DS$subj),
                    W1_CO1  = FF_W1_CO1_m2_t3_OBS_Fitted,
                    W1_CO2  = FF_W1_CO2_m2_t3_OBS_Fitted,
                    W1_NO2  = FF_W1_NO2_m2_t3_OBS_Fitted,
                    W1_O3   = FF_W1_O3_m2_t3_OBS_Fitted,
                    W1_SO2  = FF_W1_SO2_m2_t3_OBS_Fitted,
                    W1_AEI  = FF_W1_AEI_m2_t3_OBS_Fitted,
                    W1_HCHO = FF_W1_HCHO_m2_t3_OBS_Fitted)



FF_W2_t3 <- data.frame(PRV  = unique(CO1_W1_NOCOVID_2019_DS$subj),
                    W2_CO1  = FF_W2_CO1_m2_t3_OBS_Fitted,
                    W2_CO2  = FF_W2_CO2_m2_t3_OBS_Fitted,
                    W2_NO2  = FF_W2_NO2_m2_t3_OBS_Fitted,
                    W2_O3   = FF_W2_O3_m2_t3_OBS_Fitted,
                    W2_SO2  = FF_W2_SO2_m2_t3_OBS_Fitted,
                    W2_AEI  = FF_W2_AEI_m2_t3_OBS_Fitted,
                    W2_HCHO = FF_W2_HCHO_m2_t3_OBS_Fitted)

Iran_names <- c("Tehran", "Zanjan","Yazd" ,"West Azerbaijan","Markazi","Sistan and Baluchestan" ,
                "Semnan","Qom" ,"Qazvin","Mazandaran","Kurdistan" ,"Lorestan"  ,"Kohgiluyeh and Boyer-Ahmad",
                "Khuzestan" ,"South Khorasan" ,"Razavi Khorasan", "North Khorasan","Kermanshah" ,"Kerman",
                "Ilam","Hormozgan","Hamadan" ,"Golestan" ,"Gilan" ,"Fars" ,"Isfahan"  ,"East Azerbaijan"  ,
                "Chaharmahal and Bakhtiari", "Bushehr", "Ardabil" ,"Alborz")


FF_Fitted_Values <- cbind(ADM1_EN=Iran_names, FF_W1_t3,FF_W2_t3)
names(FF_Fitted_Values)

library(sf)
library(ggplot2)

iran <- st_read("D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Maps\\irn_adm_unhcr_20190514_shp\\irn_admbnda_adm1_unhcr_20190514.shp")
iran$W1_CO1  <- FF_Fitted_Values$W1_CO1[match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W1_CO2  <- FF_Fitted_Values$W1_CO2[match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W1_NO2  <- FF_Fitted_Values$W1_NO2[match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W1_O3   <- FF_Fitted_Values$W1_O3[match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W1_SO2  <- FF_Fitted_Values$W1_SO2[match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W1_AEI  <- FF_Fitted_Values$W1_AEI[match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W1_HCHO <- FF_Fitted_Values$W1_HCHO[match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]

iran$W2_CO1  <- FF_Fitted_Values$W2_CO1 [match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W2_CO2  <- FF_Fitted_Values$W2_CO2 [match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W2_NO2  <- FF_Fitted_Values$W2_NO2 [match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W2_O3   <- FF_Fitted_Values$W2_O3  [match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W2_SO2  <- FF_Fitted_Values$W2_SO2 [match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W2_AEI  <- FF_Fitted_Values$W2_AEI [match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]
iran$W2_HCHO <- FF_Fitted_Values$W2_HCHO[match(iran$ADM1_EN,FF_Fitted_Values$ADM1_EN )]

library(scales) # for muted
library(ggpubr)

########## CO1 
W1_CO1 <- ggplot(iran) + 
  geom_sf(aes(fill = W1_CO1)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(0,2), 
                       na.value = "orange",
                       name = "") +
  theme_void() 



W2_CO1 <- ggplot(iran) + 
  geom_sf(aes(fill = W2_CO1)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(0,2), 
                       na.value = "orange",
                       name = "") +
  theme_void() +
  guides(fill=guide_legend(title=""))

ggarrange_CO1 <- ggarrange(W1_CO1 , W2_CO1,
                           labels = c("A", "B"),
                           ncol = 2, nrow = 1,
                           common.legend = TRUE, legend="bottom")

ggsave("D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_Maps_ffpc\\ggarrange_CO1.png", 
       ggarrange_CO1,limitsize = FALSE, dpi = "print",width=11,height = 5)



########## CO2 
W1_CO2 <- ggplot(iran) + 
  geom_sf(aes(fill = W1_CO2)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-30,100), 
                       na.value = "orange",
                       name = "") +
  theme_void() 



W2_CO2 <- ggplot(iran) + 
  geom_sf(aes(fill = W2_CO2)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-30,100), 
                       na.value = "orange",
                       name = "") +
  theme_void() +
  guides(fill=guide_legend(title=""))

ggarrange_CO2 <- ggarrange(W1_CO2 , W2_CO2,
                           labels = c("A", "B"),
                           ncol = 2, nrow = 1,
                           common.legend = TRUE, legend="bottom")

ggsave("D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_Maps_ffpc\\ggarrange_CO2.png", 
       ggarrange_CO2,limitsize = FALSE, dpi = "print",width=11,height = 5)




########## NO2 
W1_NO2 <- ggplot(iran) + 
  geom_sf(aes(fill = W1_NO2)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-25,120), 
                       na.value = "orange",
                       name = "") +
  theme_void() 



W2_NO2 <- ggplot(iran) + 
  geom_sf(aes(fill = W2_NO2)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-25,120), 
                       na.value = "orange",
                       name = "") +
  theme_void() +
  guides(fill=guide_legend(title=""))

ggarrange_NO2 <- ggarrange(W1_NO2 , W2_NO2,
                           labels = c("A", "B"),
                           ncol = 2, nrow = 1,
                           common.legend = TRUE, legend="bottom")

ggsave("D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_Maps_ffpc\\ggarrange_NO2.png", 
       ggarrange_NO2,limitsize = FALSE, dpi = "print",width=11,height = 5)





########## O3 
W1_O3 <- ggplot(iran) + 
  geom_sf(aes(fill = W1_O3)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-2,2), 
                       na.value = "orange",
                       name = "") +
  theme_void() 



W2_O3 <- ggplot(iran) + 
  geom_sf(aes(fill = W2_O3)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-2,2), 
                       na.value = "orange",
                       name = "") +
  theme_void() +
  guides(fill=guide_legend(title=""))

ggarrange_O3 <- ggarrange(W1_O3 , W2_O3,
                          labels = c("A", "B"),
                          ncol = 2, nrow = 1,
                          common.legend = TRUE, legend="bottom")

ggsave("D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_Maps_ffpc\\ggarrange_O3.png", 
       ggarrange_O3,limitsize = FALSE, dpi = "print",width=11,height = 5)





########## SO2 ()
W1_SO2 <- ggplot(iran) + 
  geom_sf(aes(fill = W1_SO2)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-35,80), 
                       na.value = "orange",
                       name = "") +
  theme_void() 



W2_SO2 <- ggplot(iran) + 
  geom_sf(aes(fill = W2_SO2)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-35,80), 
                       na.value = "orange",
                       name = "") +
  theme_void() +
  guides(fill=guide_legend(title=""))

ggarrange_SO2 <- ggarrange(W1_SO2 , W2_SO2,
                           labels = c("A", "B"),
                           ncol = 2, nrow = 1,
                           common.legend = TRUE, legend="bottom")

ggsave("D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_Maps_ffpc\\ggarrange_SO2.png", 
       ggarrange_SO2,limitsize = FALSE, dpi = "print",width=11,height = 5)





########## AEI 
W1_AEI <- ggplot(iran) + 
  geom_sf(aes(fill = W1_AEI)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-5,5), 
                       na.value = "orange",
                       name = "") +
  theme_void() 



W2_AEI <- ggplot(iran) + 
  geom_sf(aes(fill = W2_AEI)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-5,5), 
                       na.value = "orange",
                       name = "") +
  theme_void() +
  guides(fill=guide_legend(title=""))

ggarrange_AEI <- ggarrange(W1_AEI , W2_AEI,
                           labels = c("A", "B"),
                           ncol = 2, nrow = 1,
                           common.legend = TRUE, legend="bottom")

ggsave("D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_Maps_ffpc\\ggarrange_AEI.png", 
       ggarrange_AEI,limitsize = FALSE, dpi = "print",width=11,height = 5)




########## HCHO ()
W1_HCHO <- ggplot(iran) + 
  geom_sf(aes(fill = W1_HCHO)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-50,25), 
                       na.value = "orange",
                       name = "") +
  theme_void() 



W2_HCHO <- ggplot(iran) + 
  geom_sf(aes(fill = W2_HCHO)) +
  geom_sf_text(data = iran, aes(label = ADM1_EN),size=2.5) + 
  scale_fill_gradient2(low = "pink", high = "blue", 
                       mid = "gray80",
                       midpoint = 0, limits = c(-50,25), 
                       na.value = "orange",
                       name = "") +
  theme_void() +
  guides(fill=guide_legend(title=""))

ggarrange_HCHO <- ggarrange(W1_HCHO , W2_HCHO,
                            labels = c("A", "B"),
                            ncol = 2, nrow = 1,
                            common.legend = TRUE, legend="bottom")

ggsave("D:\\Articles\\04_140309_COVID_Iran\\03_RProject\\R_Project\\Output_Maps_ffpc\\ggarrange_HCHO.png", 
       ggarrange_HCHO,limitsize = FALSE, dpi = "print",width=11,height = 5)















