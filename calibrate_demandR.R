library("devtools")
library("data.table")
library("ggplot2")
library("sp")
library("maptools")
library("lubridate")
library("mgcv")
library("tidyverse")
library("caret")
library("zoo")

rsq <- function (x, y) cor(x, y) ^ 2
  
getSeason<-function(MONTH){
    ifelse (MONTH>=3 & MONTH <6,"2",
      ifelse(MONTH>=6 & MONTH <9,"3",
        ifelse(MONTH>=9 & MONTH <12, "4","1"))) 
}
  
    
  

## Calibrate GAM for South Staffs (SS), Severn Trent (ST) and a model for SS and ST combined


#directory of whespere the relevant data  and output directories are located
dirs <-"/home/h01/kchowien/Projects/WRW"
data_dir<-file.path(dirs,"data/")
model_dir<-file.path(dirs,"models/")
graph_dir<-file.path(dirs,"graphs/")
output_dir<-file.path(dirs,"output/")


##Read in weather data, this inlcudes: MORECS data-  Soil moisture defecit (SMD) and sunshine, HadUK - maximum temperature and rain

morecs_data<-read.csv(file.path(data_dir,"morecs_125.csv")) 
morecs_data$Date<-as.Date(morecs_data$Date,"%d/%m/%Y")
rain<-read.csv(file.path(data_dir,"rain_ncic.csv"))
rain$Date<-as.Date(rain$Date,"%Y-%m-%d")
temp<-read.csv(file.path(data_dir,"maxT_ncic.csv"))
temp$Date<-as.Date(temp$Date,"%Y-%m-%d")
ncic_data<-merge(temp,rain,by="Date")

##Create weather_data data frame by merging ncic and morecs data and add month/day of week index

weather_data<-merge(ncic_data,morecs_data,by="Date")
weather_data$dayyear<-yday(weather_data$Date)
weather_data$year<-year(weather_data$Date)
weather_data$month<-month(weather_data$Date)
weather_data$weekday<-wday(weather_data$Date)
weather_data$monthindex<-weather_data$month+(weather_data$year)*12


## Calculate temp index - slow moving index based on temp
weather_data$tempindex<-weather_data$MaxT
for(i in seq(2,length(weather_data$MaxT))){
  weather_data$temp_index[i] = (weather_data$temp_index[i-1]*0.95)+(weather_data$MaxT[i]*0.05)
}


##Rain factor - index if it rained i.e. daily rain>0.2 mm

weather_data$rainfactor<-ifelse(weather_data$Rain>0.2,1,0)
weather_data$rainfactoryday<-c(NA,weather_data$rainfactor[1:length(weather_data$rainfactor)-1])


##rolling mean temp  -2 day and 3 day rolling mean temp


weather_data$max_2<-rollmean(weather_data$MaxT,2,align="right",na.pad="TRUE")
weather_data$max_2<-c(NA,weather_data$max_2[1:length(weather_data$max_2)-1])


weather_data$max_3<-rollmean(weather_data$MaxT,3,align="right",na.pad="TRUE")
weather_data$max_3<-c(NA,NA,weather_data$max_3[1:length(weather_data$max_3)-2])


#Read in latest demand data 
Demand<- read.csv(file.path(data_dir,"Demand_SS_2.csv"), na.strings=c("","NA","#NAME?"), stringsAsFactors = FALSE)
Demand$Date<-as.Date(Demand$Date,"%d/%m/%Y")

#Plot demand data 
png(file.path(graph_dir,"timeseriesdemand_SS.png"))
plot(Demand$Date,Demand$Demand,type="l",xlab="Date",ylab="Demand (Ml/day)")
dev.off()


Demand_ST<-read.csv(file.path(data_dir,"Severntrent_company_demand.csv"))
Demand_ST$Date<-as.Date(Demand_ST$Date,"%d/%m/%Y")


png(file.path(graph_dir,"timeseriesdemand_ST.png"))
plot(Demand_ST$Date,Demand_ST$Demand.ST,type="l",xlab="Date",ylab="Demand (Ml/day)")
dev.off()


#plot SS vs ST Demand
png(file.path(graph_dir,"demand_STvsDemandSS.png"))
ggplot(model_data_all,aes(x=Demand,y=Demand.ST))+geom_point()+geom_smooth(method=lm)+xlab("South Staffs Demand (Ml/day)")+ylab("Severn Trent Demand (Ml/day)")
dev.off()


##merging the DI data with weather data
model_data_SS<-merge(Demand,weather_data,by="Date")
model_data_ST<-merge(Demand_ST,weather_data,by="Date")

bad_start_SS<-as.Date("01/01/2010","%d/%m/%Y")
bad_end_SS<-as.Date("01/01/2011","%d/%m/%Y")


##Set bad data  to NA 
model_data_SS<-within(model_data_SS,Demand[Date>=bad_start_SS & Date<bad_end_SS]<-NA)
model_data_all<-merge(model_data_SS,model_data_ST[,0:2],by="Date")
model_data_all$Totaldemand<-model_data_all$Demand+model_data_all$Demand.ST

##Don't want to use 2020 in modelling as lockdown changed demand pattern 
model_data_all<-subset(model_data_all,year<2020)
model_data_all<-na.omit(model_data_all)



#Plot weather vs demand 

png(file.path(graph_dir,"weather+demand_Ss.png"))
weather_demand<-model_data_all[c("Demand","MaxT","Rain","SMD","Sunshine","tempindex","max_2","max_3")]
mtmelt<-melt(weather_demand,id="Demand")
ggplot(mtmelt, aes(x = value, y = Demand)) +
  facet_wrap(~variable, scales = "free") +
  geom_point()+geom_smooth()
dev.off()

png(file.path(graph_dir,"weather+demand_ST.png"))
weather_demand<-model_data_all[c("Demand.ST","MaxT","Rain","SMD","Sunshine","tempindex")]
mtmelt<-melt(weather_demand,id="Demand.ST")
ggplot(mtmelt, aes(x = value, y = Demand.ST)) +
  facet_wrap(~variable, scales = "free") +
  geom_point()+geom_smooth()

dev.off()


###Calibrate 3 seperate models for SS , ST and Total demand (SS+ST)

model_SS <- gam(Demand ~
                ti(dayyear,bs="cr",k=20)
                +ti(monthindex)
                +ti(weekday,k=7)
                +ti(Sunshine,by=factor(rainfactoryday))
                +ti(tempindex)
                +ti(MaxT,by=factor(rainfactoryday))
                +ti(SMD,by=factor(rainfactoryday)), data = model_data_all)
plot(model_SS,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)
gam.check(model)
save(model_SS,file=file.path(model_dir,"model_SS.RData"))

model_ST<-gam(Demand.ST ~
                ti(dayyear,bs="cr",k=20)
              +ti(monthindex)
              +ti(weekday,k=7)
              +ti(Sunshine,by=factor(rainfactoryday))
              +ti(tempindex)
              +ti(MaxT,by=factor(rainfactoryday))
              +ti(SMD,by=factor(rainfactoryday)), data = model_data_all)
plot(model_SS,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)
gam.check(model)
save(model_ST,file=file.path(model_dir,"model_ST.RData"))


model_all <- gam(Totaldemand~
                +ti(dayyear,bs="cr",k=20)
                +ti(monthindex)
                +ti(weekday,k=7)
                +ti(Sunshine,by=factor(rainfactoryday))
                +ti(tempindex)
                +ti(max_3,by=factor(rainfactoryday))
                +ti(MaxT,by=factor(rainfactoryday))
                +ti(SMD), data = model_data_all)

save(model_all,file=file.path(model_dir,"model_all.RData"))

##Plot model all 
png(file.path(graph_dir,"model_all_weather.png"))
par(mfrow=c(4,2))
plot(model_all,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2,select=4)
plot(model_all,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2,select=5)     
plot(model_all,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2,select=7)
plot(model_all,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2,select=8)
plot(model_all,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2,select=9)
plot(model_all,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2,select=10)
plot(model_all,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2,select=11)
plot(model_all,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2,select=6)
dev.off()



png(file.path(graph_dir,"model_all_base.png"))
par(mfrow=c(1,3))
plot(model_all,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2,select=2)
plot(model_all,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2,select=1)     
plot(model_all,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2,select=3)

dev.off()
gam.check(model_all)



#covert total demand to seperate areas using the ratio 
SS_ratio<-mean(model_data_all$Demand,na.rm="TRUE")/mean(model_data_all$Totaldemand,na.rm="TRUE")
ST_ratio<-mean(model_data_all$Demand.ST,na.rm="TRUE")/mean(model_data_all$Totaldemand,na.rm="TRUE")



##test model performance make data frame called pred with relevant columns 

pred<-data.table(predict.gam(model_all,model_data_all,type="terms"))

#base = non weather component of model
pred$predbase<-pred$`ti(dayyear)`+pred$`ti(monthindex)`+pred$`ti(weekday)`+model_all$coefficients[1]
pred$predDI<-predict.gam(model_all,model_data_all)
pred$predweather=pred$predDI-pred$predbase

#weather component can't be negative
minpw<-min(pred$predweather)
pred$predweather<-pred$predweather-minpw
pred$predbase<-pred$predbase+minpw

pred$Demand<-model_data_all$Totaldemand

##Convert to SS and ST using ratio 

pred$predbase_SS<-pred$predbase*SS_ratio
pred$predbase_ST<-pred$predbase*ST_ratio
pred$predDI_SS<-pred$predDI*SS_ratio
pred$predDI_ST<-pred$predDI*ST_ratio
pred$Demand_SS<-model_data_all$Demand
pred$Demand_ST<-model_data_all$Demand.ST

pred$error_ST<-pred$predDI_ST-pred$Demand_ST
pred$error_SS<-pred$predDI_SS-pred$Demand_SS
pred$percent_error_ST<-pred$error_ST/pred$Demand_ST
pred$percent_error_SS<-pred$error_SS/pred$Demand_SS
pred$Date<-model_data_all$Date


#plot time series for ST
png(file.path(graph_dir,"timeseriesdemandmodel_ST.png"))
plot(pred$Date,pred$Demand_ST,type="l",col="black",ylim=c(1600,2300),xlab=("Date"),ylab=("Demand (Ml/day)"))
lines(pred$Date,pred$predDI_ST,col="blue")
lines(pred$Date,pred$predbase_ST,col="red")
legend("topleft",c("Modelled demand","Observed demand","Base demand"),cex=.8,lty=c(1,1,1),col=c("blue", "black","red"))
dev.off()


##2018
png(file.path(graph_dir,"timeseriesdemandmodel_ST_2018.png"))
pred_2018<-subset(pred,year(pred$Date)==2018)
plot(pred_2018$Date,pred_2018$predDI_ST,type="l",col="blue",ylim=c(1700,2300),xlab=("Date"),ylab=("Demand (Ml/day)"))
lines(pred_2018$Date,pred_2018$Demand_ST,col="black")
lines(pred_2018$Date,pred_2018$predbase_ST,col="red")
legend("topright",c("Modelled demand","Observed demand","Base demand"),cex=.8,lty=c(1,1,1),col=c("blue", "black","red"))
dev.off()
  



#plot time series for SS
png(file.path(graph_dir,"timeseriesdemandmodel_SS.png"))
plot(pred$Date,pred$Demand_SS,type="l",col="black",ylim=c(250,450),xlab=("Date"),ylab=("Demand (Ml/day)"))
lines(pred$Date,pred$predDI_SS,col="blue")
lines(pred$Date,pred$predbase_SS,col="red")
legend("topleft",c("Modelled demand","Observed demand","Base demand"),cex=.8,lty=c(1,1,1),col=c("blue", "black","red"))
dev.off()


##2018
png(file.path(graph_dir,"timeseriesdemandmodel_2018_SS.png"))
pred_2018<-subset(pred,year(pred$Date)==2018)
plot(pred_2018$Date,pred_2018$predDI_SS,type="l",col="blue",ylim=c(300,450),xlab=("Date"),ylab=("Demand (Ml/day)"))
lines(pred_2018$Date,pred_2018$Demand_SS,col="black")
lines(pred_2018$Date,pred_2018$predbase_SS,col="red")
legend("topleft",c("Modelled demand","Observed demand","Base demand"),cex=.8,lty=c(1,1,1),col=c("blue", "black","red"))
dev.off()




# Model performance SS
pred<-na.omit(pred)
data.frame(
  #RMSE = RMSE(pred$predDI_SS, pred$Demand_SS,na.rm=TRUE),
  #RMSE_percent=RMSE(pred$predDI_SS, pred$Demand_SS,na.rm=TRUE)/mean(pred$Demand_SS),
  sd=sd(pred$error_SS,na.rm=TRUE),
  bias=mean(pred$error_SS,na.rm=TRUE),
  R2 = rsq(pred$predDI_SS, pred$Demand_SS),
  sdpercent=sd(pred$percent_error_SS,na.rm=TRUE))

# Model performance ST
data.frame(
  #RMSE = RMSE(pred$predDI_ST, pred$Demand_ST,na.rm=TRUE),
  #RMSE_percent=RMSE(pred$predDI_ST, pred$Demand_ST,na.rm=TRUE)/mean(pred$Demand_ST),
  sd=sd(pred$error_ST,na.rm=TRUE),
  bias=mean(pred$error_ST,na.rm=TRUE),
  R2 = rsq(pred$predDI_ST, pred$Demand_ST),
  sdpercent=sd(pred$percent_error_ST,na.rm=TRUE),
  bias=mean(pred$percent_error_ST,na.rm=TRUE))




###ST specific model performance 
pred_ST<-data.table(predict.gam(model_ST,type="terms"))
pred_ST$predbase<-pred_ST$`ti(dayyear)`+pred_ST$`ti(monthindex)`+pred_ST$`ti(weekday)`+model_ST$coefficients[1]
pred_ST$predDI<-predict.gam(model_ST,model_data_all)
pred_ST$predweather=pred_ST$predDI-pred_ST$predbase

#weather component can't be negative
minpw_ST<-min(pred_ST$predweather)
pred_ST$predweather<-pred_ST$predweather-minpw_ST
pred_ST$predbase<-pred_ST$predbase+minpw
pred_ST$Demand<-model_data_all$Demand.ST
pred_ST$Date<-model_data_all$Date

pred_ST$error<-pred_ST$Demand-pred_ST$predDI
pred_ST$error_percent<-pred_ST$error/pred_ST$Demand

##ST model performace 
data.frame(
  #RMSE = RMSE(pred$predDI_ST, pred$Demand_ST,na.rm=TRUE),
  #RMSE_percent=RMSE(pred$predDI_ST, pred$Demand_ST,na.rm=TRUE)/mean(pred$Demand_ST),
  sd=sd(pred_ST$error,na.rm=TRUE),
  bias=mean(pred_ST$error,na.rm=TRUE),
  R2 = rsq(pred_ST$predDI, pred_ST$Demand),
  sdpercent=sd(pred_ST$error_percent,na.rm=TRUE),
  biaspercent=mean(pred_ST$error_percent,na.rm=TRUE))


###SS specific model performance 

pred_SS<-data.table(predict.gam(model_SS,type="terms"),model_data_all)
pred_SS$predbase<-pred_SS$`ti(dayyear)`+pred_SS$`ti(monthindex)`+pred_SS$`ti(weekday)`+model_SS$coefficients[1]
pred_SS$predDI<-predict.gam(model_SS,model_data_all)
pred_SS$predweather=pred_SS$predDI-pred_SS$predbase

#weather component can't be negative
minpwSS<-min(pred_SS$predweather)
pred_SS$predweather<-pred_SS$predweather-minpwSS
pred_SS$predbase<-pred_SS$predbase+minpwSS
pred_SS$Demand<-model_data_all$Demand
pred_SS$Date<-model_data_all$Date

pred_SS$error<-pred_SS$Demand-pred_SS$predDI
pred_SS$error_percent<-pred_SS$error/pred_SS$Demand
pred_SS<-na.omit(pred_SS)
##ST model performace 
data.frame(
  #RMSE = RMSE(pred$predDI_ST, pred$Demand_ST,na.rm=TRUE),
  #RMSE_percent=RMSE(pred$predDI_ST, pred$Demand_ST,na.rm=TRUE)/mean(pred$Demand_ST),
  sd=sd(pred_SS$error,na.rm=TRUE),
  bias=mean(pred_SS$error,na.rm=TRUE),
  R2 = rsq(pred_SS$predDI, pred_SS$Demand),
  sdpercent=sd(pred_SS$error_percent,na.rm=TRUE),
  biaspercent=mean(pred_SS$error_percent,na.rm=TRUE))


#plot time series for SS
png(file.path(graph_dir,"timeseriesSSmodel.png"))
plot(pred_SS$Date,pred_SS$Demand,type="l",col="black",ylim=c(250,450),xlab=("Date"),ylab=("Demand (Ml/day)"))
lines(pred_SS$Date,pred_SS$predDI,col="blue")
lines(pred_SS$Date,pred_SS$predbase,col="red")
legend("topleft",c("Modelled demand","Observed demand","Base demand"),cex=.8,lty=c(1,1,1),col=c("blue", "black","red"))
dev.off()


##2018
png(file.path(graph_dir,"timeseriesSSdemandmodel_2018.png"))
pred_2018<-subset(pred_SS,year(pred_SS$Date)==2018)
plot(pred_2018$Date,pred_2018$predDI,type="l",col="blue",ylim=c(300,450),xlab=("Date"),ylab=("Demand (Ml/day)"))
lines(pred_2018$Date,pred_2018$Demand,col="black")
lines(pred_2018$Date,pred_2018$predbase,col="red")
legend("topleft",c("Modelled demand","Observed demand","Base demand"),cex=.8,lty=c(1,1,1),col=c("blue", "black","red"))
dev.off()




#run model back to 1960 and save data frame 
pred_1960<-data.table(predict.gam(model_all,weather_data,type="terms"))

pred_1960$predbase<-pred_1960$`ti(dayyear)`+pred_1960$`ti(monthindex)`+pred_1960$`ti(weekday)`+model_all$coefficients[1]+minpw
pred_1960$predDI<-predict.gam(model_all,weather_data)
pred_1960$predweather=pred_1960$predDI-pred_1960$predbase

#weather component can't be negative

modeloutput<-data.table(Date=weather_data$Date)
modeloutput$modelledweather<-pred_1960$predweather
modeloutput$modelledweatherSS<-modeloutput$modelledweather*SS_ratio
modeloutput$modelledweatherST<-modeloutput$modelledweather*ST_ratio


##save data frame

write.csv(modeloutput,file.path(output_dir,"dailymodelledusage_1961_2020.csv"))




