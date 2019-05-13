####0.LIBRARIES AND DIRECTORIES####
if(require("pacman")=="FALSE"){
  install.packages('pacman')
  library('pacman')
  pacman::p_load(here, readxl, plyr, caret, dplyr, doParallel,
                 lubridate, corrplot, ggplot2, 
                 tidyverse, arules, arulesViz, rstudioapi,RMySQL,
                 plotly, lubridate, forecast)
} else {
  library('pacman')
  pacman::p_load(here, readxl, plyr, caret, dplyr, doParallel,
                 lubridate, corrplot, ggplot2, 
                 tidyverse, arules, arulesViz, rstudioapi,RMySQL,
                 plotly, lubridate, forecast)
}

#setting up directory
current_path=getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)
list.files("Data/")

###Reading Data###

DataClean<-readRDS("./Data/NewData.rds")
#DataClean <- readRDS("C:/Users/Sergi Ch/Downloads/UBIQUM/PROJECT 3/TASK 3.1/Data/NewData.rds")

monthlypower<-readRDS("./Data/monthlypower.rds")
weeklypower<-readRDS("./Data/weeklypower.rds")
dailypower<-readRDS("./Data/dailypower.rds")


#### 1. GROUPING BY DATASETS (day, week, month) #####

####groupbyMONTH###
monthlypower<- DataClean%>%
  group_by(year, month)  %>%
  dplyr::summarize(GAP = sum(Global_active_power)/1000, 
                   SM = sum(SM)/1000,
                   Kitchen=sum(Sub_metering_1)/1000,
                   Laundry=sum(Sub_metering_2)/1000,
                   WaterHeaterAC=sum(Sub_metering_3)/1000,
                   REST=sum(Residual)/1000 )

#column with row names
X <- rownames(monthlypower)
monthlypower <- cbind(X=X, monthlypower)
  
saveRDS(monthlypower, file = "./Data/monthlypower.rds")

####groupbyWEEK###
weeklypower<- DataClean%>%
  group_by(year, month, week)  %>%
  dplyr::summarize(GAP = sum(Global_active_power)/1000, 
                   SM = sum(SM)/1000,
                   Kitchen=sum(Sub_metering_1)/1000,
                   Laundry=sum(Sub_metering_2)/1000,
                   WaterHeaterAC=sum(Sub_metering_3)/1000,
                   REST=sum(Residual)/1000)
X <- rownames(weeklypower)
weeklypower <- cbind(X=X, weeklypower)

saveRDS(weeklypower, file = "./Data/weeklypower.rds")

####groupbyDAY###
dailypower<- DataClean%>%
  group_by(year, month, week, day)  %>%
  dplyr::summarize(GAP = sum(Global_active_power)/1000, 
                   SM = sum(SM)/1000,
                   Kitchen=sum(Sub_metering_1)/1000,
                   Laundry=sum(Sub_metering_2)/1000,
                   WaterHeaterAC=sum(Sub_metering_3)/1000,
                   REST=sum(Residual)/1000
  )

X <- rownames(dailypower)
dailypower <- cbind(X=X, dailypower)
saveRDS(dailypower, file = "./Data/dailypower.rds")


#IF NECESSARY Subset to one sample per week  Mondays at 8:00pm 
#house070809weekly <- filter(DataClean, weekday == "martes" & hour == 20 & minute == 1)


#DATA<- dailypower
# DATA<- weeklypower
 DATA<- monthlypower

#frequency byweek=53, byday=365
#f<-365
#f<-53
f<-12

#### 2.SUBSETTING & TIME SERIES ####

## Create TimeSeries FOR ALL SUBMETERS, REST and GENERAL POWER
tsREST <- ts(DATA$REST, frequency=f, start=c(2007,1))
tsGAP <- ts(DATA$GAP, frequency=f, start=c(2007,1))
tsKITCHEN <- ts(DATA$Kitchen, frequency=f, start=c(2007,1))
tsLAUNDRY <- ts(DATA$Laundry, frequency=f, start=c(2007,1))
tsAC <- ts(DATA$WaterHeaterAC, frequency=f, start=c(2007,1))

plot.ts(tsGAP)
plot.ts(tsREST)

#### 2.2 TS METRICS ####

tsMETRICS <- tsREST

TS_DECOMPOSE<- decompose(tsMETRICS)
autoplot(TS_DECOMPOSE)

#checking weights
DEC1<-stl(tsGAP, s.window="periodic")
apply(DEC1$time.series, 2 ,var)/var(tsGAP)

adjusted<- tsGAP - TS_DECOMPOSE$seasonal
autoplot(adjusted)

tail(adjusted)

####3. TEST & TRAINING####
#training=07-09 testing=2010

tsWINDOW <- tsGAP
train<-window(tsGAP, start=c(2007,1), end=c(2009, 12))
test<- window(tsGAP, start=c(2010,1), end=c(2010, 11))
h<-length(test)




#### 4. FORECASTING#####

#HOLTWINTERS
HW<-HoltWinters(train)
HW_Forecast <-forecast:::forecast.HoltWinters(HW, h=length(test))
checkresiduals(HW_Forecast)

accuracy(HW_Forecast)


#arima
ARIMA<-forecast(auto.arima(train, lambda=0),h=length(test))
checkresiduals(ARIMA)

accuracy(ARIMA)

#OTHER MODELS
ETS <- forecast(stlf(train), h=h)
ARIMA <- forecast(auto.arima(train, lambda=0), h=h)

X <- cbind(ETS=ETS$mean, ARIMA=ARIMA$mean, HoltWinters=HW_Forecast$mean)
df <- cbind(test, X)
colnames(df) <- c("Data","ETS","ARIMA","HoltWinters")

autoplot(df) +
  xlab("Year") + ylab(expression("Global Active Power"))


#MIXTURE OF THE MODELS
library(opera)
MLpol0 <- mixture(model = "MLpol", loss.type = "square")
weights <- predict(MLpol0, X, test, type='weights')
z <- ts(predict(MLpol0, X, test, type='response'), start=c(2010,1), freq=12)
dfMIX <- cbind(test, z)
colnames(dfMIX) <- c("Data","Mixture")
autoplot(dfMIX) +
  xlab("Year") + ylab(expression("Global Active Power"))

accuracy(ARIMA, test)

#Forecast Plots
autoplot(tsGAP)+
  autolayer(ARIMA$mean)+
  autolayer(HW_Forecast$mean)




########################################################################################


####5.DECOMPOSING#####


# Decompose into trend, seasonal and remainder
decomposed_ts_month <- stl(tsGAP, s.window = "periodic")
decomposed_ts_month

ggplot2::autoplot(decomposed_ts_month)


## Check variance
variance_ts_month<-apply(decomposed_ts_month$time.series,2,var)/var(tsGAP)
variance_ts_month

#create training and test sets

train_month<-window (tsGAP,start=c(2007,1), end=c(2010,1))
test_month <- window(tsGAP,start=c(2010,2))

##create a Holt Winters model
HW_month <- HoltWinters(train_month)

#check residuals for the model
plot(HW_month)
checkresiduals(HW_month)

#make forecasts
forecast_HW_month <- forecast:::forecast.HoltWinters(HW_month,h=20)

#plot time series with the forecasts
autoplot(tsGAP) + 
   autolayer(forecast_HW_month$mean)

#check accuracy
HW_accuracy_month<-accuracy(forecast_HW_month,test_month)

##create an ARIMA model
AR_month<-auto.arima(train_month)

#check residuals for the model
plot(AR_month)
checkresiduals(AR_month)

##make forecasts
forecast_AR_month<-forecast:::forecast(AR_month,h=9)



#check accuracy
AR_accuracy_month<-accuracy(forecast_AR_month,test_month)
AR_accuracy_month


