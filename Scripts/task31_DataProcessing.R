####0.LIBRARIES AND DIRECTORIES####
if(require("pacman")=="FALSE"){
  install.packages('pacman')
  library('pacman')
  pacman::p_load(here, stringr, readxl, plyr, caret, dplyr, doParallel,
                 lubridate, crayon, corrplot, ggplot2, e1071, reshape2, 
                 tidyverse, arules, arulesViz, rstudioapi, RMySQL, esquisse)
} else {
  library('pacman')
  pacman::p_load(here, stringr, readxl, plyr, caret, dplyr, doParallel,
                 lubridate, crayon, corrplot, ggplot2, e1071, reshape2, 
                 tidyverse, arules, arulesViz, rstudioapi,RMySQL, esquisse)
}

#setting up directory
current_path=getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)
list.files("Data/")
#household_power_consumption<-read.csv("./Data/household_power_consumption.txt")

####1.LOADING DATA #####
Data_COMPLETE <- read_delim("C:/Users/Sergi Ch/Downloads/UBIQUM/PROJECT 3/TASK 3.1/Data/household_power_consumption.txt",";", escape_double = FALSE, trim_ws = TRUE)

#Connect to server
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
dbListTables(con)
dbListFields(con, 'yr_2006') #check features

#####2.DIVIDING DATA#####

yr_2006<-dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3, Global_active_power FROM yr_2006")
yr_2007<-dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3, Global_active_power FROM yr_2007")
yr_2008<-dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3, Global_active_power FROM yr_2008")
yr_2009<-dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3, Global_active_power FROM yr_2009")
yr_2010<-dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3, Global_active_power FROM yr_2010")

#analyse year by year
summary(yr_2007)
head(yr_2007)
tail(yr_2007)

#Data with only FULL YEARS 07-09
Data_0709 <- bind_rows(yr_2007, yr_2008, yr_2009, yr_2010) #we ended up including 2010 also

####3.DATE PROCESSING#####

#Data with only time date and submeters
newData<-cbind(Data_0709, paste(Data_0709$Date, Data_0709$Time), stringsAsFactors=FALSE)

#set DateTime feature
names(newData)[length(newData)]<-"DateTime"
newData <- newData[,c(ncol(newData), 1:(ncol(newData)-1))]

#Set TimeZone
newData$DateTime <- as.POSIXct(newData$DateTime, "%Y/%m/%d %H:%M:%S")
attr(newData$DateTime, "tzone") <- "Europe/London" #May have to remove this line because it gave us problem

####4. MISSING VALUES####
library(padr)
#create NA for the missing rows
newData<-pad(newData, by="DateTime", break_above=2.5)
newData[!complete.cases(newData),]
#Show missing rows 
library(zoo)
newData<-replace(newData, TRUE, lapply(newData, na.aggregate))
sum(is.na(newData))

#create year, quarter, month, week, weekday, day, hour and minute
newData$year <- year(newData$DateTime)
newData$quarter <- quarter(newData$DateTime)
newData$month <- month(newData$DateTime)
newData$week <- week(newData$DateTime)
newData$weekday <-weekdays(newData$DateTime)
newData$ day <-  day(newData$DateTime)
newData$hour <- hour(newData$DateTime)
newData$minute <-  minute(newData$DateTime)

####5.. NEW FEATURES (SMtotal, Residual...)#####
#active energy consumed every minute (in watt hour)
newData$SM<- newData$Sub_metering_1 + newData$Sub_metering_2 + newData$Sub_metering_3
newData$Global_active_power<- ((newData$Global_active_power*1000)/60)
newData$Residual<- (newData$Global_active_power - newData$SM)

#PRICE RATES
newData$HM <- lubridate::hm(paste(newData$hour, newData$minute)) #auxiliar feature for different price rates
newData$Price<- ifelse(newData$HM>=hm("6H30M")&newData$HM<=hm("22H30M"),
                       0.158, 0.123) 
newData = newData[,!(names(newData) %in% c("HM"))] #Remove the HM colum
  
#Store the data
saveRDS(newData, file = "./Data/NewData.rds")


