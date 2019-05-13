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
pacman::p_load(rstudioapi, shiny, shinydashboard, DT, dplyr, highcharter)

#setting up directory
current_path=getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)
list.files("Data/")

##### Loading data####
#data_bydays<-readRDS("./Data/dailypower.rds")
data_bydays<- read.csv("./Data/data_bydays.csv")
data_byweeks<-read.csv("./Data/data_byweeks.csv")
data_bymonths<-read.csv("./Data/data_bymonths.csv")
DataClean<-readRDS("./Data/NewDataS.rds")

data_bydays$Date <-paste(data_bydays$Year,data_bydays$Month,data_bydays$Day,sep="-")%>%ymd()%>%as.Date()
data_bydays$REST <- data_bydays$ActiveEnergy -(data_bydays$Kitchen+data_bydays$Laundry+data_bydays$EWAC)


data_bymonths$REST <- data_bymonths$ActiveEnergy -(data_bymonths$Kitchen+data_bymonths$Laundry+data_bymonths$EWAC)

#REPLACE NOVEMBER WITH AVERAGE OF PREVIOUS YEARS
AvgNov<- data_bymonths %>% group_by(Month) %>% 
  filter (Year!= 2010 & Month == 11) %>% 
  summarise(AENov=mean(ActiveEnergy),
            KINov=mean(Kitchen),
            LANov=mean(Laundry),
            WACNov=mean(EWAC),
            RESTNov=mean(REST))

data_bymonths[47,4]<- AvgNov[1,2] #Replacing Active Energy
data_bymonths[47,6]<- AvgNov[1,3] #Replacing Kitchen
data_bymonths[47,7]<- AvgNov[1,4] #Replacing Laundry
data_bymonths[47,8]<- AvgNov[1,5] #Replacing WH/AC
data_bymonths[47,9]<- AvgNov[1,6] #Replacing REST


##### USER INTERFACE ####

# USER INTERFACE
ui <- dashboardPage (
  dashboardHeader(title="Energy consumption"),
  dashboardSidebar(
    sidebarMenu(
      selectInput(inputId = "Variable", label="Select Consumption From",
                  choices=c("ActiveEnergy","Kitchen","Laundry","EWAC","REST"))
    )
  ),
  dashboardBody
  (
    tabBox(id="tabset1", height="600px", width=12,
           tabPanel("Usage",
                   selectInput(
                          inputId =  "date_from",
                          label = "Select Year:",
                          choices = 2007:2010),
                        box(width = 12,  solidHeader = FALSE,
                            highchartOutput("plot1"))
           ),
                    tabPanel("Usage costs",
                             column(width=4,
                                    selectInput(
                                      inputId =  "date_from2",
                                      label = "Select year:",
                                      choices = 2007:2010),
                                    
                                    selectInput(
                                      inputId =  "date_month",
                                      label = "Select month:",
                                      choices = 1:12)
                             ),
                             infoBoxOutput("message1"),
                             infoBoxOutput("message2")
                             ),
                    tabPanel("Prediction", height="800px", width=12,
                             sliderInput("slider1", label = h3("Nº of months"), min = 0,
                                         max = 12, value = 1),
                             box( width = 12,  solidHeader = TRUE,
                                 highchartOutput("plot2")),
                             box(
                               verbatimTextOutput("messages"))
                    )
           )
    )
  
  )

  
  #### SERVER ####
  server <- function(input, output) {
    
    #REACTIVE FOR THE PLOT
    yeardata<-reactive({
      data_bydays %>% select (Variable=input$Variable, Date, Year, Month) %>% filter(Year == input$date_from)
    })
   
    #REACTIVE FOR THE COST
    costdata<-reactive({
      data_bydays %>% select (Variable=input$Variable, Year, Month, Day) %>%
        filter(Year==input$date_from2 & Month== input$date_month) %>% 
        summarize(sum=sum(Variable)*0.00015)
    })
    
    #REACTIVE FOR THE PREVIOUS MONTH
    costdata2<-reactive({
      if(input$date_month == 1){
        previousMonth<-12
        previousYear<- as.numeric(input$date_from2)-1
        
      }else {
        previousMonth<- as.numeric(input$date_month )-1
        previousYear<- as.numeric(input$date_from2)
      }
      data_bydays %>% select (Variable=input$Variable, Year, Month, Day) %>%
        filter(Year==previousYear & Month== previousMonth) %>% 
        summarize(sum=sum(Variable)*0.00015)
    })
    
    #PLOT1
    output$plot1 <- renderHighchart({
      dfhist<- yeardata()
      highchart(type="stock") %>% hc_add_series(dfhist, "line", hcaes(Date, Variable))
    })
    

    #FORECAST DATA FOR PLOT2
    monthdata<-reactive({
      data_bymonths %>% select (Variable=input$Variable, Year, Month) 
    })
    #PLOT2
    output$plot2 <- renderHighchart({
      energyforecast<- monthdata()
      ts <- ts(energyforecast$Variable, frequency=12, start=c(2007,1))
      energyforecast2 <- forecast(HoltWinters(ts), h=input$slider1, level = 95)
      hchart(energyforecast2)
    })
    #INFO BOX
    output$message1<-renderInfoBox({
      box<-costdata()
      infoBox("COST", paste0(round(box),"€"), icon=icon("fas fa-euro-sign"), color="green")
    })
    #INFO FOR THE ARROWS
    output$message2<- renderInfoBox({
      compar1<-costdata()
      compar2<- costdata2()
      
    compar<-compar1[,1]-compar2[,1]
    if(compar > 0){
      infoBox("COMPARED TO LAST MONTH", 
              icon=icon("fas fa-arrow-circle-up"), color = "red")
    }else{
        infoBox("COMPARED TO LAST MONTH", 
                icon=icon("fas fa-arrow-circle-down"))
      }
    })
    #PREDICTION SLIDER
    output$value <- renderPrint({ input$slider1 })
    
    #ERROR TEXT
    output$messages<-renderText({
      month<- monthdata()
      ts1 <- ts(month$Variable, frequency=12, start=c(2007,1))
      energyforecast3 <- forecast(HoltWinters(ts1), h=input$slider1, level = 95)
      print(paste0("Consumption Month ",  ":", sep="\n",  "[", 
                   round((energyforecast3$lower/1000),2),"-", round((energyforecast3$upper/1000),2), "]KW ||||  [", 
                   round((energyforecast3$lower/1000*0.15),2),"-", round((energyforecast3$upper/1000*0.15),2), "]€",
                   sep="\n"))
      
    })
    
  }

  
  #### RUNNING APP####
  shinyApp(ui, server)

  