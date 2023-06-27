#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(shinycssloaders)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(dplyr)
library(ggplot2)
library(shinythemes)
#  --------------------------------------------------------------------------------------------------------
#                                              READING THE FILES
#  --------------------------------------------------------------------------------------------------------
title <- tags$img("EDM PROJECT")
Datafinal= read.csv("/Users/jacoblopez/Documents/CIENCIA DE DATOS/TERCERO/EDM/PROYECTO/edm5.csv")
Datafinal$Date = as.Date(Datafinal$Date,"%d-%m-%Y")
Datafinal <- mutate(Datafinal, Year = format(Date,"%Y"))
Year<-unique(Datafinal$Year)
City<-unique(Datafinal$City)


# ---------------------------------------------------------------------------------------------------------
#                                                USER INTERFACE
# ---------------------------------------------------------------------------------------------------------

ui <- dashboardPage(
  skin = 'blue',
  dashboardHeader(title = title),
  dashboardSidebar(
    sidebarMenu(
      menuItem(("Map Distribution"),tabName = "dashboard",icon=icon('map')),
      menuItem(("Line graph"), tabName = "Line_graph",icon = icon('chart-line')),
      menuItem(("Pollutant trends"), tabName = "year_data", icon = icon('th')),
      menuItem("Raw data",tabName = "table",icon = icon('table'))
    )
  ),
  
  dashboardBody(
    #custom css
    tags$head(
      tags$style(
        HTML(" #compare_state_option,#compare_year_option ,.compare-county-wrapper { display:flex; margin-bottom:-10px;}
    				#compare_state_option > div, #compare_year_option > div, .compare-county-wrapper > div > div {padding-right: 15px;}
    				.shiny-output-error:before,.shiny-output-error{visibility: hidden !important;}
    				.compare-class > .tabbable > .nav-tabs {margin-bottom: 20px !important;}
    				.box.box-solid.box-primary {border: 1px solid #3088bf !important;}
    				.box.box-solid.box-primary>.box-header { background: #3088bf !important; background-color: #3088bf !important; }
    				.sidebar-menu>li {font-size:17px;}")
      )
    ),
    #--------------------------------------------RAW DATA TAB---------------------------------------------------------
    tabItems(
      tabItem(tabName = "table",
              tags$h3('View Data'),
                  br(),
              br(),
              # box(title = "Dataset",solidHeader = TRUE,status = "primary",height="100%", width =12,
              tableOutput("tableData")),

   
      
      # -------------------------------------------------MAP AND BAR GRAPH TAB------------------------------------------------------------------ 
      tabItem(tabName = "dashboard",
              fluidRow(
                column(3,
                       dateInput("select_date",
                                 h3("Select Date"),
                                 format = "yyyy-mm-dd",
                                 value="2019-01-01",
                                 min="2019-01-01",
                                 max="2022-12-31"))),
              fluidRow(
                box(title = "Map",solidHeader = TRUE, status = "primary",height=650,
                    width = 5,leafletOutput(height = 590,"map")),
                box(title = "Pollutants Distribution",solidHeader = TRUE, status = "primary",width = 7,
                    tabBox(width=12,
                           tabPanel(title="AQI",plotOutput(height = 500, "AQI")),
                           tabPanel(title="PM2_5",plotOutput(height = 500,"PM2_5")),
                           tabPanel(title="PM10",plotOutput(height = 500,"PM10")),
                           tabPanel(title="NO",plotOutput(height = 500,"NO")),
                           tabPanel(title="NO2",plotOutput(height = 500, "NO2")),
                           tabPanel(title="NH3",plotOutput(height = 500, "NH3")),
                           tabPanel(title="CO",plotOutput(height = 500, "CO")),
                           tabPanel(title="SO2",plotOutput(height = 500, "SO2")),
                           tabPanel(title="O3",plotOutput(height = 500, "O3"))
                    ))),
      ),
      # ---------------------------------------------------------------LINEGRAPH TAB---------------------------------------------------------
      tabItem(tabName = "Line_graph",
              fluidRow(
                column(3,
                       box(title = "Inputs",solidHeader = TRUE, status = "primary", width =12,height=600,
                           selectInput("Cities1",h3("Choose a Station"),City,selected = 'Politecnico'),
                           
                           dateInput("start_date",h3("From"),
                                     format = "yyyy-mm-dd",
                                     value="2019-01-01",
                                     min="2019-01-01",
                                     max="2022-12-31"),
                           
                           
                           dateInput("end_date",h3("To"),
                                     format="yyyy-mm-dd",
                                     value="2019-01-07",
                                     min="2019-01-01",
                                     max="2022-12-31")
                           
                           
                           
                           
                       )    
                ),
                
                
                column(9,
                       box(title = "Line Graph",solidHeader=TRUE, status = "primary",width = 12,height=600,
                           box(width=12,plotOutput(height = 500,"plots"))),
                       
                ))),     
      
      
      # -----------------------------------------------------------CORRELATION MATRIX TAB--------------------------------------------------    
      tabItem(tabName = "year_data",
              fluidRow(column(4,selectInput("Cities", ("Choose a Station:"),City,selected = 'Avda.Francia')),
                       column(8,selectInput("years",("Choose a Year:"),Year,selected="2019"))),
              fluidRow(
                column(6,
                       box(title = "Correlation matrix", solidHeader = TRUE, status = "primary", width = 55,
                           tabsetPanel(
                             tabPanel("CORRELATION COEFFICIENTS", withSpinner(plotOutput("corrcoeff",height = 800)))
                                                       )
                       )	
                       
                )
              ))
    ),
    
    
  )
)

# -------------------------------------------------------------------------------------------------------------------------------------
#                                                   SERVER
# -------------------------------------------------------------------------------------------------------------------------------------

server <- function(input, output) {
  

  # --------------------------------------------------------CORRELATION MATRIX ----------------------------------------------------------    
  
  output$corrcoeff <- renderPlot({
    mydata2 <- Datafinal %>%filter(Year==input$years, City==input$Cities)
    mydata<-mydata2[,c(3:11)]
    mydata.rcorr = rcorr(as.matrix(mydata))
    mydata.coeff = mydata.rcorr$r
    corrplot(mydata.coeff,method="number")
  })
  

  
  
  # ------------------------------------------------------TAB1--------------------------------------------------    
  output$AQI<-renderPlot({
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=City, y=AQI, fill=AQI_Bucket))
    df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  })
  
  # ----------------------------------------------------MAP FOR AQI--------------------------------------------------    
  
  output$map<-renderLeaflet({
    
    # filtering the data according to the date selected
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    
    # mutating the data with the popup info for easy access.
    Day<-Day %>% 
      mutate(popup_Info=paste("City: ",City,"</br>","AQI: ",AQI,"</br>","Condition: ",AQI_Bucket))
    
    # gradient based on the AQI level
    colour<-c("green","red")
    
    # creating a pallet out of it
    pal<-colorFactor(colour,Datafinal$AQI)
    
    # sending the data to the leaflet map to be rendered
    # the markers are provided the pallet colour
    leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(data=Day, lat=~Latitude, lng =~Longitude, 
                       radius = 20, popup = ~popup_Info, color = ~pal(AQI))
    
    
  })
  
  # ----------------------------------------------------BAR GRAPHS FOR AQI--------------------------------------------------    
  
  output$PM2_5<-renderPlot({
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=City, y=PM2.5, fill=AQI_Bucket))
    df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  })
  
  output$PM10<-renderPlot({
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=City, y=PM10, fill=AQI_Bucket))
    df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  })
  
  output$NO<-renderPlot({
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=City, y=NO, fill=AQI_Bucket))
    df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  })
  
  output$NO2<-renderPlot({
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=City, y=NO2, fill=AQI_Bucket))
    df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  })
  
  output$NH3<-renderPlot({
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=City, y=NH3, fill=AQI_Bucket))
    df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  })
  
  output$CO<-renderPlot({
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=City, y=CO, fill=AQI_Bucket))
    df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  })
  
  output$SO2<-renderPlot({
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=City, y=SO2, fill=AQI_Bucket))
    df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  })
  
  output$O3<-renderPlot({
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=City, y=O3, fill=AQI_Bucket))
    df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  })
  
  # ----------------------------------------TAB3---------------------------------------------------------------------     
  # ----------------------------------------LINE GRAPHS------------------------------------------------------------- 
  
  output$plots <- renderPlot({
    Datafinal$Date <- as.Date(Datafinal$Date)
    week_new <- Datafinal[,c(1:10)]
    week_new <- filter(week_new,between(Date, as.Date(input$start_date), as.Date(input$end_date)))
    week_city <- filter(week_new,City==input$Cities1)
    
    plot(week_city$CO,type="b",lwd=2,
         xaxt="n",ylim=c(0,120),col="blue",
         xlab="Date",ylab="values",
         main = input$Cities1)
    
    axis(1,at=1:length(week_city$Date),labels=week_city$Date)
    lines(week_city$NO2,col="red",type="b",lwd=2)
    lines(week_city$NH3,col="orange",type="b",lwd=2)
    lines(week_city$NO,col="purple",type="b",lwd=2)
    lines(week_city$O3,col="grey",type="b",lwd=2)
    lines(week_city$PM2.5,col="green",type = "b",lwd=2)
    lines(week_city$PM10,col="brown",type = "b",lwd=2)
    lines(week_city$SO2,col="violet",type = "b",lwd=2)
    
    
    
    legend("topright",legend=c("CO","NO2","NH3","NO","O3","PM2.5","PM10","SO2
                             "),
           lty=5,lwd=4,pch=10,col=c("blue","red","orange","purple","grey","green","brown","violet"),
           ncol=2,bty="n",cex=0.8,
           text.col=c("blue","red","orange","purple","grey","green","brown","violet")
    )
  })
  #------------------------------------------------------------TAB5------------------------------------------------------------
  #------------------------------------------------------------RAW DATA------------------------------------------------------------
  
  
  output$tableData <- renderTable(
    head(Datafinal,200),width = "100%"
  )
  
}
# ------------------------------------------------------------RUNNING THE PROJECT--------------------------------------------------     
shinyApp(ui = ui, server = server)