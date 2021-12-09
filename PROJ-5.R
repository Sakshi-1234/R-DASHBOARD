library(shiny)
library(htmlwidgets)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(leaflet)
library(lingtypology)
library(plotly)
library(ggstream)
#https://data.world/dataremixed/language-difficulty-ranking/workspace/file?filename=Foreign+Service+Institute+language+assignments+to+3S%2F3R.xlsx
table=read.csv("C:/SPB_Data/LANGUAGE-12.csv",TRUE,",")
table
d<-read.csv("C:/SPB_Data/data.csv")
nrow(table)
ncol(table)
map.feature(c(table$Language))
df <-data.frame(language =c(table$Language),
                features = c(table$Features))
df
map.feature(languages = df$language,
            features = df$features)
recommendation <- read.csv('C:/SPB_Data/LANGUAGE-12.csv',stringsAsFactors = F,header=T)
head(recommendation)
nrow(recommendation)
header <- dashboardHeader(title = "LANGUAGES DASHBOARD")  
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Interactive Plots", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Geographical Plots", tabName="dashboard2",icon = icon("send",lib='glyphicon')
    )
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
              valueBoxOutput("value1")
              ,valueBoxOutput("value2")
              ,valueBoxOutput("value3")
            ),
            fluidRow( 
              box(
                title = "BAR GRAPH"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("GRAPH1", height = "300px")
                ,box(
                  title = "BAR GRAPH WITH POLAR COORDINATE SYSYEM"
                  ,status = "primary"
                  ,solidHeader = TRUE 
                  ,collapsible = TRUE 
                  ,plotOutput("GRAPH2", height = "300px")
                )
              )
              ,box(
                title = "SCATTERPLOT"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotlyOutput('GRAPH3', height = "600px")
              )
            ),
            fluidRow( 
              box(
                title = "GROUPED BAR CHART"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotlyOutput("GRAPH4", height = "300px")
                ,box(
                  title = "STACKED BAR CHART"
                  ,status = "primary"
                  ,solidHeader = TRUE 
                  ,collapsible = TRUE 
                  ,plotlyOutput("GRAPH5", height = "300px")
                )
              )
              ,box(
                title = "BOX PLOT"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotlyOutput("GRAPH11", height = "600px")
              )
            ),
            fluidRow(
              box(
                title = "PIE-CHART"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotlyOutput("GRAPH7", height = "300px")
                ,box(
                  title = "BULL'S-EYE"
                  ,status = "primary"
                  ,solidHeader = TRUE 
                  ,collapsible = TRUE 
                  ,plotOutput("GRAPH8", height = "300px")
                )
              )
              ,box(
                title = "HISTOGRAM"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput('GRAPH9', height = "300px")
                ,box(
                  title = "HISTOGRAM IN POLAR COORDINATES"
                  ,status = "primary"
                  ,solidHeader = TRUE 
                  ,collapsible = TRUE 
                  ,plotOutput("GRAPH10", height = "300px")
                )
              )
            ),
            fluidRow( 
              box(
                title = "VIOLIN PLOT"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotlyOutput("GRAPH12", height = "600px")
              )
              ,box(
                title = "BAR-CHART"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput('GRAPH6', height = "300px")
              )
            )
    ),
    tabItem(tabName ="dashboard2",
            fluidPage(
              titlePanel("Languages"),
              mainPanel(leafletOutput("map"))
            )
            )
  )
)

ui <- dashboardPage(title = 'DV Project', header, sidebar, body, skin='blue')
server <- function(input, output) { 
  total.languages <- nrow(table)
  output$value1 <- renderValueBox({
    valueBox(
      formatC(total.languages, format="d", big.mark=',')
      ,paste('Total Languages',total.languages)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  output$map <- renderLeaflet({
    d %>%
      leaflet(width="100%")%>%
      addTiles()%>%
      setView(-78.8310,35.9867,zoom=7)%>%
      addMarkers(lat=~Latitude,lng=~Longitude,popup=d$country)
  })
  output$GRAPH1 <- renderPlot({
    bar<-ggplot(d,aes(x=endangerment,fill=endangerment))+geom_bar(width=1)
    bar
   
  })
  output$GRAPH2 <- renderPlot({
    bar<-ggplot(d,aes(x=endangerment,fill=endangerment))+geom_bar(width=1)
    bar
    bar+coord_polar(theta="x")
  })
  output$GRAPH3 <- renderPlotly({
    scatterplot<-ggplot(data=d ,aes(x=EXPERTS, y=speakers,color=endangerment))+geom_point() 
    scatterplot 
  })
  output$GRAPH4 <- renderPlotly({
    GRAPH4<-plot_ly(d,x=d$endangerment,y=d$EXPERTS,type="bar",name="EXPERTS")%>%add_trace(y=d$speakers,name="SPEAKERS")%>%layout(barmode="group")
  })
  output$GRAPH5 <- renderPlotly({
    GRAPH5<-plot_ly(d,x=d$endangerment,y=d$EXPERTS,type="bar")%>%add_trace(y=d$speakers)%>%layout(barmode="stack")
  })
  output$GRAPH6 <- renderPlot({
    bar<-ggplot(table,aes(x=Title,fill=Title))+geom_bar(width=1)
    bar
  })
  output$GRAPH7 <- renderPlotly({
    df2<-data.frame(d$EXPERTS,d$endangerment)
    df2%>%plot_ly(type="pie",labels=d$endangerment,values=d$EXPERTS,textinfo='lab
el+percent')
  })
  output$GRAPH8 <- renderPlot({
    base<-ggplot(d,aes(factor(1),fill=factor(endangerment)))+geom_bar(width=1)+
      scale_x_discrete(NULL,expand=c(0,0))+
      scale_y_continuous(NULL,expand=c(0,0))
    base+coord_polar(theta="y")
    base+coord_polar()
  })
  output$GRAPH9 <- renderPlot({
    #d2<-d[sample(nrow(d),500),]
    hist<-ggplot(table,aes(x=weeks,fill=Title))+geom_histogram(binwidth = 15)
    hist
  })
  output$GRAPH10 <- renderPlot({
    #d2<-d[sample(nrow(d),500),]
    hist<-ggplot(table,aes(x=weeks,fill=Title))+geom_histogram(binwidth = 15)
    hist
    hist+coord_polar()
  })
  output$GRAPH11 <- renderPlotly({
    fig<-plot_ly(y=table$weeks,type="box",name="weeks")
    fig<-fig%>%add_trace(y=table$hours,name="hours")
    fig
  })
  output$GRAPH12 <- renderPlotly({
    fig <- d %>%plot_ly(x = ~endangerment,y = ~EXPERTS,split = ~endangerment,type = 'violin',box = list(visible = T),
        meanline = list(visible = T)) 
    fig <- fig %>%layout(xaxis = list(title = "endangerment"),yaxis = list(title = "EXPERTS",zeroline = F))
    fig
  })
}
shinyApp(ui, server)
