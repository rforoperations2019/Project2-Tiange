library(plotly)
library(rgdal) 
library(shinydashboard)
library(reshape2)
library(shinythemes)
library(DT)
library(ggplot2)
library(tools)
library(stringr)
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(jsonlite)

#import and clean data
text<-na.omit(read.csv("/Users/tiangeyang/Desktop/R Shiny/Texting_Zone_Locations.csv"))
hudson<-na.omit(read.csv("/Users/tiangeyang/Desktop/R Shiny/Hudson_River_Park_Public_Facilities___Points_of_Interest.csv"))
golf<-na.omit(read.csv("/Users/tiangeyang/Desktop/R Shiny/Golf_Courses.csv"))
fishing<-na.omit(read.csv("/Users/tiangeyang/Desktop/R Shiny/Recommended_Fishing_Lakes_and_Ponds.csv"))
# Avoid plotly issues 
pdf(NULL)

# Application header & title
header <- dashboardHeader(title = "Some interesting places in New York", titleWidth = 400)

# Sidebar
sidebar <- dashboardSidebar(width = 250,
                            sidebarMenu(
                                id = "tabs",
                                
                                # Menu Items ----------------------------------------------
                                menuItem("Map", icon = icon("map-pin"), tabName = "map"),
                                menuItem("Chart", icon = icon("bar-chart"), tabName = "chart"),
                                menuItem("Table", icon = icon("table"), tabName = "table"),
                                
                                # Inputs: select county to find location for golf court
                                selectInput(inputId = "county",
                                            label = "Select county to find location",
                                            choices = sort(unique(golf$County)),
                                            selected = "Suffolk"),
                                
                                # Inputs: select boat launch type to plot for fishing places
                                checkboxGroupInput(inputId = "type",
                                                   label = "Select operation type to view",
                                                   choices = sort(unique(fishing$Boat.Launch.Type)),
                                                   selected = c("Hand Launch","Trailer Improved")),
                                
                                #   # Select sample size 
                                numericInput(inputId = "n_samp", 
                                             label = "Select number to view:", 
                                             min = 1, max = nrow(text), 
                                             value = 50),
                                
                                # Download Button--------------------------------------------------------
                                downloadButton("downloadData", "Download Data Here")
                            )
)
# Dashboard Window
body <- dashboardBody(tabItems(
    
    # Map 
    tabItem("map",

            fluidRow(
                tabBox(width = 14,
                       leafletOutput("It Is A Map")
                ))),
    
    # Graph 
    tabItem("chart",
           
            fluidRow(
                tabBox(width = 14,
                       tabPanel("size of fishing area", plotlyOutput("plot fishing area")))
                
            )),
    
   
    tabItem("table",
            fluidPage(
                box(title = "It Is A Datatable", DT::dataTableOutput("table"), width = 14)))
)
)


ui <- dashboardPage(header, sidebar, body, skin = "purple")

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
    
    # Reactive data function for golf
    golf.subset <- reactive({
        
        #District Filter
        golf <- subset(golf, County %in% input$county)
        
        # Return dataframe ----------------------------------------------
        return(golf)
    })
    
    # Reactive data function for fishing
    fishing.subset <- reactive({
        
        #District Filter
        fishing <- subset(fishing, Boat.Launch.Type%in% input$type)
        
        # Return dataframe ----------------------------------------------
        return(fishing)
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)  