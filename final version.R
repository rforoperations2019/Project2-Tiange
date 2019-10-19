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
resi<-readOGR("http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nyldgma/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson")
water<-na.omit(read.csv("/Users/tiangeyang/Desktop/project2.tiange/Water_Withdrawals_by_Facility___Beginning_2009.csv"))
str(water)
# Avoid plotly issues 
pdf(NULL)

# Application header & title
header <- dashboardHeader(title = "NYC Water Withdrawal Status Que", titleWidth = 400,
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "water withdrawal is processing", 
                                                        icon = icon("users"))
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "info",
                                       taskItem(value = 80, color = "red",
                                                "Midichlorians")
                          ),
                          dropdownMenu(type = "messages",
                                       messageItem(
                                           from = "NYC DATA CENTER",
                                           message = HTML("WE HAVE A LOT TO DO"),
                                           icon = icon("exclamation-circle"))))

# Sidebar
sidebar <- dashboardSidebar(width = 250,
                            sidebarMenu(
                                id = "tabs",
                                
                                # Menu Items ----------------------------------------------
                                menuItem("map", icon = icon("map-pin"), tabName = "map"),
                                menuItem("plot", icon = icon("bar-chart"), tabName = "plot"),
                                menuItem("tabele", icon = icon("table"), tabName = "table"),
                                
                                # Inputs: select county to find location
                                selectInput(inputId = "county",
                                            label = "Select county to find location",
                                            choices = sort(unique(water$County)),
                                            selected = "Albany"),
                                
                                # Inputs: select type to plot 
                                checkboxGroupInput(inputId = "type",
                                                   label = "Select operation type to view",
                                                   choices = sort(unique(water$Withdrawal.Type)),
                                                   selected = c("Surface Water","Groundwater")),
                                
                                
                                #   # Select sample size 
                                numericInput(inputId = "n_samp", 
                                             label = "Select number to view:", 
                                             min = 1, max = nrow(water), 
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
                       leafletOutput("map",width = "200%",
                                     height = 500)
                ))),
    
    # Graph 
    tabItem("plot",
           
            fluidRow(
                tabBox(width = 14,
                       tabPanel("chart to show Average Day Withdrawal and location", plotlyOutput("box")),
                       tabPanel("bar to show Average Day Withdrawal by year", plotlyOutput("his")))
                
            )),
    
   
    tabItem("table",
            fluidPage(
                box(title = "It Is A Datatable", DT::dataTableOutput("table"), width = 14)))
)
)


ui <- dashboardPage(header, sidebar, body, skin = "purple")

# Define server function 
server <- function(input, output) {
    
    # Reactive data function 
    water.subset <- reactive({
        
        # Filter
       water <- subset(water, County %in% input$county)
       water <- subset(water, Withdrawal.Type %in% input$type)
        
        # Return dataframe ----------------------------------------------
        return(water)
    })
    
    
    # my map
    
    output$map <- renderLeaflet({
        
        leaflet() %>%
            #add markers on base maps
            addProviderTiles("OpenStreetMap.Mapnik",group="omm") %>%
            addProviderTiles("Stamen.Toner.Lite",group="Toner.Lite")%>%
            addPolygons(data=resi,color="navy",group="resident",weight=2)%>%
            
            addLayersControl(
                baseGroups = c("World Imagery",  "Toner.Lite"),
                overlayGroups = ("resident"),
                options = layersControlOptions(collapsed = FALSE)
            )
        
    })
    
    # my plot 
    
    output$box <- renderPlotly({
        ggplot(water.subset(), 
               aes(x = Reporting.Year, 
                   y = Average.Day.Withdrawal..MGD.))+ 
            geom_boxplot() 
    })
    
    
    output$his <- renderPlotly({
        ggplot(data = water.subset(),
               aes(x = Withdrawal.Category, fill = Withdrawal.Category)) +
            geom_bar(stat = 'count') +
            labs (y = "Average.Day.Withdrawal..MGD.", x = "Withdrawal.Category ", fill = "Withdrawal.Category ") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    
    
    # My Data table 
    output$table <- DT::renderDataTable({
        ad <-subset(water.subset(),select = c(Town,County,Withdrawal.Category,Withdrawal.Type,Reporting.Year,Average.Day.Withdrawal..MGD.,Maximum.Day.Withdrawal..MGD.))
        DT::datatable(ad, options = list(scrollX = TRUE))
    })
    
    #Download function
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("NY water and fishing", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(water.subset(), file, row.names = FALSE)
        }
    )
    
}

# Run the application 

shinyApp(ui = ui, server = server)