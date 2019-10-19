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
fishing<-read.csv("/Users/tiangeyang/Desktop/R Shiny/Recommended_Fishing_Lakes_and_Ponds.csv")
water<-na.omit(read.csv("/Users/tiangeyang/Desktop/R Shiny/Water_Withdrawals_by_Facility___Beginning_2009.csv"))
str(water)
# Avoid plotly issues 
pdf(NULL)

# Application header & title
header <- dashboardHeader(title = "Some interesting places in New York", titleWidth = 400)

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
    pal<-colorNumeric(palette = "Blues",domain = as.numeric(na.omit(fishing$Acres_Mile)))
            leaflet(fishing) %>%
            addProviderTiles("OpenStreetMap.HOT", group = "osm.H") %>%
            #add a layer of points
            addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8, color = ~pal(Acres_Mile)) %>%
            #add legend
            addLegend(position = "topleft", 
                      pal = pal, 
                      values = fishing$Acres_Mile, 
                      title = "Fishing Area Arces Mile",
                      group='fishing') %>%
            
            
            #add layers control
            addLayersControl(
                baseGroups = c("osm.H"),
                overlayGroups = c("fishing"),
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