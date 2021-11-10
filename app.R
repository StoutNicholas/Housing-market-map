#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(tidyverse)
library(rgdal)
library(shinythemes)

data <- read.csv("~/shinymap/app/az_sources_merged.csv")

datacl <- data %>% 
    mutate(across(Apache:Yuma, as.numeric)) %>% 
    pivot_longer(cols = -c(Time,Source), names_to = 'County')

# county_data <- rgdal::readOGR('gz_2010_us_050_00_500k.json')
# save(county_data, file = 'county_data.Rdata')
load('county_data.Rdata')
data_map <- county_data


# Define UI for application that draws a histogram
ui <- fluidPage(
     theme = shinytheme("lumen"),
    # Application title
    titlePanel("Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
             selectInput('year','Year', choices = unique(datacl$Time)),
            # sliderInput('year','Year',value = min(unique(datacl$Time)),
            #             step = 1,
            #             min = min(unique(datacl$Time)), max = max(unique(datacl$Time))),
            selectInput('factor','Factor', choices = unique(datacl$Source)),
            selectInput('county','County',
                        choices = c('All',datacl$County))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput('map')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderLeaflet({
        data_map <- county_data
        
        dataj <- datacl %>% 
            filter(Time == input$year) %>% 
            filter(Source == input$factor)
        
        if(input$county != 'All'){
            dataj <- dataj %>% 
                filter(County == input$county)
        }
            
        data_map@data <- data_map@data %>% 
            left_join(
                dataj,
            by = c('NAME' = 'County' )
            ) %>% 
            mutate(value2 = paste(NAME, value))
        
        pal = colorNumeric('RdYlGn', NULL, reverse = T)
        
        leaflet(data_map) %>% 
            addProviderTiles(providers$CartoDB) %>% 
            setView(lat = 34.0489, lng = -111.0937, zoom = 6) %>% 
            addPolygons(label = ~value2, 
                        smoothFactor = 0.9,
                        fillColor = ~pal(value),
                        stroke = F,
                        highlightOptions = highlightOptions(
                            fillOpacity = 0.9, opacity = 0.9,
                            bringToFront = T, sendToBack = T
                        )
                        
                        ) %>% 
            addLegend('bottomleft', pal = pal,
                      values = ~value)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
