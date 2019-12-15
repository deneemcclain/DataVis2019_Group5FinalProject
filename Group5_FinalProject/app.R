#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(DT)
library(shiny)
library(dplyr)
library(leaflet)
library(sp)
library(ggmap)
library(tidyverse)
library(sf)
library(tidycensus)

#set working directory
#setwd("C:/Users/denee/Documents/University of Notre Dame Data Science/Fall 2019/Data Visualization/Assignments/Group5_FinalProject")
# load preprocessed data
load("lights_clean.RData")
load("buildings_clean.RData")
load("codeViolations_clean.RData")
load("markers_clean.RData")
load("CleanedBusinessed.Rdata")
load("SB_Zip_Data.Rdata")

# Create Pallette Objects for Plotting

# Options for Population
pal <- colorQuantile(palette = "viridis", domain = sb_zip_data$total.populationE)
pal2 <-colorNumeric(palette = "Reds", domain = sb_zip_data$total.populationE)
pal3 <- colorNumeric(palette = "plasma", domain = sb_zip_data$total.populationE)

# Options for Business Type Coloring
pal4 <- colorFactor(palette = 'Set1', domain = businesses.spatial$Category)

# Define UI for application that draws a histogram
ui <- navbarPage("South Bend Command Center",
   tabPanel("About", 
            mainPanel(
              div(
              h2("Guide to Use"),
              p("This dashboard provides a highlight of some
               potential problem areas within the city of South Bend, IN"),
              h3("Street Lights & Abandoned Buildings"),
              p("Use this tab to explore the relationship between street lights and
                   abandoned buildings in different parts of South Bend. This may help
                   to identify areas that might be at-risk for safety/dilapidated buildings."),
              h3("Population & Business Distribution"),
              p("Use this tab to get overview of where people & businesses are located throughout the city,
                   and explore relationships between these two variables that might highlight issues."),
              h3("Code Violation Explorer"),
              p("Use this tab to explore different Code Violations at establishments throughout the city.
                   This may help to identify parts of the city that are potentially neglected, indicated by
                   high concentration of code violations"),
              br(),
              h5("Authors: Courtney Cosgrove, Denee McClain, and Jay Richardson")
            ))),
   tabPanel("Distribution of Abandoned Buildings and Street Lights by Zip Code",
            sidebarLayout(
              sidebarPanel(
                selectInput("zipcode", "Zip Code", c("All", unique(lights$zipcodes))),
                selectInput("bulbtype", "Bulb Type", c("All", unique(lights$Bulb_Type))),
                selectInput("wattage", "Wattage", c("All", unique(lights$Wattage))),
                selectInput("poletype", "Pole Type", c("All", unique(as.character(lights$Pole_Type))))
              ),
              
              # Show a plot of the generated distribution
              mainPanel(
                plotOutput("abandonedLights")
              )
            )),
   tabPanel("Population & Business Distribution", 
            sidebarLayout(
              
              sidebarPanel(
                selectInput("zipselection",
                            "Select Zip Code:",
                            choices = c("All",unique(sb_zip_data$GEOID))
                ),
                selectInput("TypeSelection",
                            "Select Business Type:",
                            choices = c("All",unique(businesses.spatial$Category)))
              ), #End sidebar panel
              
              # Show a plot of the generated distribution
              mainPanel(
                leafletOutput("map")
              ) # End Main Panel
            )),
   tabPanel("City of South Bend - Code Enforcement Violations", 
            fluidPage(
              
              leafletOutput("mymap"),
              
              fluidRow(
                column(4,
                       selectInput("Case_Status_Code_Description",
                                   "Code Status:",
                                   c("All",
                                     unique(as.character(codeViolations$Case_Status_Code_Description))))
                ),
                column(4,
                       selectInput("Case_Month",
                                   "Month:",
                                   c("All",
                                     unique(as.character(codeViolations$Case_Month))))
                ),
                column(4,
                       selectInput("Zip_Code",
                                   "Zip Code:",
                                   c("All",
                                     unique(as.character(codeViolations$Zip_Code))))
                ),
                column(4,
                       selectInput("Case_Type_Code_Description",
                                   "Violation Type:",
                                   c("All",
                                     unique(as.character(codeViolations$Case_Type_Code_Description))))
                ),
                column(4,
                       selectInput("Case_Year",
                                   "Year:",
                                   c("All",
                                     unique(as.character(codeViolations$Case_Year))))
                )
              ),
              DT::dataTableOutput("table")
            ))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ## Abandoned Properties Tab
  output$abandonedLights <- renderPlot({
    if (input$zipcode == "All") {
      theselights <- lights
      thesebuildings <- buildings
    }
    else {
      theselights <- lights %>% filter(zipcodes == input$zipcode)
      thesebuildings <- buildings %>% filter(Zip_Code == input$zipcode)
    }
    
    if (input$bulbtype == "All") {
      theselights <- theselights
    }
    else {
      theselights <- theselights %>% filter(Bulb_Type == input$bulbtype)
    }
    
    if (input$wattage == "All") {
      theselights <- theselights
    }
    else {
      theselights <- theselights %>% filter(Wattage == input$wattage)
    }
    
    if (input$poletype == "All") {
      theselights <- theselights
    }
    else {
      theselights <- theselights %>% filter(Pole_Type == input$poletype)
    }
    # put count information into data frame
    lightsandbuildings <- data.frame(name = c("Abandoned Buildings", "Street Lights"),
                                     Count = c(nrow(thesebuildings), nrow(theselights)))
    
    # draw the histogram with the specified number of bins
    ggplot(data = lightsandbuildings, 
           aes(x = name, y = Count, fill = name)) + 
      geom_col(show.legend = F) + xlab("") + 
      scale_fill_manual(values = c("#9e5002", "#fcd303"))
  })
  ## Abandoned Properties Tab
  
  ## Code Enforcements Tab
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, options = providerTileOptions(
        updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
        updateWhenIdle = TRUE           # map won't load new tiles when panning
      )) %>%
      addMarkers(data = filtered(), clusterOptions = markerClusterOptions())
  })
  
  output$table <- DT::renderDataTable({
    filtered()
  })
  
  filtered <- reactive({
    temp <- codeViolations
    if (input$Case_Status_Code_Description != "All") {
      temp <- temp[temp$Case_Status_Code_Description == input$Case_Status_Code_Description,]
    }
    if (input$Case_Year != "All") {
      temp <- temp[temp$Case_Year == input$Case_Year,]
    }
    if (input$Case_Month != "All") {
      temp <- temp[temp$Case_Month == input$Case_Month,]
    }
    if (input$Zip_Code != "All") {
      temp <- temp[temp$Zip_Code == input$Zip_Code,]
    }
    if (input$Case_Type_Code_Description != "All") {
      temp <- temp[temp$Case_Type_Code_Description == input$Case_Type_Code_Description,]
    }
    return(temp)
  })
  
  observe({
    leafletProxy(mapId = "mymap", data = filtered()) %>%
      clearMarkers() %>%
      addMarkers(data = filtered(), clusterOptions = markerClusterOptions())
  })
  ## Code Enforcements Tab
  
  ## Business Info Tab
  output$map <- renderLeaflet({
    
    filtered_bus <- reactive({
      filtered_obj <- businesses.spatial
      if(input$zipselection != "All") {
        filtered_obj <- businesses.spatial[businesses.spatial$Zip_Code == input$zipselection,]
        if(input$TypeSelection != "All"){
          filtered_obj <- filtered_obj[filtered_obj$Category == input$TypeSelection,]
        }
        if(input$TypeSelection == "All"){
          filtered_obj <- businesses.spatial[businesses.spatial$Zip_Code == input$zipselection,]
        }
      }
      if(input$zipselection == "All"){
        filtered_obj <- businesses.spatial
        if(input$TypeSelection != "All"){
          filtered_obj <- filtered_obj[filtered_obj$Category == input$TypeSelection,]
        }
        if(input$TypeSelection == "All"){
          filtered_obj <- businesses.spatial
        }
      }
      return(filtered_obj)
    })
    
    filtered_map <- reactive({
      filtered_obj <- sb_zip_data
      if(input$zipselection != "All") {
        filtered_obj <- sb_zip_data[sb_zip_data$GEOID == input$zipselection,]
      }
      return(filtered_obj)
    })
    
    leaflet()  %>%
      addTiles(group = "Basic")  %>%
      addPolygons(data = filtered_map(),
                  group = "Population Fill",
                  stroke = T,
                  popup = ~popup,
                  color = ~ pal2(total.populationE),
                  fillOpacity = 0.7,
                  opacity = 1, 
                  fill = T, 
                  weight = 1) %>% 
      addPolylines(data = filtered_map(),
                   group = "ZipLines",
                   color = "black",
                   opacity = 1,
                   weight = 2) %>% 
      addCircleMarkers(data = filtered_bus(), 
                       popup = ~popup,
                       color = ~pal4(Category),
                       stroke = 0,
                       fillOpacity = 0.4, 
                       radius = 3,
                       group = "Businesses") %>% 
      addLegend("bottomright", 
                pal = pal2, 
                values = sb_zip_data$total.populationE,
                title = "Population",
                group = "Pop Legend") %>% 
      addLegend("bottomleft",
                pal = pal4,
                values = businesses.spatial$Category,
                title = "Business Types",
                group = "Business Legend") %>% 
      addLayersControl(
        overlayGroups= c("Population Fill","Businesses"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  ## Business Info Tab
}

# Run the application 
shinyApp(ui = ui, server = server)

