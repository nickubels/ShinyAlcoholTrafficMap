library(shiny)
library(WHO)
library(dplyr)
library(stringr)
library(ggplot2)
library(leaflet)
codes <- get_codes()
RoadSafey <- codes[grepl("^RS",codes$label), ]
alcohollawexistance <- get_data("RS_204")
percalcodeaths <- get_data("RS_208")
ui <- fluidPage(
  titlePanel("Alcohol and traffic around the world"),
  sidebarLayout(
    sidebarPanel("our inputs will go here"),
    mainPanel(leafletOutput("map"))
  )
)
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
}
shinyApp(ui = ui, server = server)
