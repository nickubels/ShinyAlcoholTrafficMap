library(shiny)
library(WHO)
library(dplyr)
library(stringr)
library(ggplot2)
library(leaflet)
source("key.r")
library(jsonlite)
codes <- get_codes()
RoadSafety <- codes[grepl("^RS",codes$label), ]
alcohollawexistance <- get_data("RS_204")
percalcodeaths <- get_data("RS_208")
# From http://data.okfn.org/data/datasets/geo-boundaries-world-110m
geojson <- readLines("~/Development/ShinyApp/countries.geojson", warn = FALSE) %>%
  paste(collapse = "\n") %>%
  fromJSON(simplifyVector = FALSE)
geojson$style = list(
  weight = 1,
  color = "#555555",
  opacity = 1,
  fillOpacity = 0.8
)
# Doing some nasty things to make this data work
percalcodeaths$value[percalcodeaths$country == "United Kingdom of Great Britain and Northern Ireland"] <- 17.0
percalcodeaths$value[percalcodeaths$country == "Bosnia and Herzegovina"] <- 15.0
percalcodeaths$value[percalcodeaths$country == "Spain"] <- 17.0
percalcodeaths$value <- as.numeric(percalcodeaths$value)
colours <- colorQuantile("Greens", percalcodeaths$value)
#orderedAlcPer <- percalcodeaths[order(percalcodeaths$country)]
#orderedAlcLaw <- percalcodeaths[order(alcohollawexistance$country)]

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
        urlTemplate = mbox,
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 30, lat = 31, zoom = 2) %>%
      addGeoJSON(geojson)
  })
}
shinyApp(ui = ui, server = server)
