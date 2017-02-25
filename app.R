library(shiny)
library(WHO)
library(dplyr)
library(stringr)
library(ggplot2)
library(leaflet)
source("~/Development/ShinyApp/key.r")
mbox <- mboxkey()
#library(jsonlite)
library(geojsonio)
library(countrycode)
#codes <- get_codes()
#RoadSafety <- codes[grepl("^RS",codes$label), ]
alcohollawexistance <- get_data("RS_204")
percalcodeaths <- get_data("RS_208")
# From https://github.com/johan/world.geo.json/blob/master/countries.geo.json?short_path=afdfc39
geojson <- geojsonio::geojson_read("~/Development/ShinyApp/countries.geojson", what = "sp")

# Doing some nasty things to make this data work
percalcodeaths$value[percalcodeaths$country == "United Kingdom of Great Britain and Northern Ireland"] <- 17.0
percalcodeaths$value[percalcodeaths$country == "Bosnia and Herzegovina"] <- 15.0
percalcodeaths$value[percalcodeaths$country == "Spain"] <- 17.0
percalcodeaths$value <- as.numeric(percalcodeaths$value)
colours <- colorBin("YlOrRd", percalcodeaths$value, na.color = "#808080", bins=10)
percalcodeaths$countrycode <- countrycode(percalcodeaths$country, "country.name", "wb", warn = TRUE) 
alcohollawexistance$countrycode <- countrycode(alcohollawexistance$country, "country.name", "wb", warn = TRUE) 
for (i in 1:length(alcohollawexistance$value))
{
  geojson$perdeaths[geojson$id == percalcodeaths$countrycode[i]] <- percalcodeaths$value[i]
  geojson$law[geojson$id == alcohollawexistance$countrycode[i]] <- alcohollawexistance$value[i]
}

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                      leafletOutput("map", width = "100%", height = "100%"),
                      absolutePanel(top = 60, left = "auto", right = 20, bottom = "auto",
                                    width = 330, height = "auto",wellPanel(h3("Alcohol & traffic")))
                      )
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet(geojson) %>%
      addTiles( urlTemplate = mbox,
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
               ) %>%
      setView(lng = 30, lat = 31, zoom = 2) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
                  fillColor = ~colours(perdeaths),
                  label = mapply(function(x, y) {
                    HTML(sprintf("<em>%s:</em><br>Traffic fatalities related to alcohol: %s&#37;", htmlEscape(x), htmlEscape(y)))},
                    geojson$name, geojson$perdeaths, SIMPLIFY = F),
                  labelOptions = lapply(1:nrow(geojson), function(x) {
                    labelOptions(direction='auto')
                   })) %>%
      addLegend(position = "bottomleft",pal = colours, values = ~perdeaths, opacity = 1.0,title = "% traffic fatalities related to alcohol", labFormat = labelFormat(suffix = "%"))
   })
}
shinyApp(ui = ui, server = server)
