# Made by Nick Ubels
# Shiny app UI functions
#Include the global code
source("global.r")

# Make the Shiny serverside
server <- function(input, output) {
  # Plot/table generator using Google Charts
  output$plots <- renderGvis({
    if(input$select == 1){
      gvisTable(summarydata)
    } else if(input$select == 2){
      gvisPieChart(lawplot, options=list(title="Existence of a national drunk-driving law"))
    } else if(input$select == 3){
      gvisPieChart(bacplot, options=list(title="Definition of drunk-driving by BAC"))
    }}
  )
  # Generate the map using Leaflet (Mapbox <3)
  output$map <- renderLeaflet({
    leaflet(geojson) %>%
      # Add Mapbox tiles and attribution, remove contents of addTiles to switch to default OSM
      addTiles( urlTemplate = mbox,
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      # Set center of map
      setView(lng = 30, lat = 31, zoom = 3) %>%
      # Adding the polygons from the geojson with on-hover HTML labels
      addPolygons(color = "#444444", weight = 1,stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "blue", weight = 2,bringToFront = TRUE),
                  fillColor = ~colours(perdeaths),
                  # Label magic from documentation
                  label = mapply(function(x,y,q,z,w) {
                    HTML(sprintf("<em>%s:</em><br>
                                 Traffic fatalities related to alcohol: %s&#37;<br>
                                 Est. deaths (per 100000): %s<br>
                                 Laws about drunk-driving: %s<br>
                                 Def. of drunk-driving by BAC: %s", htmlEscape(x),htmlEscape(y),htmlEscape(q),htmlEscape(z),htmlEscape(w)))},
                    geojson$name, geojson$perdeaths,geojson$deaths,geojson$law,geojson$defbybac, SIMPLIFY = F),
                  labelOptions = lapply(1:nrow(geojson), function(x) {
                    labelOptions(direction='auto')
                  })) %>%
      # Add legend
      addLegend(position = "bottomleft",
                pal = colours,
                values = ~perdeaths,
                opacity = 1.0,
                title = "% traffic fatalities related to alcohol",
                labFormat = labelFormat(suffix = "%")
                )
                  })
  }


