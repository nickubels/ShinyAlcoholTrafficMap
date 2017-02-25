library(shiny)
library(WHO)
library(dplyr)
library(stringr)
library(leaflet)
source("~/Development/ShinyApp/key.r")
mbox <- mboxkey()
library(geojsonio)
library(countrycode)
library(htmltools)
library(googleVis)
#codes <- get_codes()
#RoadSafety <- codes[grepl("^RS",codes$label), ]
alcohollawexistance <- get_data("RS_204")
percalcodeaths <- get_data("RS_208")
defbybac <- get_data("RS_205")
deaths <- get_data("RS_198")
# From https://github.com/johan/world.geo.json/blob/master/countries.geo.json?short_path=afdfc39
geojson <- geojsonio::geojson_read("~/Development/ShinyApp/countries.geojson", what = "sp")

# Doing some nasty things to make this data work
percalcodeaths$value[percalcodeaths$country == "United Kingdom of Great Britain and Northern Ireland"] <- 17.0
percalcodeaths$value[percalcodeaths$country == "Bosnia and Herzegovina"] <- 15.0
percalcodeaths$value[percalcodeaths$country == "Spain"] <- 17.0
alcohollawexistance$value[alcohollawexistance$country == "Australia"] <- "Yes"
defbybac$value[defbybac$country == "Benin"] <- NA
percalcodeaths$value <- as.numeric(percalcodeaths$value)

colours <- colorBin("YlOrRd", percalcodeaths$value, na.color = "#808080", bins=10)
percalcodeaths$countrycode <- countrycode(percalcodeaths$country, "country.name", "wb", warn = TRUE) 
alcohollawexistance$countrycode <- countrycode(alcohollawexistance$country, "country.name", "wb", warn = TRUE) 
defbybac$countrycode <- countrycode(defbybac$country, "country.name", "wb", warn = TRUE)
deaths$countrycode <- countrycode(deaths$country, "country.name", "wb", warn = TRUE)

percalcodeaths <- percalcodeaths[order(percalcodeaths$country),]
alcohollawexistance <- alcohollawexistance[order(alcohollawexistance$country),]
defbybac <- defbybac[order(defbybac$country),]
deaths <- deaths[order(deaths$country),]

for (i in 1:length(alcohollawexistance$value))
{
  geojson$deaths[geojson$id == deaths$countrycode[i]] <- round((deaths$value[i]*(percalcodeaths$value[i]/100)))
  geojson$defbybac[geojson$id == defbybac$countrycode[i]] <- defbybac$value[i]
  geojson$perdeaths[geojson$id == percalcodeaths$countrycode[i]] <- percalcodeaths$value[i]
  geojson$law[geojson$id == alcohollawexistance$countrycode[i]] <- alcohollawexistance$value[i]
}


summarydata <- data.frame(c("Mean % of alcohol related traffic fatalities", "Max % of alcohol related traffic fatalities","Min % of alcohol related traffic fatalities"),
                          c(round(mean(percalcodeaths$value, na.rm=TRUE),1),max(percalcodeaths$value, na.rm = TRUE),min(percalcodeaths$value, na.rm = TRUE)))
names(summarydata) <- c('Description','Value')

ui <- bootstrapPage(
  theme = "flatly",
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                      leafletOutput("map", width = "100%", height = "100%"),
                      absolutePanel(top = 60, left = "auto", right = 20, bottom = "auto",
                                    width = 330, height = "auto",wellPanel(h3("Alcohol & traffic"),"This interactive map is made in RStudio with the Shiny and Leaflet packages. Data is collected from the WHO using the WHO package.",
                                    hr(),selectInput("select",label = h4("Select:"), choices = list("Summary" = 1), selected = 1),hr(),htmlOutput("plots")))
                      )
server <- function(input, output) {
  myOptions <- reactive({
    list(
      page='enable',
      pageSize=4,
      width=550
    )
  })
  output$plots <- renderGvis({
    if(input$select == 1){
      gvisTable(summarydata)
    }}
  )
  output$map <- renderLeaflet({
    leaflet(geojson) %>%
      addTiles( urlTemplate = mbox,
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
               ) %>%
      setView(lng = 30, lat = 31, zoom = 2) %>%
      addPolygons(color = "#444444", weight = 1,stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "blue", weight = 2,
                                                      bringToFront = TRUE),
                  fillColor = ~colours(perdeaths),
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
      addLegend(position = "bottomleft",pal = colours, values = ~perdeaths, opacity = 1.0,title = "% traffic fatalities related to alcohol", labFormat = labelFormat(suffix = "%"))
   })
}
shinyApp(ui = ui, server = server)
