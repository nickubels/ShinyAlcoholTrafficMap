# Made by Nick Ubels

# Loading dependencies
library(shiny)
library(WHO)
library(dplyr)
library(stringr)
library(leaflet)
library(geojsonio)
library(countrycode)
library(htmltools)
library(googleVis)

# Fetching the MapBox secret. Change path to your own personal path
source("~/Development/ShinyApp/key.r")
mbox <- mboxkey()

# Fetch data from WHO
alcohollawexistance <- get_data("RS_204")
percalcodeaths <- get_data("RS_208")
defbybac <- get_data("RS_205")
deaths <- get_data("RS_198")


# From https://github.com/johan/world.geo.json/blob/master/countries.geo.json?short_path=afdfc39
# Please do not forget to change the URL
geojson <- geojson_read("~/Development/ShinyApp/countries.geojson", what = "sp")

# Cleaning some data to make it usable for our purposes
percalcodeaths$value[percalcodeaths$country == "United Kingdom of Great Britain and Northern Ireland"] <- 17.0
percalcodeaths$value[percalcodeaths$country == "Bosnia and Herzegovina"] <- 15.0
percalcodeaths$value[percalcodeaths$country == "Spain"] <- 17.0
alcohollawexistance$value[alcohollawexistance$country == "Australia"] <- "Yes"
alcohollawexistance$value[alcohollawexistance$country == "Maldives"] <- NA
defbybac$value[defbybac$value == "-"] <- NA
percalcodeaths$value <- as.numeric(percalcodeaths$value)

# Adding country codes to match to polygons
percalcodeaths$countrycode <- countrycode(percalcodeaths$country, "country.name", "wb", warn = TRUE) 
alcohollawexistance$countrycode <- countrycode(alcohollawexistance$country, "country.name", "wb", warn = TRUE) 
defbybac$countrycode <- countrycode(defbybac$country, "country.name", "wb", warn = TRUE)
deaths$countrycode <- countrycode(deaths$country, "country.name", "wb", warn = TRUE)

# Initiate colous for map
colours <- colorBin("YlOrRd", percalcodeaths$value, na.color = "#808080", bins=10)

# Sort data alphabetically
percalcodeaths <- percalcodeaths[order(percalcodeaths$country),]
alcohollawexistance <- alcohollawexistance[order(alcohollawexistance$country),]
defbybac <- defbybac[order(defbybac$country),]
deaths <- deaths[order(deaths$country),]

# Link data to geojson
for (i in 1:length(alcohollawexistance$value))
{
  geojson$deaths[geojson$id == deaths$countrycode[i]] <- round((deaths$value[i]*(percalcodeaths$value[i]/100)),1)
  geojson$defbybac[geojson$id == defbybac$countrycode[i]] <- defbybac$value[i]
  geojson$perdeaths[geojson$id == percalcodeaths$countrycode[i]] <- percalcodeaths$value[i]
  geojson$law[geojson$id == alcohollawexistance$countrycode[i]] <- alcohollawexistance$value[i]
}

# Make dataframe for summary table, country names were extracted by hand (had some trouble selecting a single element...)
summarydata <- data.frame(c("Mean % of alcohol related traffic fatalities", "Max % of alcohol related traffic fatalities","Min % of alcohol related traffic fatalities"),
                          c(round(mean(geojson$perdeaths, na.rm=TRUE),1),max(geojson$perdeaths, na.rm = TRUE),min(geojson$perdeaths, na.rm = TRUE)),
                          c('None','South Africa','Oman'))
names(summarydata) <- c('Description','Value', 'Country')

# Make dataframe for law piechart
lawplot <- data.frame(c("Yes", "No", "NA"),c(
  length(geojson$law[geojson$law=="Yes"  & !is.na(geojson$law)]),
  length(geojson$law[geojson$law=="No" & !is.na(geojson$law)]),
  length(geojson$law[is.na(geojson$law)])))
names(lawplot) <- c('Label','Data')

# Make dataframe for BAC piechart
bacplot <- data.frame(c("Yes", "No", "NA"),c(
  length(geojson$defbybac[geojson$defbybac=="Yes" & !is.na(geojson$defbybac)]),
  length(geojson$defbybac[geojson$defbybac=="No" & !is.na(geojson$defbybac)]),
  length(geojson$defbybac[is.na(geojson$defbybac)])))
names(bacplot) <- c('Label','Data')

# Make the Shiny UI. Some things are set by hand to make the full page layout
ui <- bootstrapPage(
  title = "Alcohol & Traffic",
  theme = "flatly",
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                      leafletOutput("map", width = "100%", height = "100%"),
                      absolutePanel(top = 60, draggable = TRUE, left = "auto", right = 20, bottom = "auto",
                                    width = 330, height = "auto",wellPanel(h3("Alcohol & Traffic"),
"This interactive map shows information about alcohol and traffic fatalities around the world in 2013. Hover above a country on the map to see detailed information.",
                                    hr(),selectInput("select",label = h4("Summarized information:"), choices = list("% of alcohol related traffic fatalities" = 1,
"Existence of a national drunk-driving law" = 2,"Definition of drunk-driving by BAC
"=3), selected = 1),htmlOutput("plots"),hr(),"Data: WHO; Designed by Nick Ubels")))

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
                  highlightOptions = highlightOptions(color = "blue", weight = 2,
                                                      bringToFront = TRUE),
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
      addLegend(position = "bottomleft",pal = colours, values = ~perdeaths, opacity = 1.0,title = "% traffic fatalities related to alcohol", labFormat = labelFormat(suffix = "%"))
   })
}

# Lets run this beast
shinyApp(ui = ui, server = server)
