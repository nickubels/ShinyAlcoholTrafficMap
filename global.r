# Made by Nick Ubels
# Shiny app global functionality. Massages the data and loads the dependencies
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

# Initiate colous for map
colours <- colorNumeric("YlOrRd", domain = geojson$perdeaths, na.color = "#808080")

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
