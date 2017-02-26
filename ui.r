# Made by Nick Ubels
# Shiny app UI
# Make the Shiny UI. Some things are set by hand to make the full page layout
ui <- bootstrapPage(
  title = "Alcohol & Traffic",
  theme = "flatly",
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 60,
                draggable = TRUE,
                left = "auto",
                right = 20,
                bottom = "auto",
                width = 330,
                height = "auto",
                wellPanel(h3("Alcohol & Traffic"),
                "This interactive map shows information about alcohol and traffic fatalities around the world in 2013. Hover above a country on the map to see detailed information.",
                hr(),
                selectInput("select",label = h4("Summarized information:"), choices = list("% of alcohol related traffic fatalities" = 1,"Existence of a national drunk-driving law" = 2,"Definition of drunk-driving by BAC"=3), selected = 1),
                htmlOutput("plots"),
                hr(),
                "Data: WHO; Designed by Nick Ubels"
                )
                )
  )