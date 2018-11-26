library(shiny)
library(leaflet)
library(dplyr)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("mymap"),
  p()
)

server <- function(input, output, session) {
  
  points <- select(rawData, "Latitude", "Longitude")
  
  output$mymap <- renderLeaflet({
    leaflet(data = rawData) %>% addTiles() %>%
      addMarkers(~Longitude, ~Latitude, clusterOptions = markerClusterOptions())})}

shinyApp(ui, server)

