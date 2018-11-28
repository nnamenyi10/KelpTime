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
  
  rawData <- read.csv("CleanData.csv", stringsAsFactors=FALSE)
  site_slopes <- read.csv("eco_sites.csv", stringsAsFactors=FALSE) %>%
    filter(Period == "1900-2015")
  
  rawDataFilt <- rawData %>%
    dplyr::mutate(SiteName =  paste(Study, Site, study_ID,trajectory_ID,sep="-")) %>%
    dplyr::mutate(SiteName = paste(SiteName, Study, sep=":")) %>%
    group_by(Study, SiteName, Latitude, Longitude, Site) %>%
    dplyr::summarise(Start = min(year), End = max(year)) %>%
    ungroup() 
  
  
  site_slopes_latlong <- left_join(site_slopes, rawDataFilt)
  sum(is.na(site_slopes_latlong$Latitude))
  
  
  ssl <- site_slopes_latlong %>%
    filter(!is.na(Latitude)) %>%
    filter(!is.na(Longitude)) %>%
    mutate(lab = paste0(Study, ", ", Site, 
                        "<br>",Start,"-",End,
                        "<br>slope: ", round(mean,3), " ? ", round(se,3), "SE"))
  
  qpal <- colorQuantile("RdYlBu", ssl$mean, n = 20)
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      addCircleMarkers(
        data = ssl, 
        lng = ~Longitude, 
        lat = ~Latitude,
        opacity=0.5,
        color = ~qpal(mean),
        popup = ~lab) %>%
      addLegend(, position = c("bottomright"), pal = qpal, values = ssl$mean) 
    })}

shinyApp(ui, server)

