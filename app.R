library(shiny)
library(leaflet)
library(dplyr)


ui <- fluidPage(
  headerPanel(title = "KELPTIME"),
  mainPanel(leafletOutput("mymap")),
  
  sidebarPanel(
    
    selectInput("ecoregionDrop", "Select Ecoregion:",
                ecoregions["ECOREGION"]),
    
    
    actionButton("applyButton", "Apply"),
    actionButton("resetButton", "Reset"),
    
    
    sliderInput("dateFilter", "Filter by date:", 
                min = min(ssl["Start"]), max = max(ssl["Start"]), 
                value = c(min(ssl["Start"]),max(ssl["Start"])), step = 1, sep=""),
   
    checkboxInput("legendtag", "Show Legend", FALSE)
  ),
    

  plotOutput("testplot")
  
)

server <- function(input, output, session) {
  
  rawData <- read.csv("CleanDataWithRegions.csv", stringsAsFactors=FALSE)
  site_slopes <- read.csv("eco_sites.csv", stringsAsFactors=FALSE) # %>%
    #filter(Period == "1900-2015")
  
  rawDataFilt <- rawData %>%
    dplyr::mutate(SiteName =  paste(Study, Site, study_ID,trajectory_ID,sep="-")) %>%
    dplyr::mutate(SiteName = paste(SiteName, Study, sep=":")) %>%
    group_by(Study, SiteName, Latitude, Longitude, Site, ECOREGION) %>%
    dplyr::summarise(Start = min(year), End = max(year)) %>%
    ungroup() 
  
  
  site_slopes_latlong <- left_join(site_slopes, rawDataFilt)
  
  ssl <- site_slopes_latlong %>%
    filter(!is.na(Latitude)) %>%
    filter(!is.na(Longitude)) %>%
    mutate(lab = paste0(Study, ", ", Site, 
                        "<br>",Start,"-",End,
                        "<br>slope: ", round(mean,3), " ? ", round(se,3), "SE"))
  
  qpal <- colorNumeric(palette = c("red", "purple", "blue"), domain = ssl$mean, n = 11)
  
  filtered <- reactive({
    updateCheckboxInput(session, "legendtag", value = FALSE)
    ssl[ssl$Start >= input$dateFilter[1] & ssl$End <= input$dateFilter[2], ]
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles(options = providerTileOptions(minZoom = 1, maxZoom = 10)) %>%
      addCircleMarkers(
        data = filtered(), 
        lng = ~Longitude, 
        lat = ~Latitude,
        opacity=0.5,
        color = ~qpal(mean),
        popup = ~lab)
    #addLegend(position = "bottomright", pal = qpal, values = ssl$mean)
  })
  
  
  
  #Show the legend if the checkbox is ticked 
  observe({
    proxy <- leafletProxy("mymap")
    proxy %>% clearControls()
    
    
    if(input$legendtag) {
      proxy %>% addLegend(position = "bottomright", pal = qpal, values = ssl$mean)
    }
  })
  
  observeEvent(input$applyButton, {
    eco_subset <- ssl %>% filter(ECOREGION == input$ecoregionDrop & Start >= input$dateFilter[1] & End <= input$dateFilter[2])
    
    proxy <-leafletProxy("mymap")
    proxy %>% clearMarkers() 
    proxy %>% addCircleMarkers(
      data = eco_subset, 
      lng = ~Longitude, 
      lat = ~Latitude,
      opacity = 0.5,
      color = ~qpal(mean),
      popup = ~lab)
  })
  
  observeEvent(input$resetButton, {
    proxy <-leafletProxy("mymap")
    proxy %>% clearMarkers() 
    proxy %>% addCircleMarkers(
      data = filtered(), 
      lng = ~Longitude, 
      lat = ~Latitude,
      opacity = 0.5,
      color = ~qpal(mean),
      popup = ~lab)
  })
}

shinyApp(ui, server)

