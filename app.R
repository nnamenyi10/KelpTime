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
    

  plotOutput("testplot", width="70%")
  
)

server <- function(input, output, session) {
  
  rawData <- read.csv("CleanDataWithRegions.csv", stringsAsFactors=FALSE)
  site_slopes <- read.csv("eco_sites.csv", stringsAsFactors=FALSE)
  
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
  

  filtered = reactive({
    updateCheckboxInput(session, "legendtag", value = FALSE) #temporary hack to resolve disappearing legend
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

  observe({
    leafletProxy("mymap") %>% clearPopups()
    click = input$mymap_marker_click
    if(is.null(click))
      return()
    
    # intentionally lose precision for map and data errors
    lat = paste(click$lat)
    long = paste(click$lng)

    cat(file=stderr(), "Site:", lat, round(click$lat, 3), ",", long, round(click$lng, 3), "\n")
    
    plotting = rawData[abs(rawData$Latitude - click$lat) < 0.001 
                       & abs(rawData$Longitude - click$lng) < 0.001, ]
    
    region = plotting$ECOREGION[[1]]
    
    plotting = select(plotting, rawDate, Stipe_Density_num_per_sq_m)
    plotting = plotting[!is.na(plotting[[2]]),]
    
    if(length(plotting[[1]]) == 0) {
     output$testplot = renderPlot(plot(0, 0, main="No relavant data points"))
    }
    else {
      line = lm(formula = select(plotting, Stipe_Density_num_per_sq_m)[[1]] ~ select(plotting, rawDate)[[1]])
      regions = rawData[rawData$ECOREGION == region, ]
      regionLine = lm(formula = select(regions, Stipe_Density_num_per_sq_m)[[1]] ~ select(regions, rawDate)[[1]])
      
      #print(regions)
      
      #print(plotting) #select(plotting, rawDate, Stipe_Density_num_per_sq_m))
      output$testplot = renderPlot({plot(select(plotting, rawDate, Stipe_Density_num_per_sq_m), 
                                         main="Stipe Density Over Time", ylab = "Stipe Density Number pre Square Meter",
                                         xlab = "Time")
                                    abline(line[[1]][[1]], line[[1]][[2]], col="red")
                                    abline(regionLine[[1]][[1]], regionLine[[1]][[2]], col="blue")
                                    legend(x="topright", legend=c("Ecoregion", "site"), col=c("blue", "red"), lty=1:1)})
      #line = lm(formula = select(plotting, Stipe_Density_num_per_sq_m)[[1]] ~ select(plotting, rawDate)[[1]])
      # output$testplot.new
      # output$testplot = abline(line)
    }
    })
}

shinyApp(ui, server)

