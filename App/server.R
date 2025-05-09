poop_icon <- makeIcon(
  iconUrl = "images/Poop_Emoji.png",
  iconWidth = 16, iconHeight = 16
)

server <- function(input, output) {
  
  output$walk_location_map <- renderLeaflet({
    leaflet() %>% 
      leaflet::addCircleMarkers(lng = Walk_Location_Data$Long, 
                                lat = Walk_Location_Data$Lat, 
                                radius = 1, 
                                opacity = 0.25) %>% 
      addProviderTiles(providers$Esri.WorldImagery)
    
  })
  
  output$home_range_map <- renderLeaflet({
    leaflet() %>% 
      leaflet::addPolygons(data = Home_Range)%>% 
      addProviderTiles(providers$Esri.WorldImagery)
    
  })
  
  output$peepee_map <- renderLeaflet({
    leaflet() %>% 
      leaflet::addPolygons(data = Pee_Pee_Home_Range) %>% 
      leaflet::addCircleMarkers(lng = PeePees$Long, 
                                lat = PeePees$Lat, 
                                radius = 1, 
                                opacity = 0.75, 
                                color = "gold") %>% 
      leaflet::addMarkers(lng = PooPoos$Long, 
                                lat = PooPoos$Lat,
                                icon = poop_icon) %>% 
      
      addProviderTiles(providers$Esri.WorldImagery)
    
  })
  
  output$peepee_distance_from_home <- renderPlot({
    peepee_distance_from_home
  })
  output$peepee_time_from_start <- renderPlot({
    peepee_time_from_start
  })
  
}
