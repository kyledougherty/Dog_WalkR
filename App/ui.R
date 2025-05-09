ui = shiny::htmlTemplate(
  # Index Page
  "www/index.html",
  walk_location_map = leafletOutput(outputId = "walk_location_map", 
                                    height = 600), 
  home_range_map = leafletOutput(outputId = "home_range_map", 
                                 height = 600),
  peepee_map = leafletOutput(outputId = "peepee_map", 
                             height = 600),
  peepee_distance_from_home = plotOutput("peepee_distance_from_home"),
  peepee_time_from_start = plotOutput("peepee_time_from_start")
  
  
)