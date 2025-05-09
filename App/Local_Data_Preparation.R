library(tidyverse)
library(sf)
library(googledrive)

setwd("/home/kyle/Documents/Dog_WalkR/Dog_WalkR_App/")

source("https://raw.githubusercontent.com/kyledougherty/CougR/refs/heads/main/a_LoCoH_HR_Function.R")

# Download Data -----------------------------------------------------------
drive_ls("Dog WalkR/Walks") %>%
  mutate(File_Exists = file.exists(paste0("Data/Walks/", name))) %>%
  filter(File_Exists == FALSE) %>%
  pull(id) %>%
  map(~ drive_download(as_id(.), path = paste0("Data/Walks/",
                                               drive_get(as_id(.))$name)))

drive_ls("Dog WalkR/PeePees") %>%
  mutate(File_Exists = file.exists(paste0("Data/PeePees/", name))) %>%
  filter(File_Exists == FALSE) %>%
  pull(id) %>%
  map(~ drive_download(as_id(.), path = paste0("Data/PeePees/",
                                               drive_get(as_id(.))$name)))

drive_ls("Dog WalkR/PooPoos") %>%
  mutate(File_Exists = file.exists(paste0("Data/PooPoos/", name))) %>%
  filter(File_Exists == FALSE) %>%
  pull(id) %>%
  map(~ drive_download(as_id(.), path = paste0("Data/PooPoos/",
                                               drive_get(as_id(.))$name)))

# Import Data -------------------------------------------------------------
Home <-  st_sf(st_sfc(st_point(c(-96.63196334055851, 40.73022624882763)), 
                      crs = 4326)) 


Walk_Location_Data <- map_df(list.files("Data/Walks", full.names = TRUE),
                             ~read_csv(.x, col_types = cols(.default = col_character())) %>%
                               mutate(Date_Time = ymd_hms(Date_Time, tz = "America/Chicago"),
                                      Walk = date(Date_Time), 
                                      Lat = as.numeric(Lat), 
                                      Long = as.numeric(Long))) %>%
  mutate(Date_Time_Rounded = round_date(Date_Time, "15 seconds")) %>%
  group_by(Walk, Date_Time_Rounded) %>%
  filter(abs(difftime(Date_Time, Date_Time_Rounded)) ==
           min(abs(difftime(Date_Time, Date_Time_Rounded)))) %>%
  filter(row_number() == 1) %>%
  st_as_sf(coords = c("Long", "Lat"),
           crs = 4326,
           remove = FALSE) %>%
  ungroup() %>%
  select(Walk, Date_Time = Date_Time_Rounded, Long, Lat, Type, geometry)

PeePees <- map_df(list.files("Data/PeePees/", full.names = TRUE),
                  ~read_csv(.x, col_types = cols(.default = col_character())) %>%
                    mutate(Date_Time = ymd_hms(Date_Time, tz = "America/Chicago"),
                           Walk = date(Date_Time), 
                           Lat = as.numeric(Lat), 
                           Long = as.numeric(Long))) %>%
  filter(!is.na(Date_Time)) %>%
  st_as_sf(coords = c("Long", "Lat"),
           crs = 4326,
           remove = FALSE) %>%
  left_join(Walk_Location_Data %>% 
              st_drop_geometry() %>% 
              group_by(Walk) %>% 
              summarise(Walk_Start = min(Date_Time))) %>% 
  mutate(Distance_From_Home = as.numeric(st_distance(., Home)), 
         Time_From_Start = as.numeric(difftime(Date_Time, 
                                               Walk_Start, 
                                               units = "mins"))) %>% 
  select(Walk, Date_Time, Long, Lat, Type, Distance_From_Home, Time_From_Start, geometry)

PooPoos <- map_df(list.files("Data/PooPoos/", full.names = TRUE),
                  ~read_csv(.x, col_types = cols(.default = col_character())) %>%
                    mutate(Date_Time = ymd_hms(Date_Time, tz = "America/Chicago"),
                           Walk = date(Date_Time), 
                           Lat = as.numeric(Lat), 
                           Long = as.numeric(Long))) %>%
  filter(!is.na(Date_Time)) %>%
  st_as_sf(coords = c("Long", "Lat"),
           crs = 4326,
           remove = FALSE) %>%
  select(Walk, Date_Time, Long, Lat, Type, geometry)

# Basic Summary Statistics ------------------------------------------------
Walk_Summaries <- Walk_Location_Data %>%
  group_by(Walk) %>%
  summarise(Walk_Time = round(as.numeric(difftime(max(Date_Time),
                                                  min(Date_Time), 
                                                  units = "hours")), 1),
            geometry = st_combine(geometry)) %>%
  st_cast("LINESTRING") %>%
  mutate(Walk_Distance_Miles = round(as.numeric(st_length(.))/1000 * 0.6214, 2))

first_walk <- format(min(date(Walk_Location_Data$Date_Time)), "%B %d, %Y")

number_of_walks <- nrow(Walk_Summaries)
number_of_poopoos <- nrow(PooPoos)
number_of_peepees <- nrow(PeePees)

total_distance <- sum(Walk_Summaries$Walk_Distance_Miles)
total_time <- sum(Walk_Summaries$Walk_Time)

# Home Range --------------------------------------------------------------
Home_Range <- a_LoCoH_HR(Walk_Location_Data %>%
                           mutate(DATE = date(Date_Time),
                                  CRS = 32614) %>%
                           st_transform(32614),
                         min_days = 0,
                         iso_levels = c(0.95))

home_range_area = round(as.numeric(Home_Range$Area)/2.59e6, 2)
red_fox_home_range_area = round(2.9/2.59, 2)
mountain_lion_home_range_area = round(470/2.59, 2)

red_fox_relative_home_range_size = as.character(round(home_range_area/red_fox_home_range_area*100))
mountain_lion_relative_home_range_size = as.character(round(home_range_area/mountain_lion_home_range_area, 3)*100)


# PeePee ------------------------------------------------------------------
Pee_Pee_Home_Range <- a_LoCoH_HR(PeePees %>%
                                   mutate(DATE = date(Date_Time),
                                          CRS = 32614) %>%
                                   st_transform(32614),
                                 min_days = 0,
                                 iso_levels = c(0.95))

peepee_time_from_start = ggplot(data = PeePees, 
                                aes(x = Time_From_Start)) + 
  geom_density(fill = "red",
               alpha = 0.25,
               color = "red") + 
  labs(x = "Time From Start of Walk (Minutes)", 
       y = "Frequency of Peeing") + 
  theme_classic() + 
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 14, 
                                   face = "bold"),
        axis.title = element_text(size = 16, 
                                  face = "bold"))

peepee_distance_from_home = ggplot(data = PeePees, 
                                   aes(x = Distance_From_Home)) + 
  geom_density(fill = "blue",
               alpha = 0.25,
               color = "blue") + 
  labs(x = "Distance From Home (Meters)", 
       y = "Frequency of Peeing") + 
  theme_classic() + 
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 14, 
                                   face = "bold"),
        axis.title = element_text(size = 16, 
                                  face = "bold"))

save.image("App_Data.RData")
