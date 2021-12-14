library(tidyverse)
library(here)
library(sf)
library(janitor)

report <- read_csv(here::here("task", 
                              "Report_Card_Assessment_Data_2018-19_School_Year.csv"))

shape <- st_read(here::here("task",
                            "Washington_Counties_with_Natural_Shoreline___washsh_area (1)",
                            "Washington_Counties_with_Natural_Shoreline___washsh_area.shp"))

county_only <- report %>%
  clean_names()%>%
  select(county, test_subject, percent_met_standard)%>%
  # the != means don't select this, but select everything else
  filter(county != "Multiple")%>%
  filter(test_subject == "Science") %>%
  #slice(101:120,)
  filter(percent_met_standard != "Suppressed: N<10")%>%
  filter(percent_met_standard != "No Students")%>%
  filter(str_detect(percent_met_standard, "^<", negate = T))%>%
  mutate(percent_met_standard = str_replace_all(percent_met_standard, pattern = c('%' = "")))%>%
  mutate(percent_met_standard2= as.numeric(percent_met_standard))%>%
  group_by(county)%>%
  summarise(average_met=mean(percent_met_standard2, na.rm=T))

shape

joined_data <- shape %>% 
  clean_names() %>%
  left_join(., 
            county_only,
            by = c("countylabe" = "county"))

library(tmap)
library(tmaptools)

bbox_county <- shape %>%
  st_bbox(.) %>%     #extract bonding box form your map
  tmaptools::read_osm(., type = "esri", zoom = NULL)

tm_shape(bbox_county)+
  tm_rgb()+
  
  
  tm_shape(joined_data) + 
  tm_polygons("average_met", 
              style="pretty",
              palette="Blues",
              midpoint=NA,
              #title="Number of years",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Difference in life expectancy", legend.position = c("right", "bottom"))


  