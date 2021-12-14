#The task is to join some non spatial data to some spatial data 
#and wrangle it.
# Q:
#calcuate the average percent of science students (in all)grades
#per county meeting the required standards and produce a map 
#to show where the Country averages are above or below the State
#of Washington average.
library(tidyverse)
library(here)
library(sf)
library(janitor)

report <- read_csv(here::here("CASA",
                              "modules",
                              "gis",
                              "CASA0005repo",
                              "wk2",
                              "wk2_HW",
                              "Report_Card_Assessment_Data_2018-19_School_Year.csv"))

shape <- st_read(here::here("CASA",
                            "modules",
                            "gis",
                            "CASA0005repo",
                            "wk2",
                            "wk2_HW",
                            "Washington_Counties_with_Natural_Shoreline___washsh_area",
                            "Washington_Counties_with_Natural_Shoreline___washsh_area.shp"))

#remove everything we don't want from .csv
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
  #above one saying that replacing the column of"percent_met_standard" ,% to non.
  mutate(percent_met_standard2= as.numeric(percent_met_standard))%>%
  group_by(county)%>%
  #as.numeric() is a function to make something numeric.
  summarise(average_met=mean(percent_met_standard2, na.rm=T))


#left join(left part is .shp,right part is wranged .csv)
joined_data <- shape %>% 
  clean_names() %>%
  left_join(.,    
            county_only,
            county_to_state_difference,
            by = c("countylabe" = "county"))
#"countylabe" from .shp, "county" from .csv


#mapping
library(tmap)
library(tmaptools)
#we need bonding box first
bbox_county <- shape %>%
  st_bbox(.) %>%     #extract bonding box form .shp
  tmaptools::read_osm(., type = "esri", zoom = NULL)

# merge
tm_shape(bbox_county)+
  tm_rgb()+
  
  tm_shape(joined_data) + 
  tm_polygons("average_met", 
              style="pretty",
              palette="Blues",
              midpoint=NA,
              #title="Number of years", #out because we don't ahve title here
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Difference in life expectancy", legend.position = c("right", "bottom"))

#this task Andy didn't take the average of the State of Washington(below or above)
