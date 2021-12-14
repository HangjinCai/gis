library(rgdal)
library(sf)
library(tmap) 
library(tmaptools)
library(RSQLite)
library(tidyverse)
#read in the shapefile
shape <- st_read(
  "CASA/modules/gis/wk1_prac/ESRI/London_Borough_Excluding_MHW.shp")
# read in the csv
mycsv <- read_csv("CASA/modules/gis/wk1_prac/fly_tipping_borough_edit.csv")  
#plot a outline of the shape rather than every variable in the shapefile
shape %>% 
  st_geometry() %>%
  plot()    #rather than just 'plot(shape)'
# merge csv and shapefile
shape <- shape%>%
  merge(.,
        mycsv,
        by.x="GSS_CODE", 
        by.y="Row Labels")
# set tmap to plot
tmap_mode("plot")
# have a look at the map
qtm(shape, fill = "2011_12")
# write to a .gpkg
shape %>%
  st_write(.,"CASA/modules/gis/wk1_prac/Rwk1.gpkg",
           "london_boroughs_fly_tipping",
           delete_layer=TRUE)
# connect to the .gpkg
con <- dbConnect(SQLite(),dbname="CASA/modules/gis/wk1_prac/Rwk1.gpkg")
# list what is in it
con %>%
  dbListTables()
# add the original .csv
con %>%
  dbWriteTable(.,
               "original_csv",
               mycsv,
               overwrite=TRUE)
# disconnect from it
con %>% 
  dbDisconnect()