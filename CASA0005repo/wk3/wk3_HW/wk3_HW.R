#Task:
#to manipulate some raster data and produce some 
#descriptivec statistics.

#Questions:
#For any country in the World produce descriptive statistics
#that show the difference in maximum annual temperature for 
#key cities between SSP1 and SSP5 for the years 2081-2100,
#using any model and resolution.
#our data is monthly rather than annually here(don't misunderstand it)

#the city we choose is Spain
library(sf)
library(here)
library(janitor)
library(tidyverse)
library(raster)
library(ggplot2)
#to see the layers of a Geopkg:
st_layers(here("CASA", "modules", "gis", "CASA0005repo", "wk3", "wk3_HW", "gadm36_ESP_gpkg", "gadm36_ESP.gpkg"))

#To read our data:
#read geopkg
spain <- st_read(here("CASA", "modules", "gis", "CASA0005repo", "wk3", "wk3_HW", "gadm36_ESP_gpkg", "gadm36_ESP.gpkg"), 
                 layer='gadm36_ESP_0')

#read .shp
world_cities <- st_read(here("CASA", "modules", "gis", "CASA0005repo", "wk3", "wk3_HW", "World_Cities", "World_Cities.shp"))

#just want to get the Spain .shp out from world_cities:
spanish_cities <- world_cities %>%
  clean_names()%>%
  filter(cntry_name=="Spain")

#Read raster datas

#if you read in 'raster(here())',type 'ssp1'(don't run this)
## ssp1 <-raster(here("CASA", "modules", "gis", "CASA0005repo", "wk3", "wk3_HW", "WorldClim future projections", "wc2.1_2.5m_tmax_BCC-CSM2-MR_ssp126_2081-2100.tif"))

## ssp5 <-raster(here("CASA", "modules", "gis", "CASA0005repo", "wk3", "wk3_HW", "WorldClim future projections", "wc2.1_2.5m_tmax_BCC-CSM2-MR_ssp585_2081-2100.tif"))
## ssp1
#it shows:  band: 1  (of  12  bands)
#this telling you that the source had four bands but
#irs only read one,so use 'stack' instead:
ssp1 <-stack(here("CASA", "modules", "gis", "CASA0005repo", "wk3", "wk3_HW", "WorldClim future projections", "wc2.1_2.5m_tmax_BCC-CSM2-MR_ssp126_2081-2100.tif"))

ssp5 <-stack(here("CASA", "modules", "gis", "CASA0005repo", "wk3", "wk3_HW", "WorldClim future projections", "wc2.1_2.5m_tmax_BCC-CSM2-MR_ssp585_2081-2100.tif"))

#now we have all 12 bands.
#(if read in brick:change 'stack'to'brick',that would do)
# 'brick' is similar to a RasterStack (that can be 
#created with stack), but shorter processing time with 
#less flexibility as they can only point to a single file.


#create a differencing(subtract them)
ssp_diff <- ssp5-ssp1

####ssp1
spain_diff <- ssp1 %>%
  # now crop our temp data to the extent
  crop(.,spain)

exact_spain <- spain_diff %>%
  mask(.,spain, na.rm=TRUE)

###ssp5
spain_diff5 <- ssp5 %>%
  # now crop our temp data to the extent
  crop(.,spain)

exact_spain5 <- spain_diff5 %>%
  mask(.,spain, na.rm=TRUE)


# subtract
diff_climate_model <- exact_spain5 - exact_spain 

month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(diff_climate_model) <- month

spain_city_diff<- raster::extract(diff_climate_model, spanish_cities)


plot(diff_climate_model)






#check the CRS
spain
